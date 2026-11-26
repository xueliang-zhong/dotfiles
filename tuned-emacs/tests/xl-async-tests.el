;;; xl-async-tests.el --- tests for the composable xl-async primitives -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L tuned-emacs/lisp -L tuned-emacs/tests \
;;        -l tuned-emacs/tests/xl-async-tests.el -f ert-run-tests-batch-and-exit

;; These tests cover layers 0 and 1 only.  The compatibility layer
;; (`xl-async-run', `xl-async-enqueue' and friends) is still covered by
;; smoke-tests.el.

(require 'ert)
(require 'cl-lib)
(require 'xl-async)

(defun xl-async-test--settled-p (proc)
  "Return non-nil once PROC has reached a final status."
  (memq (xl-async-status proc) '(done timeout cancelled)))

(defun xl-async-test--wait (predicate &optional seconds)
  "Wait until PREDICATE returns non-nil, at most SECONDS (default 30)."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defun xl-async-test--wait-settled (proc)
  "Wait until PROC has finished or the deadline passes."
  (xl-async-test--wait (lambda () (xl-async-test--settled-p proc))))

(defun xl-async-test--late-p (predicate)
  "Wait for one event-loop turn, then return PREDICATE's value."
  (accept-process-output nil 0.05)
  (funcall predicate))

;;; Layer 0: core

(ert-deftest xl-async-on-exit-runs-exactly-once ()
  "The sentinel and the reaper both finish the task; on-exit runs once."
  (let ((proc (xl-async-start '("sh" "-c" "exit 0")))
        (calls 0))
    (xl-async-on-exit proc (lambda (_p) (cl-incf calls)))
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'done))
    ;; Whatever fires first, the other must be a no-op.
    (xl-async--exit-sentinel proc "finished\n")
    (xl-async--reap-task proc)
    (should (xl-async-test--late-p (lambda () (= calls 1))))
    (should (= calls 1))))

(ert-deftest xl-async-abort-skips-on-exit-and-runs-finally-once ()
  "An aborted task is cancelled: no on-exit, exactly one finally."
  (let ((proc (xl-async-start '("sleep" "5")))
        (exits 0)
        (finals 0))
    (xl-async-on-exit proc (lambda (_p) (cl-incf exits)))
    (xl-async-finally proc (lambda (_p) (cl-incf finals)))
    (xl-async-abort proc)
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'cancelled))
    (xl-async-abort proc)
    (should (xl-async-test--late-p (lambda () (= finals 1))))
    (should (= exits 0))
    (should (= finals 1))
    ;; A late on-exit never runs for a cancelled task; a late finally does.
    (let ((late-exit 0)
          (late-finally 0))
      (xl-async-on-exit proc (lambda (_p) (cl-incf late-exit)))
      (xl-async-finally proc (lambda (_p) (cl-incf late-finally)))
      (should (xl-async-test--wait (lambda () (= late-finally 1))))
      (should (= late-exit 0)))))

(ert-deftest xl-async-large-output-is-complete ()
  "More than a megabyte arrives whole, before on-exit and afterwards."
  (let* ((size 2000000)
         (proc (xl-async-start
                (list "sh" "-c" (format "yes a | head -c %d" size))))
         (seen nil))
    (xl-async-on-exit
     proc (lambda (p) (setq seen (length (xl-async-output p)))))
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'done))
    (should (= seen size))
    (should (= (length (xl-async-output proc)) size))
    (should (string= "a\na\na" (substring (xl-async-output proc) 0 5)))))

(ert-deftest xl-async-late-registration-runs ()
  "on-exit and finally registered after the end still run."
  (let ((proc (xl-async-start '("true"))))
    (should (xl-async-test--wait-settled proc))
    (let ((exits 0)
          (finals 0))
      (xl-async-on-exit proc (lambda (_p) (cl-incf exits)))
      (xl-async-finally proc (lambda (_p) (cl-incf finals)))
      (should (xl-async-test--wait (lambda () (and (= exits 1)
                                                   (= finals 1)))))
      (should (= exits 1))
      (should (= finals 1)))))

(ert-deftest xl-async-one-failing-handler-does-not-stop-the-next ()
  "A handler that signals is reported; the handlers after it still run."
  (let ((proc (xl-async-start '("true")))
        (second nil))
    (xl-async-on-exit proc (lambda (_p) (error "boom")))
    (xl-async-on-exit proc (lambda (_p) (setq second t)))
    (let ((inhibit-message t))
      (should (xl-async-test--wait (lambda () second))))
    (should second)))

(ert-deftest xl-async-stdin-is-closed-once-written ()
  "Writing :stdin and closing it lets a command that reads stdin finish."
  (let ((proc (xl-async-start '("cat") :stdin "hello\n")))
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'done))
    (should (equal (xl-async-output proc) "hello\n"))))

(ert-deftest xl-async-named-buffer-is-kept ()
  "A :buffer-name buffer with the pid survives the end."
  (let ((proc (xl-async-start '("sh" "-c" "printf x")
                              :buffer-name "*xl-async-test %d*")))
    (should (xl-async-test--wait-settled proc))
    (let ((buffer (xl-async-buffer proc)))
      (unwind-protect
          (progn
            (should buffer)
            (should (string-match-p "\\`\\*xl-async-test [0-9]+\\*\\'"
                                    (buffer-name buffer)))
            (should (equal (with-current-buffer buffer (buffer-string)) "x")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; Layer 1: timeout

(ert-deftest xl-async-timeout-sets-status-and-code ()
  "A timed-out task has status `timeout', code 124 and a cancelled timer."
  (let ((proc (xl-async-start '("sleep" "5")))
        (exits 0))
    (xl-async-on-exit proc (lambda (_p) (cl-incf exits)))
    (xl-async-timeout proc 0.2)
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'timeout))
    (should (= (xl-async-exit-code proc) 124))
    (should (= exits 1))
    (should-not (process-get proc 'xl-async-timeout-timer))))

;;; Layer 1: ownership

(ert-deftest xl-async-own-runs-handlers-in-the-owner ()
  "on-output and on-exit run with the owner buffer current."
  (let* ((owner (generate-new-buffer " *xl-async-owner*"))
         (proc (xl-async-start '("sh" "-c" "printf hi")))
         (output-buffers nil)
         (exit-buffer nil)
         (seen nil))
    (unwind-protect
        (progn
          (xl-async-own proc owner)
          (should (memq proc (buffer-local-value 'xl-async--owned owner)))
          (xl-async-on-output
           proc
           (lambda (_p chunk)
             (push (current-buffer) output-buffers)
             (setq seen (concat seen chunk))))
          (xl-async-on-exit
           proc
           (lambda (_p) (setq exit-buffer (current-buffer))))
          (should (xl-async-test--wait-settled proc))
          (should (eq exit-buffer owner))
          (should (seq-every-p (lambda (b) (eq b owner)) output-buffers))
          (should (equal seen "hi"))
          (should (equal (xl-async-output proc) "hi"))
          (should-not (memq proc (buffer-local-value 'xl-async--owned owner))))
      (kill-buffer owner))))

(ert-deftest xl-async-killing-the-owner-aborts-its-tasks ()
  "Killing the owner aborts the task: no on-exit, one finally."
  (let* ((owner (generate-new-buffer " *xl-async-owner*"))
         (proc (xl-async-start '("sleep" "5")))
         (exits 0)
         (finals 0))
    (xl-async-own proc owner)
    (xl-async-on-exit proc (lambda (_p) (cl-incf exits)))
    (xl-async-finally proc (lambda (_p) (cl-incf finals)))
    (kill-buffer owner)
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'cancelled))
    (should (xl-async-test--late-p (lambda () (= finals 1))))
    (should (= exits 0))
    (should (= finals 1))))

;;; Layer 1: slots

(ert-deftest xl-async-slot-replaces-and-leaves-the-new-entry ()
  "A newer task aborts the older one; the older finally keeps the new entry."
  (let* ((owner (generate-new-buffer " *xl-async-slot*"))
         (first (xl-async-start '("sleep" "5")))
         (first-finals 0))
    (unwind-protect
        (progn
          (xl-async-own first owner)
          (xl-async-slot first 'job)
          (xl-async-finally first (lambda (_p) (cl-incf first-finals)))
          (should (eq (xl-async-slot-process 'job owner) first))
          (let ((second (xl-async-start '("true"))))
            (xl-async-own second owner)
            (xl-async-slot second 'job)
            (should (xl-async-test--wait (lambda () (= first-finals 1))))
            (should (eq (xl-async-status first) 'cancelled))
            (should (eq (xl-async-slot-process 'job owner) second))
            (should (xl-async-test--wait
                     (lambda () (not (xl-async-live-p second)))))
            (should (xl-async-test--wait
                     (lambda () (null (xl-async-slot-process 'job owner)))))
            (should (= first-finals 1))))
      (kill-buffer owner))))

;;; Layer 1: queues

(ert-deftest xl-async-queue-runs-jobs-in-order ()
  "Jobs of one key run strictly one after another."
  (let* ((key (cons (current-buffer) 'order))
         (log nil))
    (dotimes (i 3)
      (let ((text (substring "abc" i (1+ i))))
        (xl-async-queue-push
         key
         (xl-async-job
          (lambda ()
            (xl-async-on-exit
             (xl-async-start
              (list "sh" "-c" (concat "sleep 0.05; printf " text)))
             (lambda (p) (push (xl-async-output p) log))))))))
    (should (xl-async-queue-busy-p key))
    (should (= (xl-async-queue-size key) 2))
    (should (xl-async-test--wait
             (lambda () (not (xl-async-queue-busy-p key)))))
    (should (equal (nreverse log) '("a" "b" "c")))
    (should (= (xl-async-queue-size key) 0))))

(ert-deftest xl-async-attempts-tries-until-one-succeeds ()
  "Attempts fall through on failure and stop after the first success."
  (let* ((key (cons (current-buffer) 'attempts))
         (tried nil)
         (done-output nil))
    (xl-async-queue-push
     key
     (xl-async-attempts
      (list (lambda ()
              (push 'one tried)
              (xl-async-start '("false")))
            (lambda ()
              (push 'two tried)
              (xl-async-start '("sh" "-c" "printf ok; exit 0")))
            (lambda ()
              (push 'three tried)
              (xl-async-start '("true"))))
      (lambda (p) (setq done-output (xl-async-output p)))))
    (should (xl-async-test--wait
             (lambda () (not (xl-async-queue-busy-p key)))))
    (should (equal (nreverse tried) '(one two)))
    (should (equal done-output "ok"))
    (should (= (xl-async-queue-size key) 0))))

(ert-deftest xl-async-queue-calls-on-drain-once ()
  "ON-DRAIN runs once when the queue empties, and a new one replaces it."
  (let* ((key (cons (current-buffer) 'drain))
         (drains 0))
    (xl-async-queue-push
     key
     (xl-async-job (lambda () (xl-async-start '("true"))))
     (lambda () (cl-incf drains)))
    (should (xl-async-test--wait
             (lambda () (not (xl-async-queue-busy-p key)))))
    (should (xl-async-test--late-p (lambda () (= drains 1))))
    (should (= drains 1))
    (xl-async-queue-push
     key
     (xl-async-job (lambda () (xl-async-start '("true"))))
     (lambda () (cl-incf drains)))
    (should (xl-async-test--wait
             (lambda () (not (xl-async-queue-busy-p key)))))
    (should (xl-async-test--late-p (lambda () (= drains 2))))
    (should (= drains 2))))

(ert-deftest xl-async-queue-clear-aborts-the-running-job ()
  "Clearing a queue drops its waiting jobs and aborts the running one."
  (let* ((key (cons (current-buffer) 'clear))
         (proc nil)
         (finals 0))
    (xl-async-queue-push
     key
     (xl-async-job (lambda () (setq proc (xl-async-start '("sleep" "5"))))))
    (xl-async-finally proc (lambda (_p) (cl-incf finals)))
    (should (xl-async-queue-busy-p key))
    (xl-async-queue-clear key)
    (should-not (xl-async-queue-busy-p key))
    (should (xl-async-test--wait-settled proc))
    (should (eq (xl-async-status proc) 'cancelled))
    (should (xl-async-test--late-p (lambda () (= finals 1))))
    (should (= finals 1))))

;;; Compatibility layer

(ert-deftest xl-async-run-calls-on-done-in-the-owner ()
  "ON-DONE gets the output and code, with the owner buffer current."
  (with-temp-buffer
    (let ((owner (current-buffer))
          result)
      (xl-async-run :command '("sh" "-c" "printf hi; exit 3")
                    :on-done (lambda (output code)
                               (setq result (list output code
                                                  (current-buffer)))))
      (should (xl-async-busy-p))
      (should (xl-async-test--wait (lambda () result)))
      (should (equal result (list "hi" 3 owner)))
      (should-not (xl-async-busy-p)))))

(ert-deftest xl-async-run-cancel-skips-on-done ()
  "Cancelling a run kills it without calling ON-DONE."
  (with-temp-buffer
    (let (calls proc)
      (setq proc (xl-async-run :command '("sleep" "5")
                               :slot 'compat
                               :on-done (lambda (&rest _) (push 'done calls))))
      (should (xl-async-busy-p 'compat))
      (xl-async-cancel 'compat)
      (should (xl-async-test--wait
               (lambda () (not (xl-async-live-p proc)))))
      (should (xl-async-test--late-p (lambda () (null calls))))
      (should-not calls)
      (should-not (xl-async-busy-p 'compat)))))

(ert-deftest xl-async-run-unowned-calls-on-done-and-ignores-slot ()
  "With :owner nil, ON-DONE runs and :slot is ignored."
  (with-temp-buffer
    (let (result)
      (xl-async-run :command '("echo" "done") :owner nil :slot 'ignored
                    :on-done (lambda (output _code) (setq result output)))
      (should-not (xl-async-busy-p 'ignored))
      (should (xl-async-test--wait (lambda () result)))
      (should (equal result "done\n"))
      (should-not (xl-async-slot-process 'ignored (current-buffer))))))

(ert-deftest xl-async-run-streams-output-into-on-output-and-on-done ()
  "Every :on-output chunk is part of the OUTPUT ON-DONE receives."
  (with-temp-buffer
    (let (chunks result)
      (xl-async-run
       :command '("sh" "-c" "printf one; sleep 0.05; printf two")
       :on-output (lambda (chunk) (push chunk chunks))
       :on-done (lambda (output _code) (setq result output)))
      (should (xl-async-test--wait (lambda () result)))
      (should chunks)
      (should (equal (apply #'concat (nreverse chunks)) result))
      (should (equal result "onetwo")))))

(ert-deftest xl-async-run-timeout-reports-the-timeout-code ()
  "A timed-out compatibility run gives ON-DONE `xl-async-timeout-code'."
  (with-temp-buffer
    (let (code)
      (xl-async-run :command '("sleep" "5") :timeout 0.2
                    :on-done (lambda (_output exit) (setq code exit)))
      (should (xl-async-test--wait (lambda () code)))
      (should (= code xl-async-timeout-code)))))

(ert-deftest xl-async-run-keeps-the-process-name ()
  "The :name reaches the process, so errors and traces can name it."
  (let ((proc (xl-async-run :command '("true") :name "compat-name")))
    (should (equal (process-name proc) "compat-name"))
    (should (xl-async-test--wait
             (lambda () (not (xl-async-live-p proc)))))))

(ert-deftest xl-async-clear-queue-drops-waiting-and-running ()
  "The compatibility clear drops waiting jobs and aborts the running one."
  (with-temp-buffer
    (xl-async-enqueue 'q (list :attempts '(("sleep" "5"))))
    (xl-async-enqueue 'q (list :attempts '(("true"))))
    (should (= (xl-async-queue-length 'q) 1))
    (xl-async-clear-queue 'q)
    (should (= (xl-async-queue-length 'q) 0))
    (should-not (xl-async-queue-busy-p 'q))))

(ert-deftest xl-async-killing-a-buffer-clears-its-queue ()
  "Killing the owner drops its queued jobs and aborts the running one."
  (let* ((buffer (generate-new-buffer " *xl-async-queue-owner*"))
         proc)
    (with-current-buffer buffer
      (xl-async-enqueue 'q (list :attempts '(("sleep" "5"))))
      (setq proc (xl-async-slot-process 'q buffer)))
    (should (xl-async-queue-busy-p 'q buffer))
    (kill-buffer buffer)
    (should-not (xl-async-queue-busy-p 'q buffer))
    (should (xl-async-test--wait
             (lambda () (not (xl-async-live-p proc)))))))

(provide 'xl-async-tests)
;;; xl-async-tests.el ends here
