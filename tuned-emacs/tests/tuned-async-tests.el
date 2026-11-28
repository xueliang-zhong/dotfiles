;;; tuned-async-tests.el --- tests for the composable tuned-async primitives -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L tuned-emacs/lisp -L tuned-emacs/tests \
;;        -l tuned-emacs/tests/tuned-async-tests.el -f ert-run-tests-batch-and-exit

;; These tests cover layers 0 and 1 only.  The compatibility layer
;; (`tuned-async-run', `tuned-async-enqueue' and friends) is still covered by
;; smoke-tests.el.

(require 'ert)
(require 'cl-lib)
(require 'tuned-async)

(defun tuned-async-test--settled-p (proc)
  "Return non-nil once PROC has reached a final status."
  (memq (tuned-async-status proc) '(done timeout cancelled)))

(defun tuned-async-test--wait (predicate &optional seconds)
  "Wait until PREDICATE returns non-nil, at most SECONDS (default 30)."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defun tuned-async-test--wait-settled (proc)
  "Wait until PROC has finished or the deadline passes."
  (tuned-async-test--wait (lambda () (tuned-async-test--settled-p proc))))

(defun tuned-async-test--late-p (predicate)
  "Wait for one event-loop turn, then return PREDICATE's value."
  (accept-process-output nil 0.05)
  (funcall predicate))

;;; Layer 0: core

(ert-deftest tuned-async-does-not-load-the-ui-layer ()
  "The process layer does not depend on tuned-spinner; callers require both."
  (should-not (featurep 'tuned-spinner)))

(ert-deftest tuned-async-on-exit-runs-exactly-once ()
  "The sentinel and the reaper both finish the task; on-exit runs once."
  (let ((proc (tuned-async-start '("sh" "-c" "exit 0")))
        (calls 0))
    (tuned-async-on-exit proc (lambda (_p) (cl-incf calls)))
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'done))
    ;; Whatever fires first, the other must be a no-op.
    (tuned-async--exit-sentinel proc "finished\n")
    (tuned-async--reap-task proc)
    (should (tuned-async-test--late-p (lambda () (= calls 1))))
    (should (= calls 1))))

(ert-deftest tuned-async-abort-skips-on-exit-and-runs-finally-once ()
  "An aborted task is cancelled: no on-exit, exactly one finally."
  (let ((proc (tuned-async-start '("sleep" "5")))
        (exits 0)
        (finals 0))
    (tuned-async-on-exit proc (lambda (_p) (cl-incf exits)))
    (tuned-async-finally proc (lambda (_p) (cl-incf finals)))
    (tuned-async-abort proc)
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'cancelled))
    (tuned-async-abort proc)
    (should (tuned-async-test--late-p (lambda () (= finals 1))))
    (should (= exits 0))
    (should (= finals 1))
    ;; A late on-exit never runs for a cancelled task; a late finally does.
    (let ((late-exit 0)
          (late-finally 0))
      (tuned-async-on-exit proc (lambda (_p) (cl-incf late-exit)))
      (tuned-async-finally proc (lambda (_p) (cl-incf late-finally)))
      (should (tuned-async-test--wait (lambda () (= late-finally 1))))
      (should (= late-exit 0)))))

(ert-deftest tuned-async-large-output-is-complete ()
  "More than a megabyte arrives whole, before on-exit and afterwards."
  (let* ((size 2000000)
         (proc (tuned-async-start
                (list "sh" "-c" (format "yes a | head -c %d" size))))
         (seen nil))
    (tuned-async-on-exit
     proc (lambda (p) (setq seen (length (tuned-async-output p)))))
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'done))
    (should (= seen size))
    (should (= (length (tuned-async-output proc)) size))
    (should (string= "a\na\na" (substring (tuned-async-output proc) 0 5)))))

(ert-deftest tuned-async-late-registration-runs ()
  "on-exit and finally registered after the end still run."
  (let ((proc (tuned-async-start '("true"))))
    (should (tuned-async-test--wait-settled proc))
    (let ((exits 0)
          (finals 0))
      (tuned-async-on-exit proc (lambda (_p) (cl-incf exits)))
      (tuned-async-finally proc (lambda (_p) (cl-incf finals)))
      (should (tuned-async-test--wait (lambda () (and (= exits 1)
                                                   (= finals 1)))))
      (should (= exits 1))
      (should (= finals 1)))))

(ert-deftest tuned-async-one-failing-handler-does-not-stop-the-next ()
  "A handler that signals is reported; the handlers after it still run."
  (let ((proc (tuned-async-start '("true")))
        (second nil))
    (tuned-async-on-exit proc (lambda (_p) (error "boom")))
    (tuned-async-on-exit proc (lambda (_p) (setq second t)))
    (let ((inhibit-message t))
      (should (tuned-async-test--wait (lambda () second))))
    (should second)))

(ert-deftest tuned-async-stdin-is-closed-once-written ()
  "Writing :stdin and closing it lets a command that reads stdin finish."
  (let ((proc (tuned-async-start '("cat") :stdin "hello\n")))
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'done))
    (should (equal (tuned-async-output proc) "hello\n"))))

(ert-deftest tuned-async-named-buffer-is-kept ()
  "A :buffer-name buffer with the pid survives the end."
  (let ((proc (tuned-async-start '("sh" "-c" "printf x")
                              :buffer-name "*tuned-async-test %d*")))
    (should (tuned-async-test--wait-settled proc))
    (let ((buffer (tuned-async-buffer proc)))
      (unwind-protect
          (progn
            (should buffer)
            (should (string-match-p "\\`\\*tuned-async-test [0-9]+\\*\\'"
                                    (buffer-name buffer)))
            (should (equal (with-current-buffer buffer (buffer-string)) "x")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; Layer 1: timeout

(ert-deftest tuned-async-timeout-sets-status-and-code ()
  "A timed-out task has status `timeout', code 124 and a cancelled timer."
  (let ((proc (tuned-async-start '("sleep" "5")))
        (exits 0))
    (tuned-async-on-exit proc (lambda (_p) (cl-incf exits)))
    (tuned-async-timeout proc 0.2)
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'timeout))
    (should (= (tuned-async-exit-code proc) 124))
    (should (= exits 1))
    (should-not (process-get proc 'tuned-async-timeout-timer))))

;;; Layer 1: ownership

(ert-deftest tuned-async-own-runs-handlers-in-the-owner ()
  "on-output and on-exit run with the owner buffer current."
  (let* ((owner (generate-new-buffer " *tuned-async-owner*"))
         (proc (tuned-async-start '("sh" "-c" "printf hi")))
         (output-buffers nil)
         (exit-buffer nil)
         (seen nil))
    (unwind-protect
        (progn
          (tuned-async-own proc owner)
          (should (memq proc (buffer-local-value 'tuned-async--owned owner)))
          (tuned-async-on-output
           proc
           (lambda (_p chunk)
             (push (current-buffer) output-buffers)
             (setq seen (concat seen chunk))))
          (tuned-async-on-exit
           proc
           (lambda (_p) (setq exit-buffer (current-buffer))))
          (should (tuned-async-test--wait-settled proc))
          (should (eq exit-buffer owner))
          (should (seq-every-p (lambda (b) (eq b owner)) output-buffers))
          (should (equal seen "hi"))
          (should (equal (tuned-async-output proc) "hi"))
          (should-not (memq proc (buffer-local-value 'tuned-async--owned owner))))
      (kill-buffer owner))))

(ert-deftest tuned-async-killing-the-owner-aborts-its-tasks ()
  "Killing the owner aborts the task: no on-exit, one finally."
  (let* ((owner (generate-new-buffer " *tuned-async-owner*"))
         (proc (tuned-async-start '("sleep" "5")))
         (exits 0)
         (finals 0))
    (tuned-async-own proc owner)
    (tuned-async-on-exit proc (lambda (_p) (cl-incf exits)))
    (tuned-async-finally proc (lambda (_p) (cl-incf finals)))
    (kill-buffer owner)
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'cancelled))
    (should (tuned-async-test--late-p (lambda () (= finals 1))))
    (should (= exits 0))
    (should (= finals 1))))

;;; Layer 1: slots

(ert-deftest tuned-async-slot-replaces-and-leaves-the-new-entry ()
  "A newer task aborts the older one; the older finally keeps the new entry."
  (let* ((owner (generate-new-buffer " *tuned-async-slot*"))
         (first (tuned-async-start '("sleep" "5")))
         (first-finals 0))
    (unwind-protect
        (progn
          (tuned-async-own first owner)
          (tuned-async-slot first 'job)
          (tuned-async-finally first (lambda (_p) (cl-incf first-finals)))
          (should (eq (tuned-async-slot-process 'job owner) first))
          (let ((second (tuned-async-start '("true"))))
            (tuned-async-own second owner)
            (tuned-async-slot second 'job)
            (should (tuned-async-test--wait (lambda () (= first-finals 1))))
            (should (eq (tuned-async-status first) 'cancelled))
            (should (eq (tuned-async-slot-process 'job owner) second))
            (should (tuned-async-test--wait
                     (lambda () (not (tuned-async-live-p second)))))
            (should (tuned-async-test--wait
                     (lambda () (null (tuned-async-slot-process 'job owner)))))
            (should (= first-finals 1))))
      (kill-buffer owner))))

;;; Layer 1: queues

(ert-deftest tuned-async-queue-runs-jobs-in-order ()
  "Jobs of one key run strictly one after another."
  (let* ((key (cons (current-buffer) 'order))
         (log nil))
    (dotimes (i 3)
      (let ((text (substring "abc" i (1+ i))))
        (tuned-async-queue-push
         key
         (tuned-async-job
          (lambda ()
            (tuned-async-on-exit
             (tuned-async-start
              (list "sh" "-c" (concat "sleep 0.05; printf " text)))
             (lambda (p) (push (tuned-async-output p) log))))))))
    (should (tuned-async-queue-busy-p key))
    (should (= (tuned-async-queue-size key) 2))
    (should (tuned-async-test--wait
             (lambda () (not (tuned-async-queue-busy-p key)))))
    (should (equal (nreverse log) '("a" "b" "c")))
    (should (= (tuned-async-queue-size key) 0))))

(ert-deftest tuned-async-attempts-tries-until-one-succeeds ()
  "Attempts fall through on failure and stop after the first success."
  (let* ((key (cons (current-buffer) 'attempts))
         (tried nil)
         (done-output nil))
    (tuned-async-queue-push
     key
     (tuned-async-attempts
      (list (lambda ()
              (push 'one tried)
              (tuned-async-start '("false")))
            (lambda ()
              (push 'two tried)
              (tuned-async-start '("sh" "-c" "printf ok; exit 0")))
            (lambda ()
              (push 'three tried)
              (tuned-async-start '("true"))))
      (lambda (p) (setq done-output (tuned-async-output p)))))
    (should (tuned-async-test--wait
             (lambda () (not (tuned-async-queue-busy-p key)))))
    (should (equal (nreverse tried) '(one two)))
    (should (equal done-output "ok"))
    (should (= (tuned-async-queue-size key) 0))))

(ert-deftest tuned-async-queue-calls-on-drain-once ()
  "ON-DRAIN runs once when the queue empties, and a new one replaces it."
  (let* ((key (cons (current-buffer) 'drain))
         (drains 0))
    (tuned-async-queue-push
     key
     (tuned-async-job (lambda () (tuned-async-start '("true"))))
     (lambda () (cl-incf drains)))
    (should (tuned-async-test--wait
             (lambda () (not (tuned-async-queue-busy-p key)))))
    (should (tuned-async-test--late-p (lambda () (= drains 1))))
    (should (= drains 1))
    (tuned-async-queue-push
     key
     (tuned-async-job (lambda () (tuned-async-start '("true"))))
     (lambda () (cl-incf drains)))
    (should (tuned-async-test--wait
             (lambda () (not (tuned-async-queue-busy-p key)))))
    (should (tuned-async-test--late-p (lambda () (= drains 2))))
    (should (= drains 2))))

(ert-deftest tuned-async-queue-clear-aborts-the-running-job ()
  "Clearing a queue drops its waiting jobs and aborts the running one."
  (let* ((key (cons (current-buffer) 'clear))
         (proc nil)
         (finals 0))
    (tuned-async-queue-push
     key
     (tuned-async-job (lambda () (setq proc (tuned-async-start '("sleep" "5"))))))
    (tuned-async-finally proc (lambda (_p) (cl-incf finals)))
    (should (tuned-async-queue-busy-p key))
    (tuned-async-queue-clear key)
    (should-not (tuned-async-queue-busy-p key))
    (should (tuned-async-test--wait-settled proc))
    (should (eq (tuned-async-status proc) 'cancelled))
    (should (tuned-async-test--late-p (lambda () (= finals 1))))
    (should (= finals 1))))

(ert-deftest tuned-async-enqueue-key-is-shared-across-buffers ()
  "One explicit key serializes jobs enqueued from different buffers."
  (let* ((key (cons "shared-repository" 'git-writes))
         (log nil)
         (first (generate-new-buffer " *tuned-async-key-1*"))
         (second (generate-new-buffer " *tuned-async-key-2*")))
    (unwind-protect
        (progn
          (dolist (pair (list (cons first "a") (cons second "b")))
            (with-current-buffer (car pair)
              (tuned-async-enqueue
               key
               (list :attempts
                     (list (list "sh" "-c"
                                 (concat "sleep 0.05; printf " (cdr pair))))
                     :on-done (lambda (output _code) (push output log))))))
          (should (tuned-async-queue-busy-p key))
          (should (= (tuned-async-queue-length key) 1))
          (should (tuned-async-test--wait
                   (lambda () (not (tuned-async-queue-busy-p key)))))
          (should (equal (nreverse log) '("a" "b"))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest tuned-async-enqueue-skips-jobs-of-a-dead-owner ()
  "A job whose owner died before it started is skipped, not run."
  (let* ((key (cons "dead-owner" 'git-writes))
         (owner (generate-new-buffer " *tuned-async-dead-owner*"))
         (skipped nil)
         (live nil))
    (unwind-protect
        (progn
          (with-current-buffer owner
            (tuned-async-enqueue key (list :attempts '(("sleep" "0.3"))))
            (tuned-async-enqueue
             key
             (list :attempts '(("sh" "-c" "printf x"))
                   :on-start (lambda () (setq skipped 'started))
                   :on-done (lambda (_output _code) (setq skipped 'done)))))
          (should (tuned-async-queue-busy-p key))
          ;; The first job is running and owned by OWNER; killing it aborts
          ;; that job, and the waiting one must not start in a dead buffer.
          (kill-buffer owner)
          (tuned-async-enqueue
           key
           (list :owner nil
                 :attempts '(("sh" "-c" "printf live"))
                 :on-done (lambda (_output _code) (setq live t))))
          (should (tuned-async-test--wait
                   (lambda () (not (tuned-async-queue-busy-p key)))))
          (should (tuned-async-test--late-p (lambda () live)))
          (should-not skipped))
      (when (buffer-live-p owner) (kill-buffer owner)))))

(ert-deftest tuned-async-clear-queue-accepts-an-explicit-key ()
  "Clearing a keyed queue drops waiting jobs and aborts the running one."
  (with-temp-buffer
    (let* ((key (cons "clear-key" 'git-writes))
           proc)
      (tuned-async-enqueue key (list :attempts '(("sleep" "5"))))
      (tuned-async-enqueue key (list :attempts '(("true"))))
      (setq proc (nth 3 (assoc key tuned-async--queue-table)))
      (should (= (tuned-async-queue-length key) 1))
      (tuned-async-clear-queue key)
      (should (= (tuned-async-queue-length key) 0))
      (should-not (tuned-async-queue-busy-p key))
      (should (tuned-async-test--wait-settled proc))
      (should (eq (tuned-async-status proc) 'cancelled)))))

;;; Compatibility layer

(ert-deftest tuned-async-run-calls-on-done-in-the-owner ()
  "ON-DONE gets the output and code, with the owner buffer current."
  (with-temp-buffer
    (let ((owner (current-buffer))
          result)
      (tuned-async-run :command '("sh" "-c" "printf hi; exit 3")
                    :on-done (lambda (output code)
                               (setq result (list output code
                                                  (current-buffer)))))
      (should (tuned-async-busy-p))
      (should (tuned-async-test--wait (lambda () result)))
      (should (equal result (list "hi" 3 owner)))
      (should-not (tuned-async-busy-p)))))

(ert-deftest tuned-async-run-cancel-skips-on-done ()
  "Cancelling a run kills it without calling ON-DONE."
  (with-temp-buffer
    (let (calls proc)
      (setq proc (tuned-async-run :command '("sleep" "5")
                               :slot 'compat
                               :on-done (lambda (&rest _) (push 'done calls))))
      (should (tuned-async-busy-p 'compat))
      (tuned-async-cancel 'compat)
      (should (tuned-async-test--wait
               (lambda () (not (tuned-async-live-p proc)))))
      (should (tuned-async-test--late-p (lambda () (null calls))))
      (should-not calls)
      (should-not (tuned-async-busy-p 'compat)))))

(ert-deftest tuned-async-run-unowned-calls-on-done-and-ignores-slot ()
  "With :owner nil, ON-DONE runs and :slot is ignored."
  (with-temp-buffer
    (let (result)
      (tuned-async-run :command '("echo" "done") :owner nil :slot 'ignored
                    :on-done (lambda (output _code) (setq result output)))
      (should-not (tuned-async-busy-p 'ignored))
      (should (tuned-async-test--wait (lambda () result)))
      (should (equal result "done\n"))
      (should-not (tuned-async-slot-process 'ignored (current-buffer))))))

(ert-deftest tuned-async-run-streams-output-into-on-output-and-on-done ()
  "Every :on-output chunk is part of the OUTPUT ON-DONE receives."
  (with-temp-buffer
    (let (chunks result)
      (tuned-async-run
       :command '("sh" "-c" "printf one; sleep 0.05; printf two")
       :on-output (lambda (chunk) (push chunk chunks))
       :on-done (lambda (output _code) (setq result output)))
      (should (tuned-async-test--wait (lambda () result)))
      (should chunks)
      (should (equal (apply #'concat (nreverse chunks)) result))
      (should (equal result "onetwo")))))

(ert-deftest tuned-async-run-timeout-reports-the-timeout-code ()
  "A timed-out compatibility run gives ON-DONE `tuned-async-timeout-code'."
  (with-temp-buffer
    (let (code)
      (tuned-async-run :command '("sleep" "5") :timeout 0.2
                    :on-done (lambda (_output exit) (setq code exit)))
      (should (tuned-async-test--wait (lambda () code)))
      (should (= code tuned-async-timeout-code)))))

(ert-deftest tuned-async-run-keeps-the-process-name ()
  "The :name reaches the process, so errors and traces can name it."
  (let ((proc (tuned-async-run :command '("true") :name "compat-name")))
    (should (equal (process-name proc) "compat-name"))
    (should (tuned-async-test--wait
             (lambda () (not (tuned-async-live-p proc)))))))

(ert-deftest tuned-async-clear-queue-drops-waiting-and-running ()
  "The compatibility clear drops waiting jobs and aborts the running one."
  (with-temp-buffer
    (tuned-async-enqueue 'q (list :attempts '(("sleep" "5"))))
    (tuned-async-enqueue 'q (list :attempts '(("true"))))
    (should (= (tuned-async-queue-length 'q) 1))
    (tuned-async-clear-queue 'q)
    (should (= (tuned-async-queue-length 'q) 0))
    (should-not (tuned-async-queue-busy-p 'q))))

(ert-deftest tuned-async-killing-a-buffer-clears-its-queue ()
  "Killing the owner drops its queued jobs and aborts the running one."
  (let* ((buffer (generate-new-buffer " *tuned-async-queue-owner*"))
         proc)
    (with-current-buffer buffer
      (tuned-async-enqueue 'q (list :attempts '(("sleep" "5"))))
      (setq proc (tuned-async-slot-process 'q buffer)))
    (should (tuned-async-queue-busy-p 'q buffer))
    (kill-buffer buffer)
    (should-not (tuned-async-queue-busy-p 'q buffer))
    (should (tuned-async-test--wait
             (lambda () (not (tuned-async-live-p proc)))))))

(provide 'tuned-async-tests)
;;; tuned-async-tests.el ends here
