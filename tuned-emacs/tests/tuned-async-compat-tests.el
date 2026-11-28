;;; tuned-async-compat-tests.el --- ERT tests for the tuned-async compatibility layer -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-async-compat-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'tuned-async)

(defun tuned-test--async-wait (pred)
  "Spin the event loop until PRED returns non-nil, at most 10 seconds."
  (let ((end (+ (float-time) 10)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (should (funcall pred))))

(ert-deftest tuned-async-run-delivers-output-in-the-owner ()
  "ON-DONE gets the output and exit code, with the owner buffer current."
  (with-temp-buffer
    (let ((owner (current-buffer)) result)
      (tuned-async-run :command '("sh" "-c" "printf hi; exit 3")
                    :on-done (lambda (output code)
                               (setq result (list output code (current-buffer)))))
      (should (tuned-async-busy-p))
      (tuned-test--async-wait (lambda () result))
      (should (equal result (list "hi" 3 owner)))
      (should-not (tuned-async-busy-p)))))

(ert-deftest tuned-async-stdin-reaches-the-process ()
  "STDIN is written and closed, so a reader such as `git apply' finishes."
  (with-temp-buffer
    (let (result)
      (tuned-async-run :command '("cat") :stdin "patch text\n"
                    :on-done (lambda (output _code) (setq result output)))
      (tuned-test--async-wait (lambda () result))
      (should (equal result "patch text\n")))))

(ert-deftest tuned-async-slot-cancels-the-older-run ()
  "A newer run in the same slot kills the older one, whose ON-DONE never runs."
  (with-temp-buffer
    (let (calls)
      (tuned-async-run :command '("sleep" "5") :slot 'test
                    :on-done (lambda (&rest _) (push 'old calls)))
      (tuned-async-run :command '("true") :slot 'test
                    :on-done (lambda (&rest _) (push 'new calls)))
      (tuned-test--async-wait (lambda () (not (tuned-async-busy-p 'test))))
      ;; Give a stray sentinel of the killed run a chance to fire.
      (accept-process-output nil 0.1)
      (should (equal calls '(new))))))

(ert-deftest tuned-async-killing-the-owner-kills-its-processes ()
  "A killed owner takes its processes with it, even after a mode change."
  (let* ((buffer (generate-new-buffer " *tuned-async-test*"))
         (proc (with-current-buffer buffer
                 (prog1 (tuned-async-run :command '("sleep" "5") :slot 'test)
                   ;; A major mode resets buffer-locals; ownership survives.
                   (fundamental-mode)))))
    (should (tuned-async-busy-p 'test buffer))
    (kill-buffer buffer)
    (should-not (process-live-p proc))))

(ert-deftest tuned-async-unowned-run-outlives-buffers ()
  "With :owner nil, ON-DONE runs and no buffer tracks the process."
  (let (result)
    (with-temp-buffer
      (tuned-async-run :command '("echo" "done") :owner nil
                    :on-done (lambda (output _code) (setq result output)))
      (should-not (tuned-async-busy-p)))
    (tuned-test--async-wait (lambda () result))
    (should (equal result "done\n"))))

(ert-deftest tuned-async-queue-runs-in-order-and-retries ()
  "Jobs run one at a time; a failed attempt falls back; drain runs once."
  (with-temp-buffer
    (let (events)
      (dolist (n '(1 2))
        (tuned-async-enqueue
         'q (list :attempts (list '("false") (list "echo" (number-to-string n)))
                  :on-start (lambda () (push (list 'start n) events))
                  :on-done (lambda (output code)
                             (push (list 'done (string-trim output) code) events)))
         (lambda () (push 'drained events))))
      (should (= 1 (tuned-async-queue-length 'q)))
      (should (tuned-async-queue-busy-p 'q))
      (tuned-test--async-wait (lambda () (memq 'drained events)))
      (should (equal (nreverse events)
                     '((start 1) (done "1" 0) (start 2) (done "2" 0) drained)))
      (should-not (tuned-async-queue-busy-p 'q)))))
