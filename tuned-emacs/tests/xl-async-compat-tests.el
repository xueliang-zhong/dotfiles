;;; xl-async-compat-tests.el --- ERT tests for the xl-async compatibility layer -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-async-compat-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'xl-async)

(defun xl-test--async-wait (pred)
  "Spin the event loop until PRED returns non-nil, at most 10 seconds."
  (let ((end (+ (float-time) 10)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (should (funcall pred))))

(ert-deftest xl-async-run-delivers-output-in-the-owner ()
  "ON-DONE gets the output and exit code, with the owner buffer current."
  (with-temp-buffer
    (let ((owner (current-buffer)) result)
      (xl-async-run :command '("sh" "-c" "printf hi; exit 3")
                    :on-done (lambda (output code)
                               (setq result (list output code (current-buffer)))))
      (should (xl-async-busy-p))
      (xl-test--async-wait (lambda () result))
      (should (equal result (list "hi" 3 owner)))
      (should-not (xl-async-busy-p)))))

(ert-deftest xl-async-stdin-reaches-the-process ()
  "STDIN is written and closed, so a reader such as `git apply' finishes."
  (with-temp-buffer
    (let (result)
      (xl-async-run :command '("cat") :stdin "patch text\n"
                    :on-done (lambda (output _code) (setq result output)))
      (xl-test--async-wait (lambda () result))
      (should (equal result "patch text\n")))))

(ert-deftest xl-async-slot-cancels-the-older-run ()
  "A newer run in the same slot kills the older one, whose ON-DONE never runs."
  (with-temp-buffer
    (let (calls)
      (xl-async-run :command '("sleep" "5") :slot 'test
                    :on-done (lambda (&rest _) (push 'old calls)))
      (xl-async-run :command '("true") :slot 'test
                    :on-done (lambda (&rest _) (push 'new calls)))
      (xl-test--async-wait (lambda () (not (xl-async-busy-p 'test))))
      ;; Give a stray sentinel of the killed run a chance to fire.
      (accept-process-output nil 0.1)
      (should (equal calls '(new))))))

(ert-deftest xl-async-killing-the-owner-kills-its-processes ()
  "A killed owner takes its processes with it, even after a mode change."
  (let* ((buffer (generate-new-buffer " *xl-async-test*"))
         (proc (with-current-buffer buffer
                 (prog1 (xl-async-run :command '("sleep" "5") :slot 'test)
                   ;; A major mode resets buffer-locals; ownership survives.
                   (fundamental-mode)))))
    (should (xl-async-busy-p 'test buffer))
    (kill-buffer buffer)
    (should-not (process-live-p proc))))

(ert-deftest xl-async-unowned-run-outlives-buffers ()
  "With :owner nil, ON-DONE runs and no buffer tracks the process."
  (let (result)
    (with-temp-buffer
      (xl-async-run :command '("echo" "done") :owner nil
                    :on-done (lambda (output _code) (setq result output)))
      (should-not (xl-async-busy-p)))
    (xl-test--async-wait (lambda () result))
    (should (equal result "done\n"))))

(ert-deftest xl-async-queue-runs-in-order-and-retries ()
  "Jobs run one at a time; a failed attempt falls back; drain runs once."
  (with-temp-buffer
    (let (events)
      (dolist (n '(1 2))
        (xl-async-enqueue
         'q (list :attempts (list '("false") (list "echo" (number-to-string n)))
                  :on-start (lambda () (push (list 'start n) events))
                  :on-done (lambda (output code)
                             (push (list 'done (string-trim output) code) events)))
         (lambda () (push 'drained events))))
      (should (= 1 (xl-async-queue-length 'q)))
      (should (xl-async-queue-busy-p 'q))
      (xl-test--async-wait (lambda () (memq 'drained events)))
      (should (equal (nreverse events)
                     '((start 1) (done "1" 0) (start 2) (done "2" 0) drained)))
      (should-not (xl-async-queue-busy-p 'q)))))
