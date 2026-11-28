;;; xl-async-evaluate-tests.el --- tests for the child Emacs evaluator -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L tuned-emacs/lisp -L tuned-emacs/tests \
;;        -l tuned-emacs/tests/xl-async-evaluate-tests.el \
;;        -f ert-run-tests-batch-and-exit

;; The first six tests are the old xl-async-evaluate baseline; their names
;; and assertions are unchanged.

(require 'ert)
(require 'cl-lib)
(require 'xl-async-evaluate)
(require 'xl-native-compile)

(defun xl-async-evaluate-test--wait (box &optional seconds)
  "Wait until BOX holds a result, at most SECONDS (default 60)."
  (let ((deadline (+ (float-time) (or seconds 60))))
    (while (and (null (car box)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (car box)))

(defun xl-async-evaluate-test--wait-until (predicate &optional seconds)
  "Wait until PREDICATE is non-nil, at most SECONDS (default 60)."
  (let ((deadline (+ (float-time) (or seconds 60))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun xl-async-evaluate-test--run (form &rest args)
  "Evaluate FORM asynchronously; return (RESULT . PROCESS).
ARGS are passed through to `xl-async-evaluate'."
  (let* ((box (list nil))
         (proc (apply #'xl-async-evaluate
                      form
                      (lambda (p)
                        (setcar box (or (xl-async-evaluate-result p)
                                        '(:none))))
                      args)))
    (cons (xl-async-evaluate-test--wait box) proc)))

(defun xl-async-evaluate-test--kill-async-buffers ()
  "Abort and kill every *async-buffer* left by an earlier test."
  (dolist (buffer (buffer-list))
    (when (xl-async-evaluate--async-buffer-p buffer)
      (let ((process (get-buffer-process buffer)))
        (when (and process (xl-async-live-p process))
          (xl-async-abort process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; ============================================================
;;; Baseline: value, error, unreadable, buffer, no callback, no result
;;; ============================================================

(ert-deftest xl-async-evaluate-returns-the-value ()
  (let ((run (xl-async-evaluate-test--run '(+ 1 2))))
    (should (equal (car run) '(:ok 3)))))

(ert-deftest xl-async-evaluate-reports-an-error-with-its-message ()
  (let ((run (xl-async-evaluate-test--run '(car 1))))
    (should (eq (car (car run)) :error))
    (should (string-match-p "Wrong type argument" (cadr (car run))))))

(ert-deftest xl-async-evaluate-reports-a-value-that-cannot-be-read-back ()
  (let ((run (xl-async-evaluate-test--run '(current-buffer))))
    (should (eq (car (car run)) :error))
    (should (string-match-p "unreadable result" (cadr (car run))))))

(ert-deftest xl-async-evaluate-keeps-the-output-in-a-named-buffer ()
  (let* ((run (xl-async-evaluate-test--run '(+ 20 22)))
         (proc (cdr run))
         (buffer (process-buffer proc)))
    (should (equal (car run) '(:ok 42)))
    (should (string-match-p "\\`\\*async-buffer [0-9]+\\*\\'" (buffer-name buffer)))
    (should (= (process-id proc)
               (string-to-number
                (nth 2 (split-string (buffer-name buffer) "[* ]+")))))
    (with-current-buffer buffer
      (should (string-match-p (regexp-quote xl-async-evaluate-result-marker)
                              (buffer-string))))
    (kill-buffer buffer)))

(ert-deftest xl-async-evaluate-without-a-callback-still-runs ()
  (let ((proc (xl-async-evaluate '(ignore))))
    (should (xl-async-evaluate-test--wait-until
             (lambda () (not (xl-async-live-p proc)))))
    (should (eq (process-status proc) 'exit))
    (kill-buffer (process-buffer proc))))

(ert-deftest xl-async-evaluate-reports-a-child-that-prints-no-result ()
  "A child that ends without the marker still gets a callback, with the reason."
  (let* ((box (list nil))
         (xl-async-evaluate-program "/usr/bin/false")
         (proc (xl-async-evaluate
                '(ignore)
                (lambda (p)
                  (setcar box (or (xl-async-evaluate-result p)
                                  '(:none)))))))
    (xl-async-evaluate-test--wait box)
    (should (eq (car (car box)) :error))
    (should (string-match-p "no result" (cadr (car box))))
    (kill-buffer (process-buffer proc))))

;;; ============================================================
;;; Result on the process, exactly one callback, timeout, non-ASCII
;;; ============================================================

(ert-deftest xl-async-evaluate-result-is-on-the-process ()
  "The callback gets the process, and the result is read from it."
  (let* ((box (list nil))
         (proc (xl-async-evaluate
                '(+ 20 22)
                (lambda (p) (setcar box (xl-async-evaluate-result p))))))
    (should (equal (xl-async-evaluate-test--wait box) '(:ok 42)))
    (should (equal (xl-async-evaluate-result proc) '(:ok 42)))
    (should (equal (xl-async-evaluate-result proc 0) '(:ok 42)))))

(ert-deftest xl-async-evaluate-calls-back-exactly-once ()
  (let ((calls 0)
        proc)
    (setq proc (xl-async-evaluate '(+ 1 2) (lambda (_p) (cl-incf calls))))
    (should (xl-async-evaluate-test--wait-until
             (lambda () (not (xl-async-live-p proc)))))
    (accept-process-output nil 0.1)
    (should (= calls 1))))

(ert-deftest xl-async-evaluate-timeout-gets-an-error ()
  "A timed-out evaluation calls back once with a timeout error result."
  (let ((box (list nil))
        (calls 0)
        proc)
    (setq proc (xl-async-evaluate
                '(sleep-for 5)
                (lambda (p)
                  (cl-incf calls)
                  (setcar box (xl-async-evaluate-result p)))))
    (xl-async-timeout proc 0.2)
    (let ((result (xl-async-evaluate-test--wait box)))
      (should (eq (car result) :error))
      (should (string-match-p "timed out" (cadr result)))
      (should (= calls 1)))))

(ert-deftest xl-async-evaluate-round-trips-non-ascii ()
  (let ((run (xl-async-evaluate-test--run '(concat "héllo" " 世界"))))
    (should (equal (car run) '(:ok "héllo 世界")))))

(ert-deftest xl-async-evaluate-prepends-the-load-path ()
  (let* ((directory (file-name-as-directory (make-temp-file "xl-eval-lp-" t)))
         (helper (expand-file-name "xl-eval-lp-helper.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file helper
            (insert "(defun xl-eval-lp-value () 42)\n"
                    "(provide 'xl-eval-lp-helper)\n"))
          (let ((run (xl-async-evaluate-test--run
                      '(progn (require 'xl-eval-lp-helper)
                              (xl-eval-lp-value))
                      :load-path (list directory))))
            (should (equal (car run) '(:ok 42)))))
      (delete-directory directory t))))

;;; ============================================================
;;; Buffer cleanup
;;; ============================================================

(ert-deftest xl-async-evaluate-keeps-only-the-latest-buffers ()
  "Starting one more deletes the oldest finished buffer, not a running one."
  (xl-async-evaluate-test--kill-async-buffers)
  (unwind-protect
      (let ((buffers nil))
        (dotimes (_ xl-async-evaluate-buffer-limit)
          (let ((run (xl-async-evaluate-test--run '(+ 1 1))))
            (should (equal (car run) '(:ok 2)))
            (push (process-buffer (cdr run)) buffers)))
        (setq buffers (nreverse buffers))
        (let* ((running (xl-async-evaluate '(sleep-for 5)))
               (running-buffer (process-buffer running)))
          (should (buffer-live-p running-buffer))
          (should-not (buffer-live-p (car buffers)))
          (should (buffer-live-p (nth (1- (length buffers)) buffers)))
          (xl-async-abort running)
          (should (xl-async-evaluate-test--wait-until
                   (lambda () (not (xl-async-live-p running)))))
          (when (buffer-live-p running-buffer)
            (kill-buffer running-buffer))))
    (xl-async-evaluate-test--kill-async-buffers)))

;;; ============================================================
;;; Worker
;;; ============================================================

(ert-deftest xl-async-worker-delivers-results-by-id ()
  "Requests run in order and each callback gets its own result."
  (let ((worker (xl-async-worker-start))
        (log nil)
        (ids nil))
    (unwind-protect
        (progn
          (dolist (form '((+ 1 1) (+ 2 2) (+ 3 3)))
            (push (xl-async-worker-eval
                   worker form
                   (lambda (p)
                     (push (xl-async-evaluate-result p) log)))
                  ids))
          (should (equal (nreverse ids) '(0 1 2)))
          (should (xl-async-evaluate-test--wait-until
                   (lambda () (= (length log) 3))))
          (should (equal (nreverse log) '((:ok 2) (:ok 4) (:ok 6)))))
      (xl-async-worker-stop worker)
      (xl-async-evaluate-test--wait-until
       (lambda () (not (process-live-p worker))) 10))))

(ert-deftest xl-async-worker-keeps-state-between-requests ()
  "A worker keeps its state; a fresh evaluate would not see xl-probe."
  (let ((worker (xl-async-worker-start))
        (log nil))
    (unwind-protect
        (progn
          (dolist (form '((+ 1 2) (setq xl-probe 7) xl-probe))
            (xl-async-worker-eval
             worker form
             (lambda (p) (push (xl-async-evaluate-result p) log))))
          (should (xl-async-evaluate-test--wait-until
                   (lambda () (= (length log) 3))))
          (should (equal (nreverse log) '((:ok 3) (:ok 7) (:ok 7)))))
      (xl-async-worker-stop worker)
      (xl-async-evaluate-test--wait-until
       (lambda () (not (process-live-p worker))) 10))))

(ert-deftest xl-async-worker-killed-fails-pending-requests ()
  "Killing a worker gives its unfinished requests an error result."
  (let ((worker (xl-async-worker-start))
        (box (list nil)))
    (unwind-protect
        (progn
          (xl-async-worker-eval
           worker '(sleep-for 5)
           (lambda (p) (setcar box (xl-async-evaluate-result p))))
          (xl-async-abort worker)
          (let ((result (xl-async-evaluate-test--wait box)))
            (should (eq (car result) :error))
            (should (string-match-p "worker exited" (cadr result)))))
      (when (xl-async-live-p worker) (xl-async-abort worker)))))

(ert-deftest xl-async-worker-stop-drops-pending-callbacks ()
  "Stopping a worker on purpose does not call the remaining callbacks."
  (let ((worker (xl-async-worker-start))
        (calls 0))
    (unwind-protect
        (progn
          (xl-async-worker-eval worker '(sleep-for 0.3)
                                (lambda (_p) (cl-incf calls)))
          (xl-async-worker-stop worker)
          (should (xl-async-evaluate-test--wait-until
                   (lambda () (not (process-live-p worker))) 10))
          (accept-process-output nil 0.1)
          (should (= calls 0)))
      (when (xl-async-live-p worker) (xl-async-abort worker)))))

;;; ============================================================
;;; Native compilation
;;; ============================================================

(ert-deftest xl-async-native-compile-without-support-signals-and-starts-nothing ()
  "Without native compilation: a `user-error', no child, no callback."
  (let ((started nil)
        (called nil))
    (cl-letf (((symbol-function 'native-comp-available-p)
               (lambda () nil))
              ((symbol-function 'xl-async-start)
               (lambda (&rest _) (setq started t))))
      (should-error (xl-async-native-compile
                     "/tmp/xl-eval-unavailable.el"
                     (lambda (_proc) (setq called t)))
                    :type 'user-error))
    (should-not started)
    (should-not called)))

(ert-deftest xl-async-native-compile-uses-the-load-path ()
  "Compiling a file that needs a helper only works with :load-path."
  (skip-unless (and (fboundp 'native-comp-available-p)
                    (native-comp-available-p)))
  (let* ((default-directory (expand-file-name default-directory))
         (directory (file-name-as-directory (make-temp-file "xl-eval-nc-" t)))
         (helper (expand-file-name "xl-eval-nc-helper.el" directory))
         (target (expand-file-name "xl-eval-nc-target.el" directory))
         (cache (expand-file-name "cache/" directory))
         (process-environment
          (append (list (concat "HOME=" directory)
                        (concat "XDG_CACHE_HOME=" cache))
                  process-environment))
         (box (list nil)))
    (unwind-protect
        (progn
          (with-temp-file helper
            (insert "(defun xl-eval-nc-value () 42)\n"
                    "(provide 'xl-eval-nc-helper)\n"))
          (with-temp-file target
            (insert "(eval-when-compile (require 'xl-eval-nc-helper))\n"
                    "(defun xl-eval-nc-target () (xl-eval-nc-value))\n"))
          (let ((proc (xl-async-native-compile
                       target
                       (lambda (p)
                         (setcar box (xl-async-evaluate-result p)))
                       :load-path (list directory))))
            (let ((result (xl-async-evaluate-test--wait box)))
              (should (eq (car result) :ok))
              (should (stringp (cadr result)))
              (should (string-suffix-p ".eln" (cadr result)))
              (should (file-exists-p (cadr result))))
            (when (buffer-live-p (process-buffer proc))
              (kill-buffer (process-buffer proc)))))
      (delete-directory directory t))))

;; A feature symbol is resolved to its .el source; an unknown feature is refused
;; before any child process starts.
(ert-deftest xl-async-native-compile-resolves-a-feature-to-its-source ()
  "`xl-native-compile--source-of' maps an .elc to its .el, and an unknown
feature signals `user-error'."
  (let* ((dir (make-temp-file "xl-nc-" t))
         (el (expand-file-name "xlnc-probe.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file el
            (insert ";;; xlnc-probe.el --- probe -*- lexical-binding: t; -*-\n(provide 'xlnc-probe)\n"))
          (byte-compile-file el)
          (should (equal (xl-native-compile--source-of (concat el "c")) el))
          (let ((load-path (cons dir load-path)))
            (should (equal (xl-native-compile--source-of
                            (locate-library "xlnc-probe"))
                           el))
            (should-error (xl-async-native-compile 'xlnc-no-such-feature-zz)
                          :type 'user-error)))
      (ignore-errors (delete-directory dir t)))))

(provide 'xl-async-evaluate-tests)
;;; xl-async-evaluate-tests.el ends here
