;;; tuned-org-babel-async-tests.el --- ERT tests for tuned-org-babel-async -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-org-babel-async-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'tuned-spinner)
(require 'tuned-org-babel-async)

;; The profile sets this; a module suite run without the profile must too,
;; or the synchronous fallback tests block on the confirmation prompt.
(setq org-confirm-babel-evaluate nil)

(defun tuned-test-org-buffer (body &optional header)
  "Return a temporary org buffer with one bash block running BODY.
HEADER is added to the block's header line.  Point is in the block."
  (require 'org)
  (require 'ob-shell)
  (let ((org-confirm-babel-evaluate nil)
        (dir (make-temp-file "tuned-test-org-" t)))
    (let ((buffer (find-file-noselect (expand-file-name "t.org" dir))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (format "* Test\n\n#+begin_src bash %s\n%s\n#+end_src\n\nafter\n"
                        (or header ":async yes") body))
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^#\\+begin_src")
        (forward-line 1))
      buffer)))

(defun tuned-test-org-wait (buffer)
  "Wait until BUFFER has no running block."
  (let ((end (+ (float-time) 15)))
    (while (and (buffer-local-value 'tuned-org-babel--runs buffer)
                (< (float-time) end))
      (sit-for 0.02)))
  (should-not (buffer-local-value 'tuned-org-babel--runs buffer)))

(defun tuned-test-org-text (buffer)
  "Return the text of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest tuned-org-babel-async-spinner-overlay-while-running ()
  "A running block shows a spinner on its #+RESULTS: line, not in the text."
  (skip-unless (executable-find "bash"))
  (let ((buffer (tuned-test-org-buffer "sleep 0.6; echo late")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          ;; Starting the block wrote the placeholder; only the ticks are tested.
          (set-buffer-modified-p nil)
          (let* ((run (car tuned-org-babel--runs))
                 (overlay (tuned-org-babel-run-overlay run))
                 (first (overlay-get overlay 'display)))
            (should (overlayp overlay))
            (should (tuned-spinner-running-p))
            (should (string-match-p "running… (C-c C-k cancels)" first))
            ;; Frames move, and the buffer text does not change for them.
            (tuned-spinner-tick (current-buffer))
            (should-not (equal first (overlay-get overlay 'display)))
            (should-not (buffer-modified-p))
            (should (string-match-p "^#\\+RESULTS:$" (tuned-test-org-text buffer)))
            (tuned-test-org-wait buffer)
            ;; Done: the overlay is gone and so is the spinner.
            (should-not (overlay-buffer overlay))
            (should-not (tuned-spinner-running-p))
            (should (string-match-p "^: late$" (tuned-test-org-text buffer)))))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-mode-line-counts-running-blocks ()
  "The mode line says how many blocks run in the background, then clears."
  (skip-unless (executable-find "bash"))
  (require 'org)
  (require 'ob-shell)
  (let* ((org-confirm-babel-evaluate nil)
         (dir (make-temp-file "tuned-test-org-" t))
         (buffer (find-file-noselect (expand-file-name "two.org" dir))))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "#+begin_src bash :async yes\nsleep 0.6; echo one\n#+end_src\n\n"
                  "#+begin_src bash :async yes\nsleep 0.6; echo two\n#+end_src\n")
          (org-mode)
          (goto-char (point-min))
          (forward-line 1)
          (org-babel-execute-src-block)
          ;; The slot is at the left edge of this buffer's mode line.
          (should (equal '(:eval tuned-spinner-mode-line) (car mode-line-format)))
          (should (string-match-p "1 block running" tuned-spinner-mode-line))
          (goto-char (point-min))
          (re-search-forward "echo two")
          (org-babel-execute-src-block)
          (should (string-match-p "2 blocks running" tuned-spinner-mode-line))
          ;; The overlays still animate: one clock draws both.
          (let ((overlay (tuned-org-babel-run-overlay (car tuned-org-babel--runs))))
            (let ((before (overlay-get overlay 'display)))
              (tuned-spinner-tick (current-buffer))
              (should-not (equal before (overlay-get overlay 'display)))))
          (tuned-test-org-wait buffer)
          (should-not tuned-spinner-mode-line)
          (should-not (tuned-spinner-running-p))
          (should (string-match-p "^: one$" (tuned-test-org-text buffer)))
          (should (string-match-p "^: two$" (tuned-test-org-text buffer))))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-returns-at-once-and-fills-results ()
  "An `:async yes' block returns before it ends; its results come after."
  (skip-unless (executable-find "bash"))
  (let ((buffer (tuned-test-org-buffer "echo first; sleep 0.5; echo second")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((t0 (float-time)))
            (org-babel-execute-src-block)
            (should (< (- (float-time) t0) 0.4))
            (should (string-match-p "running…" (tuned-test-org-text buffer))))
          (tuned-test-org-wait buffer)
          (should (string-match-p "^: first$" (tuned-test-org-text buffer)))
          (should (string-match-p "^: second$" (tuned-test-org-text buffer)))
          (should-not (string-match-p "running…" (tuned-test-org-text buffer))))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-keeps-the-first-results-lines ()
  "Long output: the live tail is replaced by the first lines and a note."
  (skip-unless (executable-find "bash"))
  (let ((tuned-org-babel-result-lines 20)
        (buffer (tuned-test-org-buffer "seq 1 300")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (tuned-test-org-wait buffer)
          (let ((text (tuned-test-org-text buffer)))
            (should (string-match-p "^: 20$" text))
            (should-not (string-match-p "^: 21$" text))
            (should (string-match-p "280 more lines not written" text)))
          ;; The whole output is still in its buffer.
          (with-current-buffer (get-buffer (format "*Org Babel Output: %s:%d*"
                                                   (buffer-name) 3))
            (should (= 300 (count-lines (point-min) (point-max))))))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-reports-exit-code-and-timeout ()
  "A failing block says its exit code; a timed-out one says it was stopped."
  (skip-unless (executable-find "bash"))
  (let ((buffer (tuned-test-org-buffer "echo x; exit 3")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (tuned-test-org-wait buffer)
          (should (string-match-p "exit code 3" (tuned-test-org-text buffer))))
      (kill-buffer buffer)))
  (let ((buffer (tuned-test-org-buffer "sleep 5" ":async yes :timeout 0.3")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (tuned-test-org-wait buffer)
          (should (string-match-p "timed out, stopped" (tuned-test-org-text buffer))))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-cancel-keeps-the-output-so-far ()
  "C-c C-k stops the block and writes that it was cancelled."
  (skip-unless (executable-find "bash"))
  (let ((buffer (tuned-test-org-buffer "echo before; sleep 5; echo never")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (sit-for 0.3)
          (tuned-org-babel-cancel-at-point)
          (should-not tuned-org-babel--runs)
          (let ((text (tuned-test-org-text buffer)))
            (should (string-match-p "^: before$" text))
            (should (string-match-p "cancelled" text))
            ;; The body mentions `never'; the output must not have it.
            (should-not (string-match-p "^: never$" text))))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-running-again-replaces-the-run ()
  "Running a block that is still going cancels that run."
  (skip-unless (executable-find "bash"))
  (let ((buffer (tuned-test-org-buffer "sleep 5; echo OLDRUN")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (should (= 1 (length tuned-org-babel--runs)))
          (goto-char (point-min))
          (re-search-forward "^#\\+begin_src")
          (forward-line 1)
          (org-babel-execute-src-block)
          ;; The second run owns the results now; one run is listed.
          (should (= 1 (length tuned-org-babel--runs)))
          (tuned-org-babel-cancel-at-point)
          ;; The first run was killed: its output never reaches the file.
          (should-not (string-match-p "^: OLDRUN$" (tuned-test-org-text buffer))))
      (kill-buffer buffer))))

(ert-deftest tuned-org-babel-async-sessions-and-plain-blocks-stay-synchronous ()
  "Without `:async yes' (and no default), or with a `:session', a block runs as org-babel does."
  (skip-unless (executable-find "bash"))
  (let ((org-babel-default-header-args:bash nil)
        (org-babel-default-header-args:sh nil))
    (dolist (header '(":results output" ":async yes :session s"))
      (let ((buffer (tuned-test-org-buffer "echo done" header)))
        (unwind-protect
            (with-current-buffer buffer
              (org-babel-execute-src-block)
              (should-not tuned-org-babel--runs)
              (should-not (string-match-p "running…" (tuned-test-org-text buffer)))
              (should (string-match-p "done" (tuned-test-org-text buffer))))
          (kill-buffer buffer))))))

(ert-deftest tuned-org-babel-async-falls-back-silently-for-other-results ()
  "With the default on, blocks that are not output, or have a session, run as before."
  (skip-unless (executable-find "bash"))
  (let ((org-babel-default-header-args:bash '((:async . "yes")))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (dolist (header '(":results value" ":results silent" ":session s" ":results output file"))
        (let ((buffer (tuned-test-org-buffer "echo done" header)))
          (unwind-protect
              (with-current-buffer buffer
                (org-babel-execute-src-block)
                (should-not tuned-org-babel--runs))
            (kill-buffer buffer)))))
    ;; org prints the results of the blocks that ran as before; we print nothing.
    (should-not (cl-some (lambda (m) (string-match-p "async\\|session" m)) messages))))

(ert-deftest tuned-org-babel-async-default-header-makes-blocks-async ()
  "With `:async yes' as the default header, a plain bash block runs in the background."
  (skip-unless (executable-find "bash"))
  (let ((org-babel-default-header-args:bash '((:async . "yes"))))
    (let ((buffer (tuned-test-org-buffer "sleep 0.3; echo later" ":results output")))
      (unwind-protect
          (with-current-buffer buffer
            (org-babel-execute-src-block)
            (should (= 1 (length tuned-org-babel--runs)))
            (tuned-test-org-wait buffer)
            (should (string-match-p "later" (tuned-test-org-text buffer))))
        (kill-buffer buffer)))))
