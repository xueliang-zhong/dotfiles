;;; tuned-doctor-tests.el --- ERT tests for tuned-doctor -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp \
;;        -l tuned-emacs/tests/tuned-doctor-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'tuned-doctor)

(defvar tuned-dotfiles)

(defmacro tuned-doctor-test--with-repo (var &rest body)
  "Bind VAR to a temporary dotfiles repository while running BODY."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "tuned-doctor-test-" t))
          (tuned-emacs (expand-file-name "tuned-emacs/" ,var)))
     (make-directory (expand-file-name "lisp/" tuned-emacs) t)
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(ert-deftest tuned-doctor-render-shows-failures-and-fixes ()
  "A failing check prints FAIL and its suggested command."
  (let ((text (tuned-doctor--render
               (list (tuned-doctor--result 'ok "version" "1.0")
                     (tuned-doctor--result 'fail "byte code" "1 stale" "run tuned sync")))))
    (should (string-match-p "ok    version: 1.0" text))
    (should (string-match-p "FAIL  byte code: 1 stale" text))
    (should (string-match-p "-> run tuned sync" text))))

(ert-deftest tuned-doctor-version-comes-from-the-version-file ()
  "Without the profile variable, the VERSION file is read."
  (tuned-doctor-test--with-repo dir
    (with-temp-file (expand-file-name "VERSION" tuned-emacs)
      (insert "9.9.9\n"))
    (let ((tuned-dotfiles dir))
      (should (equal (tuned-doctor--version-string) "9.9.9"))
      (let ((result (tuned-doctor--check-version)))
        (should (eq (plist-get result :status) 'ok))
        (should (equal (plist-get result :detail) "9.9.9"))))))

(ert-deftest tuned-doctor-detects-orphan-byte-code ()
  "A .elc without its source is a failure."
  (tuned-doctor-test--with-repo dir
    (with-temp-file (expand-file-name "lisp/tuned-gone.elc" tuned-emacs)
      (insert ""))
    (let ((tuned-dotfiles dir))
      (let ((result (tuned-doctor--check-elc)))
        (should (eq (plist-get result :status) 'fail))
        (should (string-match-p "orphan" (plist-get result :detail)))))))

(ert-deftest tuned-doctor-detects-missing-autoloads ()
  "A missing generated autoload file is a failure."
  (tuned-doctor-test--with-repo dir
    (with-temp-file (expand-file-name "lisp/tuned-async.el" tuned-emacs)
      (insert ""))
    (let ((tuned-dotfiles dir))
      (let ((result (tuned-doctor--check-autoloads)))
        (should (eq (plist-get result :status) 'fail))
        (should (string-match-p "missing" (plist-get result :detail)))))))

(ert-deftest tuned-doctor-package-lock-missing-is-a-warning ()
  "A fresh profile has no packages.lock yet; sync writes it."
  (let ((user-emacs-directory (make-temp-file "tuned-doctor-profile-" t)))
    (unwind-protect
        (let ((result (tuned-doctor--check-packages-lock)))
          (should (eq (plist-get result :status) 'warn))
          (should (string-match-p "packages.lock" (plist-get result :title))))
      (delete-directory user-emacs-directory t))))

(provide 'tuned-doctor-tests)
;;; tuned-doctor-tests.el ends here
