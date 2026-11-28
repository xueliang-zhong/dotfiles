;;; tuned-home-tests.el --- ERT tests for tuned-home.el -*- lexical-binding: t; -*-
;;
;; Run without the profile:
;;   emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-home-tests.el \
;;         -f ert-run-tests-batch-and-exit
;; Batch Emacs has no graphical frame, so these draw the text logo.

(require 'ert)
;; tuned-home turns recentf on, which reads and, at exit, writes its save
;; file.  Point it at a temporary file so the tests never touch the real list.
(defvar recentf-save-file)
(setq recentf-save-file (make-temp-file "tuned-home-recentf-" nil ".eld"))
(require 'tuned-home)

(defvar tuned-dotfiles "/tmp/")

(defmacro tuned-home-test--with-recent (files &rest body)
  "Run BODY with `recentf-list' set to FILES and the home buffer killed after."
  (declare (indent 1))
  `(let ((recentf-list ,files))
     (unwind-protect (progn ,@body)
       (when (get-buffer tuned-home-buffer-name)
         (kill-buffer tuned-home-buffer-name)))))

(ert-deftest tuned-home-draws-the-tuned-brand ()
  "The screen greets with the distribution's name and never says Crafted."
  (tuned-home-test--with-recent '("/tmp/a.txt")
    (with-current-buffer (tuned-home)
      (should (derived-mode-p 'tuned-home-mode))
      (should (string-match-p "Welcome to Tuned Emacs!" (buffer-string)))
      (should-not (string-match-p "Crafted" (buffer-string)))
      (should buffer-read-only))))

(ert-deftest tuned-home-uses-the-text-logo-without-graphics ()
  "In a terminal (and in batch) the logo is text: the sun over broken lines."
  (tuned-home-test--with-recent nil
    (with-current-buffer (tuned-home)
      (dolist (line (last tuned-home-ascii-logo 3))
        (should (string-match-p (regexp-quote line) (buffer-string)))))))

(ert-deftest tuned-home-logo-file-exists ()
  "The SVG logo ships with the distribution, next to lisp/."
  (should (file-readable-p tuned-home-logo-file))
  (should (string-suffix-p "assets/tuned-emacs-logo.svg" tuned-home-logo-file)))

(ert-deftest tuned-home-lists-recent-files-with-digit-keys ()
  "At most nine distinct files, numbered, and digit N opens the Nth."
  (let ((files (append (mapcar (lambda (i) (format "/tmp/tuned-home-%02d.txt" i))
                               (number-sequence 1 11))
                       '("/tmp/tuned-home-01.txt")))
        opened)
    (tuned-home-test--with-recent files
      (with-current-buffer (tuned-home)
        (should (= (length tuned-home--files) 9))
        (should (equal tuned-home--files (delete-dups (copy-sequence tuned-home--files))))
        (should (string-match-p "\\[9\\]  /tmp/tuned-home-09.txt" (buffer-string)))
        (should-not (string-match-p "tuned-home-10.txt" (buffer-string)))
        (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f))))
          (call-interactively (local-key-binding (kbd "3"))))
        (should (equal opened "/tmp/tuned-home-03.txt"))))))

(ert-deftest tuned-home-ends-with-the-startup-time ()
  "The last line says how long startup took."
  (tuned-home-test--with-recent nil
    (let ((after-init-time (time-add before-init-time 1.234)))
      (with-current-buffer (tuned-home)
        (should (string-match-p "Emacs loaded in 1\\.23 seconds[ \t\n]*\\'"
                                (buffer-string)))))))

(ert-deftest tuned-home-revert-redraws ()
  "`g' (revert) draws the screen again from the current recent files."
  (tuned-home-test--with-recent '("/tmp/first.txt")
    (with-current-buffer (tuned-home)
      (setq recentf-list '("/tmp/second.txt"))
      (revert-buffer)
      (should (string-match-p "second.txt" (buffer-string)))
      (should-not (string-match-p "first.txt" (buffer-string))))))

;;; tuned-home-tests.el ends here
