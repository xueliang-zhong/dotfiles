;;; xl-package-install.el --- install a package without blocking Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; `package-install' blocks the editor until the package is on disk.  This
;; command does the same work in a child Emacs with this profile, so the editor
;; keeps running.  The child's output goes to a monitor buffer, and the mode
;; line of that buffer shows a spinner until the child exits.  The name is read
;; with `completing-read', so Vertico and Consult still apply.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'package)
(require 'xl-async)
(require 'xl-spinner)

(defvar xl-package-install-buffer-name "*xl-package-install*"
  "Name of the buffer that shows the progress of `xl-package-install'.")

(define-derived-mode xl-package-install-mode special-mode "Package-Install"
  "Monitor of a package installed by `xl-package-install'."
  (xl-spinner-mode-line-install))

(defun xl-package-install--child-command (name)
  "The command of a child Emacs that installs package NAME with this profile.
It loads the profile the way a normal start does, then fetches NAME in parallel
and installs whatever is left, as the blocking path would."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory))
         (form `(progn (require 'xl-package-fetch)
                       (xl-fetch-packages (list ',name))
                       (xl-install-packages (list ',name)))))
    (list exe "--batch" "--init-directory" profile
          "-l" (expand-file-name "early-init.el" profile)
          "--eval" "(run-hooks (quote before-init-hook))"
          "-l" (expand-file-name "init.el" profile)
          "--eval" (format "%S" form))))

(defun xl-package-install--append (buffer text)
  "Append TEXT to BUFFER, keeping the end of the buffer in view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when at-end (goto-char (point-max)))))))

(defun xl-package-install--finish (buffer name process)
  "Stop the spinner in BUFFER and report how the child of NAME ended."
  (let ((code (xl-async-exit-code process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (eql code 0)
            (progn
              (xl-spinner-mode-line-stop (format "installed %s" name))
              (package-activate-all)
              (message "Installed %s" name))
          (xl-spinner-mode-line-stop
           (format "install of %s failed (exit %s)" name code))
          (message "Installing %s failed; see %s" name
                   xl-package-install-buffer-name))))))

;;;###autoload
(defun xl-package-install (name)
  "Install package NAME in a child Emacs, without blocking this one.
The progress of the child is shown in `xl-package-install-buffer-name', with a
spinner in its mode line.  NAME is read with `completing-read', so completion
frameworks such as Vertico and Consult apply."
  (interactive
   (progn
     (package--archives-initialize)
     (list (intern (completing-read
                    "Install package: "
                    (mapcar (lambda (entry) (symbol-name (car entry)))
                            (package--archive-contents))
                    nil t)))))
  (when (package-installed-p name)
    (user-error "%s is already installed" name))
  (let ((buffer (get-buffer-create xl-package-install-buffer-name)))
    (with-current-buffer buffer
      (xl-package-install-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Installing %s\n\n" name))))
    (display-buffer buffer)
    (let* ((process-environment (cons "XL_PROGRESS=1" process-environment))
           (process
            (xl-async-start (xl-package-install--child-command name)
                            :name (format "package-install %s" name)
                            :keep-input nil)))
      (with-current-buffer buffer
        (xl-spinner-mode-line-start (format "installing %s" name)))
      (thread-first
        process
        (xl-async-own buffer)
        (xl-async-on-output
         (lambda (_process chunk)
           (xl-package-install--append buffer chunk)))
        (xl-async-on-exit
         (lambda (proc)
           (xl-package-install--finish buffer name proc)))))))

(provide 'xl-package-install)
;;; xl-package-install.el ends here
