;;; xl-native-compile.el --- monitor a native compilation -*- lexical-binding: t; -*-

;;; Commentary:

;; `xl-async-native-compile' compiles a file in a child Emacs.  Called without a
;; callback, it shows the child's progress here: a monitor buffer that receives
;; the child's output, and a spinner in its mode line until the child exits.
;; The same shape as `xl-package-install'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xl-async)
(require 'xl-spinner)

(declare-function xl-async-evaluate-result "xl-async-evaluate" (proc &optional id))

(defvar xl-native-compile-buffer-name "*xl-native-compile*"
  "Name of the buffer that shows the progress of a native compilation.")

(define-derived-mode xl-native-compile-mode special-mode "Native-Compile"
  "Monitor of a native compilation started by `xl-async-native-compile'."
  (xl-spinner-mode-line-install))

(defun xl-native-compile--append (buffer text)
  "Append the child's output TEXT to BUFFER.
The lines the child uses for its result protocol are not shown."
  (when (buffer-live-p buffer)
    (let ((shown (replace-regexp-in-string "^XL-ASYNC-RESULT[^\n]*\n?" "" text)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (at-end (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert shown))
          (when at-end (goto-char (point-max))))))))

(defun xl-native-compile--finish (buffer name process)
  "Stop the spinner in BUFFER and report the result of compiling NAME."
  (let ((result (xl-async-evaluate-result process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (if (eq (car-safe result) :ok)
              (progn
                (xl-spinner-mode-line-stop (format "compiled %s" name))
                (insert (format "\nCompiled %s\n-> %s\n" name (cadr result)))
                (message "Native-compiled %s" name))
            (xl-spinner-mode-line-stop (format "native compile of %s failed" name))
            (insert (format "\nFailed: %s\n"
                            (or (cadr result) "no result from the child Emacs")))
            (message "Native compile of %s failed; see %s" name
                     xl-native-compile-buffer-name)))))))

;;;###autoload
(defun xl-native-compile-watch (process file)
  "Show PROCESS, a child that native-compiles FILE, in its monitor buffer.
The buffer is `xl-native-compile-buffer-name'; its mode line shows a spinner
until PROCESS exits."
  (let ((buffer (get-buffer-create xl-native-compile-buffer-name))
        (name (file-name-nondirectory (file-name-sans-extension file))))
    (with-current-buffer buffer
      (xl-native-compile-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Native-compiling %s\n%s\n\n" name file))))
    (display-buffer buffer)
    (with-current-buffer buffer
      (xl-spinner-mode-line-start (format "native-compiling %s" name)))
    (thread-first
      process
      (xl-async-own buffer)
      (xl-async-on-output
       (lambda (_process chunk)
         (xl-native-compile--append buffer chunk)))
      (xl-async-on-exit
       (lambda (proc)
         (xl-native-compile--finish buffer name proc))))
    process))

(provide 'xl-native-compile)
;;; xl-native-compile.el ends here
