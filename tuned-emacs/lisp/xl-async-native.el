;;; xl-async-native.el --- Native-compile Emacs Lisp files in the background -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, processes, compile

;;; Commentary:
;;
;; The asynchronous versions of `emacs-lisp-native-compile' and
;; `emacs-lisp-native-compile-and-load'.  The compilation runs in a fresh
;; child Emacs through `xl-async-native-compile', so Emacs stays responsive.
;;
;;   M-x xl-async-emacs-lisp-native-compile
;;   M-x xl-async-emacs-lisp-native-compile-and-load
;;
;; Both work on the current buffer's file.  A modified buffer is saved
;; first.  If the .eln is already newer than the source, nothing is
;; compiled.  Each buffer has at most one compilation: starting another one
;; in the same buffer cancels the older one (`xl-async-slot'), and killing
;; the buffer cancels it too (`xl-async-own').
;;
;; The child only sees the source file's directory on its load-path, so a
;; file that needs other libraries should either require them with a
;; `:load-path' of its own, or be compiled from a buffer that can do so.

;;; Code:

(require 'xl-async-evaluate)

(defun xl-async-native--check ()
  "Signal a `user-error' unless this Emacs can native-compile."
  (unless (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
    (user-error "Native compilation is not available in this Emacs")))

(defun xl-async-native--up-to-date-p (file)
  "Return non-nil when FILE's .eln exists and is not older than FILE."
  (require 'comp)
  (let ((eln (ignore-errors (comp-el-to-eln-filename file))))
    (and eln (file-exists-p eln)
         (not (file-newer-than-file-p file eln)))))

(defun xl-async-native--compile (file on-success)
  "Native-compile FILE in a child; call ON-SUCCESS with the .eln path.
Errors are reported with `message'.  Return the process, or nil when
the file is already up to date."
  (xl-async-native--check)
  (if (xl-async-native--up-to-date-p file)
      (progn
        (message "xl-async: %s is already native-compiled" (file-name-nondirectory file))
        nil)
    (let* ((buffer (current-buffer))
           (name (file-name-nondirectory file))
           (proc (xl-async-native-compile
                  file
                  (lambda (proc)
                    (let ((result (xl-async-evaluate-result proc)))
                      (if (eq (car result) :ok)
                          (funcall on-success (cadr result))
                        (message "xl-async: native compile of %s failed: %s"
                                 name (cadr result)))))
                  :load-path (list (file-name-directory file))
                  :profile (file-name-as-directory user-emacs-directory))))
      (xl-async-own proc buffer)
      (xl-async-slot proc 'native-compile)
      (message "xl-async: native-compiling %s in the background..." name)
      proc)))

(defun xl-async-native--file ()
  "Save the current buffer if it is modified and return its file name."
  (unless buffer-file-name
    (user-error "The buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  buffer-file-name)

;;;###autoload
(defun xl-async-emacs-lisp-native-compile ()
  "Native-compile the current buffer's file in the background.
Saves the buffer first if it is modified.  Does nothing when the .eln is
already up to date."
  (interactive)
  (let ((file (xl-async-native--file)))
    (xl-async-native--compile
     file
     (lambda (eln) (message "xl-async: native-compiled %s" eln)))))

;;;###autoload
(defun xl-async-emacs-lisp-native-compile-and-load ()
  "Native-compile the current buffer's file in the background, then load it.
The file is loaded in this Emacs once the compilation has succeeded, so
the native code is what runs."
  (interactive)
  (let ((file (xl-async-native--file)))
    (xl-async-native--compile
     file
     (lambda (eln)
       ;; Load the .eln itself: `load' of the source would run it interpreted.
       (if (fboundp 'native-elisp-load)
           (native-elisp-load eln)
         (load eln nil t))
       (message "xl-async: native-compiled and loaded %s" eln)))))

(provide 'xl-async-native)
;;; xl-async-native.el ends here
