;;; xl-jit.el --- tiered native compilation: platform gate -*- lexical-binding: t; -*-
;; Design: emacs/ideas/xl-jit.md.  This file is loaded only by a background
;; batch run (see `xl-jit-platform-report' and emacs-setup.sh `jit'), never by
;; init.el.  Step 1 of the checklist: decide whether this Emacs binary can
;; compile native code at all, and remember the answer.

(require 'cl-lib)
(require 'comp nil t)

(defvar xl-jit-state-dir (expand-file-name "xl-jit/" user-emacs-directory)
  "Folder holding xl-jit state.  Tests rebind it to a temporary folder.")

(defconst xl-jit--max-state-bytes 65536
  "Largest state file xl-jit will read.  Bigger files are treated as absent.")

(defun xl-jit--binary-key ()
  "Identify the running Emacs binary by path, size and mtime.
A new binary changes the key, which makes the cached platform answer stale."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (true (or (ignore-errors (file-truename exe)) exe))
         (attrs (file-attributes true)))
    (list true
          (and attrs (file-attribute-size attrs))
          (and attrs (file-attribute-modification-time attrs)))))

(defun xl-jit--read-eld (file)
  "Read one Lisp form from FILE, or nil.  Only `read' is used, never `eval'."
  (when (and (file-regular-p file)
             (< (or (file-attribute-size (file-attributes file)) 0)
                xl-jit--max-state-bytes))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((form (let ((read-eval nil)) (read (current-buffer)))))
            (and (listp form) form)))
      (error nil))))

(defun xl-jit--write-eld (file form)
  "Write FORM to FILE atomically: a temporary file, then a rename."
  (make-directory (file-name-directory file) t)
  (let ((tmp (make-temp-name (concat file ".tmp"))))
    (with-temp-file tmp
      (let ((print-length nil) (print-level nil))
        (prin1 form (current-buffer))
        (insert "\n")))
    (rename-file tmp file t)))

(defun xl-jit--canary ()
  "Native-compile and load a one-function file in a temporary folder.
Return nil when it works, otherwise a string saying why not.  Nothing is
written to the eln cache."
  (let* ((dir (make-temp-file "xl-jit-canary-" t))
         (src (expand-file-name "xl-jit-canary.el" dir))
         (out (expand-file-name "xl-jit-canary.eln" dir)))
    (unwind-protect
        (condition-case err
            (progn
              (with-temp-file src
                (insert ";;; xl-jit-canary.el -*- lexical-binding: t; -*-\n"
                        "(defun xl-jit-canary--probe () 1)\n"
                        "(provide 'xl-jit-canary)\n"))
              (native-compile src out)
              (load out nil t)
              (if (and (fboundp 'xl-jit-canary--probe)
                       (native-comp-function-p (symbol-function 'xl-jit-canary--probe)))
                  nil
                "canary loaded but its function is not native"))
          (error (format "canary failed: %S" err)))
      (ignore-errors (delete-directory dir t)))))

(defun xl-jit--platform-file ()
  (expand-file-name "platform.eld" xl-jit-state-dir))

(defun xl-jit-platform-available-p ()
  "Non-nil when this Emacs binary can compile native code.
The answer is cached in `platform.eld' under the binary's key.  A failed
canary is cached too, so a broken libgccjit is not retried on every run."
  (let* ((key (xl-jit--binary-key))
         (cached (xl-jit--read-eld (xl-jit--platform-file))))
    (if (and cached (equal (plist-get cached :key) key))
        (plist-get cached :available)
      (let* ((reason (if (native-comp-available-p)
                         (xl-jit--canary)
                       "native compilation not available"))
             (available (null reason)))
        (xl-jit--write-eld (xl-jit--platform-file)
                           (list :key key :available available :reason reason))
        available))))

(defun xl-jit-run ()
  "One background run.  Return `unavailable' or `ready'.
Only the platform gate exists so far; later steps do the work after it."
  (if (xl-jit-platform-available-p) 'ready 'unavailable))

(defun xl-jit-platform-report ()
  "Print one line saying whether xl-jit can run here.  Used by emacs-setup.sh."
  (if (xl-jit-platform-available-p)
      (princ "xl-jit: native compilation available\n")
    (let ((reason (plist-get (xl-jit--read-eld (xl-jit--platform-file)) :reason)))
      (princ (format "xl-jit: native compilation not available, skipped (%s)\n"
                     (or reason "unknown"))))))

(provide 'xl-jit)
;;; xl-jit.el ends here
