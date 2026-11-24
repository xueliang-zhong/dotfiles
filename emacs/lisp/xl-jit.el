;;; xl-jit.el --- tiered native compilation: platform gate -*- lexical-binding: t; -*-
;; Design: emacs/ideas/xl-jit.md.  This file is loaded only by a background
;; batch run (see `xl-jit-platform-report' and emacs-setup.sh `jit'), never by
;; init.el.  Step 1 of the checklist: decide whether this Emacs binary can
;; compile native code at all, and remember the answer.

(require 'cl-lib)
(require 'seq)
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

;;; ------------------------------------------------------------
;;; Step 2: trampoline prediction and build (see xl-jit.md, "Trampoline 预测")
;;; ------------------------------------------------------------

(defconst xl-jit-trampoline-limit 50
  "Most trampolines one run builds.  Each costs a batch compile, so this caps it.")

(defconst xl-jit--redefiners '(advice-add fset)
  "Calls whose first argument, when it names a primitive, needs a trampoline.
`defalias' is left out on purpose: (defalias 'x #'subr) only aliases.")

(defun xl-jit--primitive-p (s)
  "Non-nil if S names a C primitive, looking through advice already on it."
  (and (symbolp s) s (fboundp s)
       (subr-primitive-p (advice--cd*r (indirect-function s)))
       (not (memq s native-comp-never-optimize-functions))))

(defun xl-jit--scan-lap (code consts hits)
  "Add to HITS (a cons whose car is a list) the primitives redefined in LAP.
Two shapes count.  A call such as (advice-add \='x ...) pushes its function
name and then the symbol, so the symbol follows the name directly.  The
opcode `byte-fset' (what `cl-letf' on `symbol-function' compiles to) takes
its symbol from the constants pushed just before its value, so every
primitive among the last three constants counts.  Some extra names are
harmless: a spare trampoline costs one batch compile.  `byte-decompile-bytecode'
mixes integer offsets into its list, so only conses are kept."
  (let ((prev nil) (recent nil))
    (dolist (op (seq-filter #'consp
                            (ignore-errors (byte-decompile-bytecode code consts))))
      (let* ((head (car op))
             (c (and (eq head 'byte-constant) (car-safe (cdr op)))))
        (when (and (memq prev xl-jit--redefiners) (xl-jit--primitive-p c))
          (cl-pushnew c (car hits)))
        (when (eq head 'byte-fset)
          (dolist (r recent)
            (when (xl-jit--primitive-p r) (cl-pushnew r (car hits)))))
        (setq prev c
              recent (cons c (seq-take recent 2))))))
  (mapc (lambda (c) (when (byte-code-function-p c) (xl-jit--scan-fn c hits)))
        consts))

(defun xl-jit--scan-fn (fn hits)
  (xl-jit--scan-lap (aref fn 1) (aref fn 2) hits))

(defun xl-jit--scan-form (form hits)
  "Walk FORM read from an .elc.  Nothing here is evaluated."
  (cond ((byte-code-function-p form) (xl-jit--scan-fn form hits))
        ((and (eq (car-safe form) 'byte-code) (stringp (nth 1 form))
              (vectorp (nth 2 form)))
         ;; Top-level code in an .elc is (byte-code CODE CONSTS DEPTH).
         (xl-jit--scan-lap (nth 1 form) (nth 2 form) hits))
        ((and (consp form) (memq (car form) '(advice-add fset add-function))
              (eq (car-safe (nth 1 form)) 'quote)
              (xl-jit--primitive-p (cadr (nth 1 form))))
         (cl-pushnew (cadr (nth 1 form)) (car hits)))
        ((consp form)
         (while (consp form)
           (xl-jit--scan-form (car form) hits)
           (setq form (cdr form))))
        ((vectorp form)
         (mapc (lambda (x) (xl-jit--scan-form x hits)) form))))

(defun xl-jit--scan-elc (file)
  "Primitives FILE redefines.  FILE is read as data, never loaded."
  (let ((hits (list nil)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((read-eval nil))
        (condition-case nil
            (while t (xl-jit--scan-form (read (current-buffer)) hits))
          (error nil))))
    (car hits)))

(defun xl-jit--elc-files ()
  "Every .elc in `load-path', each file once."
  (let (files)
    (dolist (dir load-path)
      (when (and (stringp dir) (file-directory-p dir))
        (dolist (f (directory-files dir t "\\.elc\\'"))
          (push f files))))
    (delete-dups files)))

(defun xl-jit-trampolines-advised ()
  "Primitives advised right now, after init has run."
  (let (out)
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (advice--p (advice--symbol-function s))
                  (xl-jit--primitive-p s))
         (push s out))))
    out))

(defun xl-jit-trampolines-predict ()
  "Primitives that will need a trampoline: found by scanning .elc files, plus
those advised right now.  Recall matters more than precision here: a spare
trampoline costs one batch compile, a missing one freezes the UI."
  (let ((hits nil))
    (dolist (f (xl-jit--elc-files))
      (dolist (p (xl-jit--scan-elc f))
        (cl-pushnew p hits)))
    (dolist (p (xl-jit-trampolines-advised))
      (cl-pushnew p hits))
    (sort hits (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun xl-jit--trampoline-cached-p (s)
  "Non-nil if a trampoline for S is already in one of the eln-cache folders."
  (let ((name (comp-trampoline-filename s)))
    (cl-some (lambda (dir)
               (file-exists-p
                (expand-file-name name (expand-file-name comp-native-version-dir dir))))
             native-comp-eln-load-path)))

(defun xl-jit-trampolines-missing ()
  "Predicted primitives whose trampoline is not cached yet."
  (cl-remove-if #'xl-jit--trampoline-cached-p (xl-jit-trampolines-predict)))

(defun xl-jit-trampolines-build ()
  "Build the missing trampolines, at most `xl-jit-trampoline-limit' of them.
Returns `unavailable' on a platform without native compilation, otherwise a
plist (:built N :failed N :left N).  A failure is counted and skipped; it is
not retried within the same run."
  (if (not (xl-jit-platform-available-p))
      'unavailable
    (let* ((missing (xl-jit-trampolines-missing))
           (todo (seq-take missing xl-jit-trampoline-limit))
           (built 0) (failed 0))
      (dolist (s todo)
        (condition-case nil
            (progn (comp-trampoline-compile s) (cl-incf built))
          (error (cl-incf failed))))
      (list :built built :failed failed
            :left (- (length missing) (length todo))))))

(provide 'xl-jit)
;;; xl-jit.el ends here
