;;; eln-opt-probe-trampolines.el --- predict subr trampolines -*- lexical-binding: t; -*-
;; Probe, not part of the config.  Run after init in a batch Emacs (see
;; README.md in this folder); it reads .elc files as data and never loads them.
;;
;; A trampoline is needed when a C primitive is advised or redefined
;; (advice-add, fset, cl-letf on symbol-function).  Uncached, each one is
;; compiled synchronously on the main thread, about 1 s.  Recall matters more
;; than precision: a spare trampoline costs one batch compile, a missing one
;; freezes the UI.
(require 'cl-lib)
(require 'seq)
(require 'comp)
(require 'byte-opt)

(defconst probe-redefiners '(advice-add fset defalias)
  "Calls whose first argument, when it names a primitive, needs a trampoline.
`defalias' is listed only to be skipped: (defalias \\='x #\\='subr) aliases.")

(defun probe--primitive-p (s)
  "Non-nil if S names a C primitive.  Looks through advice already on S."
  (and (symbolp s) (fboundp s)
       (subr-primitive-p (advice--cd*r (indirect-function s)))
       (not (memq s native-comp-never-optimize-functions))))

;; In LAP, (advice-add 'select-window ...) is: constant advice-add, constant
;; select-window, ...  cl-letf on symbol-function expands to the same with
;; fset.  `byte-decompile-bytecode' mixes integer offsets into its list, so
;; keep only the ops, or every offset breaks the adjacency.
(defun probe--scan-lap (code consts hits)
  (let ((prev nil))
    (dolist (op (seq-filter #'consp (ignore-errors (byte-decompile-bytecode code consts))))
      (let ((c (and (eq (car-safe op) 'byte-constant) (car-safe (cdr op)))))
        (when (and (memq prev probe-redefiners) (not (eq prev 'defalias))
                   (probe--primitive-p c))
          (cl-pushnew c (car hits)))
        (setq prev c))))
  (mapc (lambda (c) (when (byte-code-function-p c) (probe--scan-vector c hits)))
        consts))

(defun probe--scan-vector (fn hits)
  (probe--scan-lap (aref fn 1) (aref fn 2) hits))

(defun probe--scan-form (form hits)
  "Walk a top-level FORM read from an .elc file.
Top-level code is compiled to (byte-code STRING VECTOR DEPTH), so that
shape gets the same LAP scan as a function."
  (cond ((byte-code-function-p form) (probe--scan-vector form hits))
        ((and (eq (car-safe form) 'byte-code) (stringp (nth 1 form)) (vectorp (nth 2 form)))
         (probe--scan-lap (nth 1 form) (nth 2 form) hits))
        ((and (consp form) (memq (car form) '(advice-add fset add-function))
              (eq (car-safe (nth 1 form)) 'quote)
              (probe--primitive-p (cadr (nth 1 form))))
         (cl-pushnew (cadr (nth 1 form)) (car hits))
         (mapc (lambda (x) (probe--scan-form x hits)) (cdr form)))
        ((consp form)
         (while (consp form) (probe--scan-form (car form) hits) (setq form (cdr form))))
        ((vectorp form) (mapc (lambda (x) (probe--scan-form x hits)) form))))

(defun probe--scan-elc (file)
  "Primitives FILE redefines, read with `read' only."
  (let ((hits (list nil)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (probe--scan-form (read (current-buffer)) hits))
        (error nil)))
    (car hits)))

;; 1. Advised right now, after init (what cmd_trampolines covers today).
(defvar probe-now nil)
(mapatoms
 (lambda (s)
   (when (and (fboundp s) (advice--p (advice--symbol-function s))
              (subr-primitive-p (advice--cd*r (advice--symbol-function s))))
     (push s probe-now))))

;; 2. Static: every .elc on the load path, read as data (never loaded).
(defvar probe-static (make-hash-table :test #'eq))
(let ((files nil))
  (dolist (dir load-path)
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\.elc\\'"))
        (push f files))))
  (setq files (delete-dups files))
  (dolist (f files)
    (dolist (p (probe--scan-elc f))
      (push (file-name-base f) (gethash p probe-static))))
  (princ (format "scanned .elc files: %d\n" (length files))))

;; 3. Compare with the cache.
(defun probe--cached-p (s)
  (cl-some (lambda (dir)
             (file-exists-p (expand-file-name (comp-trampoline-filename s)
                                              (expand-file-name comp-native-version-dir dir))))
           native-comp-eln-load-path))

(princ (format "advised primitives now: %d  %S\n" (length probe-now) (sort probe-now #'string<)))
(let (rows)
  (maphash (lambda (p fs) (push (list p (probe--cached-p p) (delete-dups fs)) rows)) probe-static)
  (setq rows (sort rows (lambda (a b) (string< (car a) (car b)))))
  (princ (format "statically predicted: %d, cached: %d, missing: %d\n"
                 (length rows) (cl-count-if #'cadr rows) (cl-count-if-not #'cadr rows)))
  (princ "missing (primitive <- files):\n")
  (dolist (r rows)
    (unless (cadr r)
      (princ (format "  %-28s %s\n" (car r) (string-join (seq-take (nth 2 r) 4) " ")))))
  (princ "cached and predicted:\n")
  (dolist (r rows)
    (when (cadr r)
      (princ (format "  %-28s %s\n" (car r) (string-join (seq-take (nth 2 r) 4) " ")))))
  (princ (format "now-advised but not predicted: %S\n"
                 (cl-remove-if (lambda (s) (gethash s probe-static)) probe-now))))
