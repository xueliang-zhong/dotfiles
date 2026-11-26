;;; eln-opt.el --- tiered native compilation for Emacs, in the background -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Xueliang Zhong

;; Author: Xueliang Zhong <xueliang.zhong@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, compile, performance

;;; Commentary:

;; eln-opt native-compiles the code you use most, ahead of time, in one
;; background worker, without turning on Emacs's global JIT.  The running
;; Emacs profiles nothing and adds no hooks; it arms one idle timer
;; (`eln-opt-auto-arm'):
;;
;; - After Emacs has been idle for `eln-opt-auto-idle' seconds, it starts one
;;   worker: a batch Emacs with this profile.  A run that compiled something
;;   arms the idle timer again, up to `eln-opt-auto-max-runs' runs a session.
;; - The worker ranks files by reachability from what you use (savehist,
;;   recentf, hooks and keymaps, read from byte code), keeps the candidates
;;   that have no usable eln yet and have not failed before, and compiles them
;;   one at a time, `eln-opt-worker-sleep' seconds apart.  The first error
;;   stops it; the failing file is skipped until its source changes.
;; - Emacs itself loads the new elns on the next start.
;; - `eln-opt-report' shows the ranking, what is native, and why each file
;;   is or is not compiled; `eln-opt-compile-now' starts the worker at once.
;;
;; Files that run interpreted (loaded from .el: no .elc, or a stale one) are
;; byte-compiled first, on every platform.  Without native compilation (no
;; libgccjit, as on most Windows builds) that is all the worker does.
;; The design notes are in tuned-emacs/ideas/eln-opt.md of the dotfiles repository.
;;
;; Requires xl-async (which holds the loader too) for the views.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(declare-function comp-delete-or-replace-file "comp" (file))
(require 'seq)
;; comp.el is loaded only where it is used (see `eln-opt--require-comp'): at
;; startup it costs about 170 ms on a Raspberry Pi, and the timer path never needs it.
(require 'xl-async)
(require 'xl-perfetto-trace)     ; spans for the worker's phases; off unless recording

;; Design: tuned-emacs/ideas/eln-opt.md.  This file is loaded only by a background
;; batch run (see `eln-opt-platform-report' and emacs-setup.sh `jit'), never by
;; init.el.  Step 1 of the checklist: decide whether this Emacs binary can
;; compile native code at all, and remember the answer.

(defvar eln-opt-state-dir (expand-file-name "eln-opt/" user-emacs-directory)
  "Folder holding eln-opt state.  Tests rebind it to a temporary folder.")

(defconst eln-opt--max-state-bytes 65536
  "Largest state file eln-opt will read.  Bigger files are treated as absent.")

(defun eln-opt--binary-key ()
  "Identify the running Emacs binary by path, size and mtime.
A new binary changes the key, which makes the cached platform answer stale."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (true (or (ignore-errors (file-truename exe)) exe))
         (attrs (file-attributes true)))
    (list true
          (and attrs (file-attribute-size attrs))
          (and attrs (file-attribute-modification-time attrs)))))

(defun eln-opt--read-eld (file &optional max-bytes)
  "Read one Lisp form from FILE, or nil.  Only `read' is used, never `eval'.
Files larger than MAX-BYTES (default `eln-opt--max-state-bytes') are ignored."
  (when (and (file-regular-p file)
             (< (or (file-attribute-size (file-attributes file)) 0)
                (or max-bytes eln-opt--max-state-bytes)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((form (let ((read-eval nil)) (read (current-buffer)))))
            (and (listp form) form)))
      (error nil))))

(defun eln-opt--write-eld (file form)
  "Write FORM to FILE atomically: a temporary file, then a rename."
  (make-directory (file-name-directory file) t)
  (let ((tmp (make-temp-name (concat file ".tmp"))))
    (with-temp-file tmp
      (let ((print-length nil) (print-level nil))
        (prin1 form (current-buffer))
        (insert "\n")))
    (rename-file tmp file t)))

(defun eln-opt--require-comp ()
  "Load comp.el, for the functions that need it.  Without it, the startup path
stays cheap; the variables and `comp-el-to-eln-filename' are preloaded anyway."
  (require 'comp nil t))

(defun eln-opt--canary ()
  "Native-compile and load a one-function file in a temporary folder.
Return nil when it works, otherwise a string saying why not.  Nothing is
written to the eln cache."
  (let* ((dir (make-temp-file "eln-opt-canary-" t))
         (src (expand-file-name "eln-opt-canary.el" dir))
         (out (expand-file-name "eln-opt-canary.eln" dir)))
    (unwind-protect
        (condition-case err
            (progn
              (with-temp-file src
                (insert ";;; eln-opt-canary.el -*- lexical-binding: t; -*-\n"
                        "(defun eln-opt-canary--probe () 1)\n"
                        "(provide 'eln-opt-canary)\n"))
              (native-compile src out)
              (load out nil t)
              (if (and (fboundp 'eln-opt-canary--probe)
                       (native-comp-function-p (symbol-function 'eln-opt-canary--probe)))
                  nil
                "canary loaded but its function is not native"))
          (error (format "canary failed: %S" err)))
      (ignore-errors (delete-directory dir t)))))

(defun eln-opt--platform-file ()
  (expand-file-name "platform.eld" eln-opt-state-dir))

(defun eln-opt-platform-available-p ()
  "Non-nil when this Emacs binary can compile native code.
The answer is cached in `platform.eld' under the binary's key.  A failed
canary is cached too, so a broken libgccjit is not retried on every run."
  (let* ((key (eln-opt--binary-key))
         (cached (eln-opt--read-eld (eln-opt--platform-file))))
    (if (and cached (equal (plist-get cached :key) key))
        (plist-get cached :available)
      (let* ((reason (if (native-comp-available-p)
                         (eln-opt--canary)
                       "native compilation not available"))
             (available (null reason)))
        (eln-opt--write-eld (eln-opt--platform-file)
                           (list :key key :available available :reason reason))
        available))))

(defun eln-opt-platform-report ()
  "Print one line saying whether eln-opt can run here.  Used by emacs-setup.sh."
  (if (eln-opt-platform-available-p)
      (princ "eln-opt: native compilation available\n")
    (let ((reason (plist-get (eln-opt--read-eld (eln-opt--platform-file)) :reason)))
      (princ (format "eln-opt: native compilation not available, byte-compile only (%s)\n"
                     (or reason "unknown"))))))

;;; ------------------------------------------------------------
;;; Step 2: trampoline prediction and build (see eln-opt.md, "Trampoline 预测")
;;; ------------------------------------------------------------

(defconst eln-opt-trampoline-limit 50
  "Most trampolines one run builds.  Each costs a batch compile, so this caps it.")

(defconst eln-opt--redefiners '(advice-add fset)
  "Calls whose first argument, when it names a primitive, needs a trampoline.
`defalias\=' is left out on purpose: (defalias \='x #\='subr) only aliases.")

(defun eln-opt--primitive-p (s)
  "Non-nil if S names a C primitive, looking through advice already on it."
  (and (symbolp s) s (fboundp s)
       (subr-primitive-p (advice--cd*r (indirect-function s)))
       (not (memq s (bound-and-true-p native-comp-never-optimize-functions)))))

(defun eln-opt--scan-lap (code consts hits)
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
                            (progn (require 'byte-opt)
                                   (ignore-errors (byte-decompile-bytecode code consts)))))
      (let* ((head (car op))
             (c (and (eq head 'byte-constant) (car-safe (cdr op)))))
        (when (and (memq prev eln-opt--redefiners) (eln-opt--primitive-p c))
          (cl-pushnew c (car hits)))
        (when (eq head 'byte-fset)
          (dolist (r recent)
            (when (eln-opt--primitive-p r) (cl-pushnew r (car hits)))))
        (setq prev c
              recent (cons c (seq-take recent 2))))))
  (mapc (lambda (c) (when (byte-code-function-p c) (eln-opt--scan-fn c hits)))
        consts))

(defun eln-opt--scan-fn (fn hits)
  (eln-opt--scan-lap (aref fn 1) (aref fn 2) hits))

(defun eln-opt--scan-form (form hits)
  "Walk FORM read from an .elc.  Nothing here is evaluated."
  (cond ((byte-code-function-p form) (eln-opt--scan-fn form hits))
        ((and (eq (car-safe form) 'byte-code) (stringp (nth 1 form))
              (vectorp (nth 2 form)))
         ;; Top-level code in an .elc is (byte-code CODE CONSTS DEPTH).
         (eln-opt--scan-lap (nth 1 form) (nth 2 form) hits))
        ((and (consp form) (memq (car form) '(advice-add fset add-function))
              (eq (car-safe (nth 1 form)) 'quote)
              (eln-opt--primitive-p (cadr (nth 1 form))))
         (cl-pushnew (cadr (nth 1 form)) (car hits)))
        ((consp form)
         (while (consp form)
           (eln-opt--scan-form (car form) hits)
           (setq form (cdr form))))
        ((vectorp form)
         (mapc (lambda (x) (eln-opt--scan-form x hits)) form))))

(defun eln-opt--scan-elc (file)
  "Primitives FILE redefines.  FILE is read as data, never loaded."
  (let ((hits (list nil)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((read-eval nil))
        (condition-case nil
            (while t (eln-opt--scan-form (read (current-buffer)) hits))
          (error nil))))
    (car hits)))

(defun eln-opt--elc-files ()
  "Every .elc in `load-path', each file once."
  (let (files)
    (dolist (dir load-path)
      (when (and (stringp dir) (file-directory-p dir))
        (dolist (f (directory-files dir t "\\.elc\\'"))
          (push f files))))
    (delete-dups files)))

(defun eln-opt-trampolines-advised ()
  "Primitives advised right now, after init has run."
  (let (out)
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (advice--p (advice--symbol-function s))
                  (eln-opt--primitive-p s))
         (push s out))))
    out))

(defun eln-opt-trampolines-predict ()
  "Primitives that will need a trampoline: found by scanning .elc files, plus
those advised right now.  Recall matters more than precision here: a spare
trampoline costs one batch compile, a missing one freezes the UI."
  (let ((hits nil))
    (dolist (f (eln-opt--elc-files))
      (dolist (p (eln-opt--scan-elc f))
        (cl-pushnew p hits)))
    (dolist (p (eln-opt-trampolines-advised))
      (cl-pushnew p hits))
    (sort hits (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun eln-opt--trampoline-cached-p (s)
  "Non-nil if a trampoline for S is already in one of the eln-cache folders.
Without comp (no native compilation), nothing is cached: there are no elns."
  (and (fboundp 'comp-trampoline-filename) (eln-opt--eln-dir-p)
       (let ((name (comp-trampoline-filename s)))
         (cl-some (lambda (dir)
                    (file-exists-p
                     (expand-file-name name (expand-file-name (eln-opt--version-name) dir))))
                  native-comp-eln-load-path))))

(defun eln-opt-trampolines-missing ()
  "Predicted primitives whose trampoline is not cached yet."
  (eln-opt--require-comp)
  (cl-remove-if #'eln-opt--trampoline-cached-p (eln-opt-trampolines-predict)))

(defun eln-opt-trampolines-build ()
  "Build the missing trampolines, at most `eln-opt-trampoline-limit' of them.
Returns `unavailable' on a platform without native compilation, otherwise a
plist (:built N :failed N :left N).  A failure is counted and skipped; it is
not retried within the same run."
  (eln-opt--require-comp)
  (if (not (eln-opt-platform-available-p))
      'unavailable
    (let* ((missing (eln-opt-trampolines-missing))
           (todo (seq-take missing eln-opt-trampoline-limit))
           (built 0) (failed 0))
      (dolist (s todo)
        (condition-case nil
            (progn (comp-trampoline-compile s) (cl-incf built))
          (error (cl-incf failed))))
      (list :built built :failed failed
            :left (- (length missing) (length todo))))))

;;; ------------------------------------------------------------
;;; State: the build folder, a lock, and the files that failed
;;; ------------------------------------------------------------

(defconst eln-opt-lock-max-age 3600
  "Seconds after which a lock counts as stale even if its owner still exists.")

(defun eln-opt--now () (float-time))

(defun eln-opt--version-name ()
  "This build's version folder name: `comp-native-version-dir' when comp is
loaded.  Without comp (no native compilation, as on many Windows builds) the
variable is void, so the name is made from the binary's key instead; it only
needs to be stable for one binary."
  (if (boundp 'comp-native-version-dir)
      comp-native-version-dir
    (format "%d_%d-%s" emacs-major-version emacs-minor-version
            (substring (md5 (format "%S" (eln-opt--binary-key))) 0 8))))

(defun eln-opt--version-dir ()
  "State folder for this Emacs build.  Ledgers never mix builds."
  (expand-file-name (eln-opt--version-name) eln-opt-state-dir))
(defun eln-opt--lock-dir () (expand-file-name "lock" (eln-opt--version-dir)))

(defun eln-opt--file-size (file)
  (let ((attrs (file-attributes file)))
    (and attrs (file-attribute-size attrs))))

(defun eln-opt--short (text)
  "The part of TEXT that says what went wrong, at most 300 characters."
  (let* ((start (or (string-match "Error:" text) 0))
         (piece (string-trim (substring text start))))
    (if (> (length piece) 300) (substring piece 0 300) piece)))

;;; Lock: taken by making a directory, which is atomic.  Never waits.

(defun eln-opt--owner-file (dir) (expand-file-name "owner.eld" dir))

(defun eln-opt--process-alive-p (pid)
  "Non-nil if PID is a running process.  A zombie (state Z) has exited: its
parent has not reaped it yet, but it no longer holds anything, so it does not
keep a lock alive."
  (let* ((attrs (process-attributes pid))
         (state (cdr (assq 'state attrs))))
    (and attrs (not (string-prefix-p "Z" (format "%s" state))))))

(defun eln-opt--lock-stale-p (dir)
  (let* ((owner (eln-opt--read-eld (eln-opt--owner-file dir)))
         (pid (plist-get owner :pid))
         (time (plist-get owner :time)))
    (or (null owner)
        (not (and (integerp pid) (eln-opt--process-alive-p pid)))
        (and time (> (- (eln-opt--now) time) eln-opt-lock-max-age)))))

(defun eln-opt--lock-take (dir)
  "Try to create DIR as the lock.  Return non-nil on success."
  (condition-case nil
      (progn
        (make-directory dir)
        (eln-opt--write-eld (eln-opt--owner-file dir)
                           (list :pid (emacs-pid) :time (eln-opt--now)))
        t)
    (file-already-exists nil)))

(defun eln-opt--sweep-stale-locks ()
  "Delete the lock.stale-* folders left by earlier takeovers.  Only call this
while holding the lock: a renamed stale lock is in use by nobody then."
  (dolist (d (directory-files (eln-opt--version-dir) t "\\`lock\\.stale-" t))
    (ignore-errors (delete-directory d t))))

(defun eln-opt--lock-acquire ()
  "Return non-nil if this process now holds the lock.  A stale lock is
renamed away first, so two runners cannot both take over the same one.
Once the lock is ours, the renamed folders are swept away."
  (let* ((dir (eln-opt--lock-dir))
         (got
          (progn
            (make-directory (eln-opt--version-dir) t)
            (or (eln-opt--lock-take dir)
                (and (eln-opt--lock-stale-p dir)
                     (condition-case nil
                         (progn (rename-file dir (make-temp-name (concat dir ".stale-")))
                                (eln-opt--lock-take dir))
                       (file-error nil)))))))
    (when got (eln-opt--sweep-stale-locks))
    got))

(defun eln-opt--lock-release ()
  "Remove the lock, but only if this process owns it."
  (let ((dir (eln-opt--lock-dir)))
    (when (equal (plist-get (eln-opt--read-eld (eln-opt--owner-file dir)) :pid)
                 (emacs-pid))
      (ignore-errors (delete-directory dir t)))))


;;; The failed list: one entry per source that failed to compile.  It is
;;; skipped until its content changes, so one bad file cannot stop every run.

(defconst eln-opt--failed-max-bytes (* 256 1024)
  "Largest failed list eln-opt will read.")

(defun eln-opt--failed-file ()
  (expand-file-name "failed.eld" (eln-opt--version-dir)))

(defun eln-opt--content-md5 (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (md5 (current-buffer))))

(defun eln-opt--failed-load ()
  "The failed list: plists with :src, :md5, :reason and :time."
  (seq-filter (lambda (e) (and (consp e) (stringp (plist-get e :src))))
              (eln-opt--read-eld (eln-opt--failed-file) eln-opt--failed-max-bytes)))

(defun eln-opt--failed-p (src)
  "Non-nil if SRC failed before and has not changed since."
  (let ((true (file-truename src)))
    (and (file-readable-p true)
         (let ((md5 (eln-opt--content-md5 true)))
           (seq-find (lambda (e) (and (equal (plist-get e :src) true)
                                      (equal (plist-get e :md5) md5)))
                     (eln-opt--failed-load))))))

(defun eln-opt--record-failure (src reason)
  "Put SRC on the failed list with REASON, replacing an older entry for it."
  (let ((true (file-truename src)))
    (eln-opt--write-eld
     (eln-opt--failed-file)
     (cons (list :src true
                 :md5 (and (file-readable-p true) (eln-opt--content-md5 true))
                 :reason (eln-opt--short (format "%s" reason))
                 :time (eln-opt--now))
           (seq-remove (lambda (e) (equal (plist-get e :src) true))
                       (eln-opt--failed-load))))))

;;; Built: the eln Emacs will load for a source.  Emacs looks the eln up by
;;; the source's path and content, and ignores it when the .elc is newer.

(defun eln-opt--eln-dir-p ()
  "Non-nil when this Emacs has an eln folder: comp is loaded."
  (and (boundp 'native-comp-eln-load-path) native-comp-eln-load-path
       (boundp 'comp-native-version-dir)))

(defun eln-opt--eln-dir ()
  "The folder where this Emacs writes and finds the user's elns, or nil when
this Emacs has no eln folder (no comp)."
  (when (eln-opt--eln-dir-p)
    (expand-file-name (eln-opt--version-name) (car native-comp-eln-load-path))))

(defun eln-opt--eln-for (src)
  (ignore-errors (comp-el-to-eln-filename src)))

(defun eln-opt--source-for (file)
  "The source Emacs pairs with FILE, an .elc or a source: the .el if it
exists, else the .el.gz (Emacs ships only that for its own libraries), else
the .el path.  Loading an .elc without an .el, Emacs also names the eln after
the .el.gz, so compiling the .el.gz gives the eln Emacs looks for."
  (let ((el (replace-regexp-in-string
             "\\.elc\\'" ".el" (string-remove-suffix ".gz" file))))
    (cond ((file-exists-p el) el)
          ((file-exists-p (concat el ".gz")) (concat el ".gz"))
          (t el))))

(defun eln-opt--elc-for (src)
  "The .elc next to SRC, an .el or .el.gz."
  (concat (string-remove-suffix ".gz" src) "c"))

(defun eln-opt--source-bytes (src)
  "SRC's size in bytes, uncompressed when SRC is an .el.gz."
  (if (string-suffix-p ".gz" src)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (insert-file-contents src))
        (buffer-size))
    (eln-opt--file-size src)))

(defun eln-opt--built-p (src)
  "Non-nil if SRC has an eln that Emacs will use: present, and not older than
the .elc next to SRC."
  (let ((eln (eln-opt--eln-for src))
        (elc (eln-opt--elc-for src)))
    (and eln (file-exists-p eln)
         (not (and (file-exists-p elc) (file-newer-than-file-p elc eln))))))

(defun eln-opt--built-sources ()
  "(SOURCE . ELN) for every loaded file whose eln, in the user's eln folder,
Emacs will use.  Emacs's own preloaded native code is not in that folder."
  (let ((dir (let ((d (eln-opt--eln-dir))) (and d (file-name-as-directory d))))
        out)
    (dolist (entry load-history)
      (let ((file (car entry)))
        (when (and dir (stringp file) (string-suffix-p ".elc" file))
          (let* ((src (eln-opt--source-for file))
                 (eln (and (file-exists-p src) (eln-opt--eln-for src))))
            (when (and eln (string-prefix-p dir eln) (eln-opt--built-p src))
              (push (cons src eln) out))))))
    (sort (delete-dups out) (lambda (a b) (string< (car a) (car b))))))

(defun eln-opt--user-elns ()
  "Every eln in the user's eln folder, except the subr trampolines."
  (let ((dir (eln-opt--eln-dir)))
    (and dir (file-directory-p dir)
         (seq-remove (lambda (f) (string-prefix-p "subr--trampoline-" (file-name-nondirectory f)))
                     (directory-files dir t "\\.eln\\'")))))

;;; ------------------------------------------------------------
;;; Candidates: which ranked files may be compiled
;;; ------------------------------------------------------------

(defconst eln-opt-min-ratio 3.0
  "Lowest score/KB a file needs to be a candidate.  Filters only; the order of
the candidates is by score.  Uncalibrated: set it from a profiler run.")

(defconst eln-opt-max-source-bytes (* 1024 1024)
  "Largest source file eln-opt will compile, uncompressed.  1 MB lets in
org.el (about 870 KB), which compiles in about a minute.")

(defconst eln-opt-worker-max 30
  "Most files one worker run compiles.")

(defconst eln-opt-worker-sleep 5
  "Seconds the worker sleeps between two compiles, to stay in the background.")

(defconst eln-opt-worker-stall 600
  "Seconds without output after which the main Emacs kills the worker and puts
the file it was compiling on the failed list.")

(defun eln-opt--profile-dir ()
  "The profile this run belongs to: the batch's user-emacs-directory."
  (file-name-as-directory user-emacs-directory))

(defun eln-opt--candidate-reason (row)
  "Why ranking ROW is not a candidate, as a string; nil if it is one.
Checked again here, not trusted from the ranking: the source is re-tested
against the trusted roots and the opt-out cookie, and must be a reasonable size."
  (let* ((path (plist-get row :path))
         (src (and path (eln-opt--source-for path))))
    (cond
     ((null path) "no source path recorded")
     ((null (plist-get row :ratio)) "no ratio")
     ((< (plist-get row :ratio) eln-opt-min-ratio)
      (format "ratio %.1f is below %.1f" (plist-get row :ratio) eln-opt-min-ratio))
     ((not (file-readable-p src))
      (if (string-suffix-p ".elc" path) "no .el or .el.gz source next to the .elc"
        "source not readable"))
     ((not (eln-opt-trusted-file-p src)) "outside the trusted roots")
     ((eln-opt-rank--no-native-p src) "opts out with no-native-compile")
     ((> (or (eln-opt--source-bytes src) 0) eln-opt-max-source-bytes)
      (format "source is %d KB, over the %d KB limit"
              (/ (eln-opt--source-bytes src) 1024) (/ eln-opt-max-source-bytes 1024)))
     (t nil))))

(defun eln-opt--candidate-p (row)
  "Non-nil if ranking ROW may be compiled."
  (null (eln-opt--candidate-reason row)))

;;; Trusted roots: a file is compiled (or ranked for compiling) only if its
;;; truename lies under one of these.  Used by the report and by step 6.

(defun eln-opt-trusted-roots ()
  "Folders and files whose lisp may be compiled: elpa, Emacs's own lisp,
our lisp/ folder, and our xueliang.el.  A root ending in a slash is a folder."
  (delq nil
        (list (and (boundp 'package-user-dir) (file-name-as-directory package-user-dir))
              (and (boundp 'lisp-directory) (file-name-as-directory lisp-directory))
              (and (bound-and-true-p xl-dotfiles)
                   (file-name-as-directory (expand-file-name "tuned-emacs/lisp" xl-dotfiles)))
              (and (bound-and-true-p xl-dotfiles)
                   (expand-file-name "tuned-emacs/xueliang.el" xl-dotfiles)))))

(defun eln-opt-trusted-file-p (file)
  "Non-nil if FILE's truename is under a trusted root.  A symlink that leaves
the roots is refused, because its truename is what gets compiled."
  (let ((true (ignore-errors (file-truename
                              ;; load-history names the .elc once compiled.
                              (eln-opt--source-for file)))))
    (and true
         (cl-some (lambda (root)
                    (let ((r (ignore-errors (file-truename root))))
                      (and r (if (string-suffix-p "/" root)
                                 (string-prefix-p r true)
                               (string= r true)))))
                  (eln-opt-trusted-roots)))))

;;; Query: which functions are native-compiled in this Emacs

(defun eln-opt--source-stem (file)
  "FILE's truename without .elc, .el or .el.gz, for matching a loaded file to
a source."
  (file-name-sans-extension
   (string-remove-suffix ".gz" (or (ignore-errors (file-truename file)) file))))

(defun eln-opt--defun-files ()
  "A hash from function symbol to the file `symbol-file' names for it, built
in one pass over `load-history'.  `symbol-file' scans the whole history for
each call, which took seconds on a Raspberry Pi with thousands of native
functions.  The first entry that names a function wins, as in `symbol-file'.
It agrees with `symbol-file' for native functions, which is all that
`eln-opt-native-list' asks; autoloaded functions are not tracked here."
  (let ((files (make-hash-table :test #'eq)))
    (dolist (entry load-history)
      (when (stringp (car entry))
        (dolist (item (cdr entry))
          (let ((sym (and (consp item) (eq (car item) 'defun) (cdr item))))
            (when (and sym (not (gethash sym files)))
              (puthash sym (car entry) files))))))
    files))

(defun eln-opt-native-list ()
  "Natively compiled functions in this Emacs, grouped by source file.
Returns a plist:
  :built    the files eln-opt built, from the ledger, as (SOURCE . ELN)
  :files    alist (FILE . SYMBOLS) for every file that has a native function,
            FILE being what `symbol-file' reports; sorted by file name
  :total    how many functions are native in all
Emacs's own preloaded native code is in :files too, so `:total' is large."
  (let ((built (eln-opt--built-sources))
        (groups (make-hash-table :test #'equal))
        (defun-files (eln-opt--defun-files))
        (total 0))
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (not (macrop s))
                  (ignore-errors (native-comp-function-p (indirect-function s))))
         (cl-incf total)
         (let ((file (or (gethash s defun-files) "(no file)")))
           (push s (gethash file groups))))))
    (let (files)
      (maphash (lambda (f syms) (push (cons f (sort syms #'string<)) files)) groups)
      (list :built built
            :files (sort files (lambda (a b) (string< (car a) (car b))))
            :total total))))

;;; ---- Report: the ranking, the built files, the skips ----

;; Step 5 of tuned-emacs/ideas/eln-opt.md.  Nothing here compiles anything.  A child
;; Emacs with the profile loaded reads the byte code, the call graph, and the
;; usage records (savehist, recentf) as data, and prints a ranking.  Entry
;; points: `eln-opt-report' (M-x), and `emacs-setup.sh eln-opt-report'.

;;; ------------------------------------------------------------
;;; Settings.  The weights are starting values; calibrate them against a
;;; profiler run before trusting the order (eln-opt.md, "校准").
;;; ------------------------------------------------------------

(defconst eln-opt-rank-heat-levels 3
  "How many call levels heat spreads through.")

(defconst eln-opt-rank-spread 0.5
  "Share of a caller's heat passed to each callee (divided by sqrt of callees).")

(defconst eln-opt-rank-loop-factor 4.0
  "Weight of code inside a loop, per nesting level.  A recursive function
counts as one level deeper.")

(defconst eln-opt-rank-savehist-cap 1.0
  "Heat of the most recent savehist command.  Older commands decay, and no
command can weigh more than this, so one habit cannot dominate.")

(defconst eln-opt-rank-always-heat 5.0
  "Heat of hooks that run on every keystroke or redisplay.")

(defconst eln-opt-rank-baseline-users 50
  "When this many savehist commands and recentf files exist, the baseline is
gone.  Below it the baseline's weight falls in step with the user's data.")

(defconst eln-opt-rank--inline-ops
  '(byte-car byte-cdr byte-cons byte-eq byte-plus byte-diff byte-add1 byte-sub1
    byte-lss byte-gtr byte-leq byte-geq byte-eqlsign byte-nth byte-nthcdr
    byte-setcar byte-setcdr byte-aref byte-aset byte-car-safe byte-cdr-safe
    byte-memq byte-consp byte-listp byte-not byte-mult byte-max byte-min
    byte-symbolp byte-stringp byte-integerp byte-numberp byte-length byte-elt
    byte-nreverse)
  "Opcodes native code runs much faster than byte code.  Count them.")

(defconst eln-opt-rank--buffer-ops
  '(byte-insert byte-goto-char byte-forward-line byte-point byte-char-after
    byte-following-char byte-preceding-char byte-buffer-substring
    byte-delete-region byte-skip-chars-forward byte-skip-chars-backward
    byte-forward-char byte-forward-word byte-insertN byte-point-max byte-point-min
    byte-set-buffer byte-save-excursion byte-save-restriction byte-narrow-to-region)
  "Opcodes whose time is spent in C.  Native code changes little.")

(defconst eln-opt-rank--c-heavy
  '(insert insert-before-markers re-search-forward re-search-backward
    search-forward search-backward looking-at looking-back string-match
    replace-match replace-regexp-in-string buffer-substring
    buffer-substring-no-properties put-text-property add-text-properties
    propertize all-completions try-completion completion-all-completions
    sort json-parse-string json-parse-buffer read format message
    call-process make-process process-send-string delete-region
    overlay-put make-overlay font-lock-ensure)
  "Functions called from Lisp whose time is spent in C.")

(defconst eln-opt-rank--alloc-heavy
  '(substring concat format split-string string-join string-trim
    make-string number-to-string mapconcat)
  "Calls that allocate.  Measured: native code gains little where they dominate
(eln-opt.md, \"实测\"), because time goes to allocation and GC.")

(defconst eln-opt-rank--dynamic-callers
  '(funcall apply run-hooks run-hook-with-args run-hook-with-args-until-success
    run-hook-with-args-until-failure run-hook-wrapped mapcar mapc mapcan
    seq-map seq-filter seq-do cl-mapcar))

;;; ------------------------------------------------------------
;;; Byte-code features of one function (nested closures included)
;;; ------------------------------------------------------------

(defconst eln-opt-rank--op-class
  (let ((h (make-hash-table :test #'eq)))
    ;; The same order as the old cond: inline, then buffer.  First one wins.
    (dolist (op eln-opt-rank--inline-ops) (unless (gethash op h) (puthash op 'inline h)))
    (dolist (op eln-opt-rank--buffer-ops) (unless (gethash op h) (puthash op 'buffer h)))
    h)
  "Instruction name to its benefit class, `inline' or `buffer'.")

(defconst eln-opt-rank--const-class
  (let ((h (make-hash-table :test #'eq)))
    ;; The same order as the old cond: C-heavy, allocating, then dynamic.
    (dolist (c eln-opt-rank--c-heavy) (unless (gethash c h) (puthash c 'c-heavy h)))
    (dolist (c eln-opt-rank--alloc-heavy) (unless (gethash c h) (puthash c 'alloc h)))
    (dolist (c eln-opt-rank--dynamic-callers) (unless (gethash c h) (puthash c 'dynamic h)))
    h)
  "Called function name to its class, `c-heavy', `alloc' or `dynamic'.")

(defvar eln-opt-rank--weight-cache (make-vector 64 nil)
  "Memo of `eln-opt-rank--weight', by loop level.")

(defun eln-opt-rank--weight (level)
  "The weight of code at LEVEL loop levels deep: the loop factor to that power."
  (if (< level 64)
      (or (aref eln-opt-rank--weight-cache level)
          (aset eln-opt-rank--weight-cache level
                (expt eln-opt-rank-loop-factor level)))
    (expt eln-opt-rank-loop-factor level)))

(defun eln-opt-rank--lap (fn)
  ;; `byte-decompile-bytecode' lives in byte-opt.el, which Emacs does not
  ;; always preload (it was missing on a Windows build).
  (require 'byte-opt)
  (seq-filter #'consp
              (byte-decompile-bytecode (aref fn 1) (aref fn 2))))

(defun eln-opt-rank--closures (fn)
  "FN and every byte-code closure nested in its constants."
  (let ((out (list fn)))
    (mapc (lambda (c)
            (when (byte-code-function-p c)
              (setq out (nconc out (eln-opt-rank--closures c)))))
          (aref fn 2))
    out))

(defun eln-opt-rank--one (fn self)
  "Benefit and call sites of one byte-code object FN, nested closures excluded.
Returns (:benefit B :loops N :calls LIST :varrefs LIST :dynamic BOOL :size N)."
  (let* ((ops (vconcat (eln-opt-rank--lap fn)))
         (n (length ops))
         (tag-pos (make-hash-table :test #'eq))
         (depth (make-vector n 0))
         (recursive (memq self (append (aref fn 2) nil)))
         (benefit 0.0) (loops 0) (calls nil) (varrefs nil) (dynamic nil))
    (dotimes (i n)
      (when (eq (car (aref ops i)) 'TAG)
        (puthash (aref ops i) i tag-pos)))
    ;; A jump back to an earlier tag marks a loop body.
    (dotimes (i n)
      (let* ((op (aref ops i))
             (j (and (memq (car op) '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
                                      byte-goto-if-nil-else-pop
                                      byte-goto-if-not-nil-else-pop))
                     (gethash (cdr op) tag-pos))))
        (when (and j (< j i))
          (cl-incf loops)
          (cl-loop for k from j to i do (cl-incf (aref depth k))))))
    (dotimes (i n)
      (let* ((op (aref ops i))
             (head (car op))
             (w (eln-opt-rank--weight (+ (aref depth i) (if recursive 1 0))))
             (class (gethash head eln-opt-rank--op-class)))
        (cond
         ((eq class 'inline) (cl-incf benefit w))
         ((eq class 'buffer) (cl-decf benefit (* 2 w)))
         ((eq head 'byte-varref)
          (let ((v (car-safe (cdr op)))) (when (symbolp v) (push v varrefs))))
         ((eq head 'byte-constant)
          (let ((c (car-safe (cdr op))))
            (when (and (symbolp c) c (fboundp c))
              (push c calls)
              (pcase (gethash c eln-opt-rank--const-class)
                ((or 'c-heavy 'alloc) (cl-decf benefit (* 2 w)))
                ('dynamic (setq dynamic t) (cl-incf benefit (* 0.5 w)))
                (_ (unless (subrp (indirect-function c))
                     (cl-incf benefit (* 0.5 w)))))))))))
    (list :benefit benefit :loops loops :calls calls :varrefs varrefs
          :dynamic dynamic :size (length (aref fn 1)))))

(defun eln-opt-rank--features (sym)
  "Summed features of SYM's definition and its nested closures."
  (let ((def (indirect-function sym))
        (b 0.0) (loops 0) (size 0) calls vars dyn)
    (when (byte-code-function-p def)
      (dolist (f (eln-opt-rank--closures def))
        (let ((r (eln-opt-rank--one f sym)))
          (cl-incf b (plist-get r :benefit))
          (cl-incf loops (plist-get r :loops))
          (cl-incf size (plist-get r :size))
          (setq calls (nconc (plist-get r :calls) calls)
                vars (nconc (plist-get r :varrefs) vars)
                dyn (or dyn (plist-get r :dynamic))))))
    (list :benefit (max 0.0 b) :loops loops :size size
          :calls (delete-dups calls) :varrefs (delete-dups vars) :dynamic dyn)))

;;; ------------------------------------------------------------
;;; Files: which source a function belongs to, and whether it may be compiled
;;; ------------------------------------------------------------

(defun eln-opt-rank--file-key (file)
  "Name used in the ranking: basename without .elc, .el or .gz.
Returns nil for autoload files (-autoloads, -loaddefs), which only hold stubs."
  (let ((base (file-name-nondirectory file)))
    (unless (string-match-p "\\(\\`\\|-\\)\\(autoloads\\|loaddefs\\)\\.el\\(c\\|\\.gz\\)?\\'" base)
      (replace-regexp-in-string "\\.\\(elc\\|el\\)\\(\\.gz\\)?\\'" "" base))))

(defvar eln-opt-rank--no-native-cache (make-hash-table :test #'equal))

(defun eln-opt-rank--no-native-p (file)
  "Non-nil if FILE opts out of native or byte compilation with a local variable."
  (let ((true (or (ignore-errors (file-truename file)) file)))
    (let ((hit (gethash true eln-opt-rank--no-native-cache 'unknown)))
      (if (not (eq hit 'unknown))
          hit
        (puthash true
                 (and (file-readable-p true)
                      (with-temp-buffer
                        (insert-file-contents true)
                        (let ((text (buffer-string)))
                          (or (string-match-p "no-native-compile:[ \t]*t" text)
                              (string-match-p "no-byte-compile:[ \t]*t" text)))))
                 eln-opt-rank--no-native-cache)))))

;;; ------------------------------------------------------------
;;; Graph: functions, call edges, and roots with evidence weights
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; Feature cache: each source file's features, kept between runs
;;; ------------------------------------------------------------
;; The features of a function come from its byte code, which only changes when
;; its file is recompiled or reloaded.  So they are kept per load file, with a
;; validator (size and mtime of that file).  A run reads the cache once, uses
;; it for every file whose validator still matches, and writes back only the
;; files it saw: a file that is gone drops out.  The cache lives in the build's
;; state folder, so another Emacs binary never reads it.

(defvar eln-opt-rank--feature-cache nil
  "Per load file: (VALIDATOR . HASH of SYM -> FEATURES), read once per process.")
(defvar eln-opt-rank--feature-loaded nil)
(defvar eln-opt-rank--feature-seen nil
  "Per load file seen in this run: (VALIDATOR . ALIST of (SYM . FEATURES)).")

(defun eln-opt-rank--feature-file ()
  (expand-file-name "features.eld" (eln-opt--version-dir)))

(defconst eln-opt-rank--feature-max-bytes (* 20 1024 1024)
  "A cache file larger than this is ignored: it is not one this code wrote.")

(defun eln-opt-rank--feature-enabled-p ()
  "Non-nil unless ELN_OPT_FEATURE_CACHE=0 turns the cache off."
  (not (equal (getenv "ELN_OPT_FEATURE_CACHE") "0")))

(defconst eln-opt-rank--code-file (or load-file-name buffer-file-name)
  "The file this code was loaded from: its digest says which analysis produced
the cached features.  Hashing the loaded file, not the source, means an edit
not yet compiled does not pretend to be the code that runs.")

(defun eln-opt-rank--digest (file)
  "SHA-256 of FILE's bytes, or nil when it cannot be read.  Reading and hashing
600 files of about 20 MB took 0.08 s, far less than the analysis it replaces."
  (when (and (stringp file) (file-readable-p file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun eln-opt-rank--analysis-key ()
  "Digest of the code that does the analysis; a changed analysis invalidates
every cached feature."
  (eln-opt-rank--digest eln-opt-rank--code-file))

(defun eln-opt-rank--validator (path)
  "What identifies PATH's current contents: its digest."
  (eln-opt-rank--digest path))

(defun eln-opt-rank--feature-valid-p (feat)
  "Non-nil if FEAT has the shape `eln-opt-rank--features' makes.  Anything
else in a cache file means the file is not trusted."
  (and (proper-list-p feat)
       (numberp (plist-get feat :benefit))
       (integerp (plist-get feat :loops))
       (integerp (plist-get feat :size))
       (proper-list-p (plist-get feat :calls))
       (cl-every #'symbolp (plist-get feat :calls))
       (proper-list-p (plist-get feat :varrefs))
       (cl-every #'symbolp (plist-get feat :varrefs))
       (memq (plist-get feat :dynamic) '(nil t))))

(defun eln-opt-rank--feature-safe-file-p (file)
  "Non-nil if FILE is a regular file, not a symbolic link, of a sane size."
  (and (file-regular-p file)
       (not (file-symlink-p file))
       (let ((size (file-attribute-size (file-attributes file))))
         (and (integerp size) (<= size eln-opt-rank--feature-max-bytes)))))

(defun eln-opt-rank--feature-load ()
  "Read the cache from disk, once per process.  Any problem means no cache."
  (unless eln-opt-rank--feature-loaded
    (setq eln-opt-rank--feature-loaded t
          eln-opt-rank--feature-cache (make-hash-table :test #'equal))
    (let ((file (eln-opt-rank--feature-file)))
      (when (and (eln-opt-rank--feature-enabled-p)
                 (eln-opt-rank--feature-safe-file-p file))
        (ignore-errors
          (let* ((data (with-temp-buffer
                         (insert-file-contents file)
                         (let ((read-eval nil)) (read (current-buffer)))))
                 )
            ;; All or nothing: a cache with one bad entry is not used at all.
            (when (and (equal (plist-get data :version) (eln-opt--version-name))
                       (equal (plist-get data :key) (eln-opt-rank--analysis-key))
                       (proper-list-p (plist-get data :files))
                       (cl-every
                        (lambda (entry)
                          (and (stringp (car entry)) (proper-list-p entry)
                               (stringp (nth 1 entry))
                               (proper-list-p (nth 2 entry))
                               (cl-every (lambda (pair)
                                           (and (consp pair) (symbolp (car pair))
                                                (eln-opt-rank--feature-valid-p (cdr pair))))
                                         (nth 2 entry))))
                        (plist-get data :files)))
              (dolist (entry (plist-get data :files))
                (let ((table (make-hash-table :test #'eq)))
                  (dolist (pair (nth 2 entry))
                    (puthash (car pair) (cdr pair) table))
                  (puthash (car entry) (cons (nth 1 entry) table)
                           eln-opt-rank--feature-cache))))))))))

(defvar eln-opt-rank--feature-digests nil
  "PATH -> digest, for this run only.  A file holds many functions, and its
digest is the costly part, so it is read once per run, not once per function.")

(defun eln-opt-rank--validator-once (path)
  (or (and eln-opt-rank--feature-digests
           (gethash path eln-opt-rank--feature-digests))
      (let ((d (eln-opt-rank--validator path)))
        (when eln-opt-rank--feature-digests
          (puthash path d eln-opt-rank--feature-digests))
        d)))

(defun eln-opt-rank--cached-features (sym path)
  "The features of SYM, from this run, else the disk cache, when PATH's digest
still matches; otherwise analysed now."
  (eln-opt-rank--feature-load)
  (let* ((valid (eln-opt-rank--validator-once path))
         (seen (gethash path eln-opt-rank--feature-seen))
         (this-run (and seen valid (equal (car seen) valid)
                        (assq sym (cdr seen))))
         (old (gethash path eln-opt-rank--feature-cache))
         (table (and old valid (equal (car old) valid) (cdr old)))
         (from-disk (and table (gethash sym table 'none)))
         (feat (cond (this-run (cdr this-run))
                     ((and from-disk (not (eq from-disk 'none))) from-disk)
                     (t (eln-opt-rank--features sym)))))
    (unless this-run
      (puthash path (cons valid (cons (cons sym feat) (and seen (cdr seen))))
               eln-opt-rank--feature-seen))
    feat))

(defun eln-opt-rank--feature-save ()
  "Write this run's features to the cache, replacing the old file.  Writes to
a fresh temporary file and renames it over the old one, so a reader sees the
old file or the new one, never a partial one.  Never fails."
  (when (and eln-opt-rank--feature-seen (eln-opt-rank--feature-enabled-p))
    (ignore-errors
      (let ((file (eln-opt-rank--feature-file))
            (key (eln-opt-rank--analysis-key))
            (files nil))
        (when (and key (not (and (file-exists-p file) (file-symlink-p file))))
          (maphash (lambda (path entry)
                     (when (car entry)
                       (push (list path (car entry) (cdr entry)) files)))
                   eln-opt-rank--feature-seen)
          (make-directory (file-name-directory file) t)
          (let ((temp (make-temp-file (expand-file-name "features-" (file-name-directory file))
                                      nil ".tmp")))
            (unwind-protect
                (progn
                  (with-temp-file temp
                    (let ((print-length nil) (print-level nil))
                      (prin1 (list :version (eln-opt--version-name) :key key :files files)
                             (current-buffer))))
                  (rename-file temp file t))
              (when (file-exists-p temp) (ignore-errors (delete-file temp))))))))))

(defun eln-opt-rank--fn-values (v)
  "Functions named by the value of variable V: one function or a list of them."
  (when (and (symbolp v) (boundp v))
    (let ((val (symbol-value v)))
      (cond ((and (symbolp val) val (fboundp val)) (list val))
            ((and (consp val) (proper-list-p val))
             (seq-filter (lambda (x) (and (symbolp x) x (not (eq x t)) (fboundp x)))
                         val))))))

(defun eln-opt-rank--read-file (file)
  (when (file-readable-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun eln-opt-rank--graph (profile-dir)
  "Build the graph from the loaded Emacs.  Returns a plist: :features (sym
. feature-plist), :file (sym . file-key), :path (sym . full file), :edges
 (sym . callees), :excluded (file-key . reason), :native-skipped N,
:interpreted-skipped N."
  (let ((features (make-hash-table :test #'eq))
        (file-of (make-hash-table :test #'eq))
        (path-of (make-hash-table :test #'eq))
        (edges (make-hash-table :test #'eq))
        (excluded nil)
        (interpreted-skipped 0)
        (native-skipped 0))
    ;; Functions: every defun from load-history, keyed by its source file.
    (setq eln-opt-rank--feature-seen (make-hash-table :test #'equal)
          eln-opt-rank--feature-digests (make-hash-table :test #'equal))
    (dolist (entry load-history)
      (let* ((file (car entry))
             (key (and (stringp file) (eln-opt-rank--file-key file))))
        (when key
          (let ((optout 'unknown))
          (dolist (d (cdr entry))
            (when (and (consp d) (eq (car d) 'defun) (symbolp (cdr d))
                       (fboundp (cdr d)))
              (let ((sym (cdr d)))
                (cond
                 ((native-comp-function-p (indirect-function sym))
                  (cl-incf native-skipped))
                 ((if (eq optout 'unknown)
                      (setq optout (eln-opt-rank--no-native-p file))
                    optout)
                  (cl-pushnew (cons key "no-native-compile or no-byte-compile") excluded
                              :test #'equal))
                 ;; Interpreted functions are skipped, not compiled here: the
                 ;; report must have no side effects.  Our own lisp is byte
                 ;; code once compile-lisp has run.
                 ((byte-code-function-p (indirect-function sym))
                  (puthash sym key file-of)
                  (puthash sym file path-of)
                  (puthash sym (eln-opt-rank--cached-features sym file) features))
                 (t (cl-incf interpreted-skipped))))))))))
    ;; Edges: constants, plus function-valued variables read by funcall/apply.
    (maphash
     (lambda (sym feat)
       (dolist (c (plist-get feat :calls))
         (unless (eq c sym) (puthash sym (cons c (gethash sym edges)) edges)))
       (when (plist-get feat :dynamic)
         (dolist (v (plist-get feat :varrefs))
           (dolist (f (eln-opt-rank--fn-values v))
             (unless (eq f sym) (puthash sym (cons f (gethash sym edges)) edges))))))
     features)
    ;; Advice: calling the original also runs its advice.
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (advice--p (advice--symbol-function s)))
         (advice-mapc (lambda (f _) (when (and (symbolp f) (not (eq f s)))
                                      (puthash s (cons f (gethash s edges)) edges)))
                      s))))
    (eln-opt-rank--feature-save)
    (list :features features :file file-of :path path-of :edges edges
          :excluded excluded :native-skipped native-skipped
          :interpreted-skipped interpreted-skipped
          :profile profile-dir)))

(defun eln-opt-rank--roots (profile-dir)
  "Roots with evidence weights, as an alist (SYM . WEIGHT).
Savehist commands are newest first, capped and decaying.  Recentf files map to
modes through `auto-mode-alist', and a mode's hook and keymap commands follow
it at a lower weight.  Hooks that run on every key are heavy."
  (let ((roots (make-hash-table :test #'eq))
        (savehist nil)
        (user-count 0))
    (cl-flet ((add (sym w)
                (when (and sym (symbolp sym) (fboundp sym))
                  (puthash sym (+ w (gethash sym roots 0.0)) roots))))
      ;; 1. Commands the user ran: savehist keeps only the command history
      ;;    here; the rest of that file (the kill ring) is never read.
      (let* ((text (eln-opt-rank--read-file (expand-file-name "history" profile-dir)))
             (cmds (and text (string-match "(setq extended-command-history '" text)
                        (seq-filter #'stringp
                                    (ignore-errors
                                     (car (read-from-string text (match-end 0))))))))
        (cl-loop for c in cmds for i from 0
                 do (let ((sym (intern-soft c))
                          (w (min eln-opt-rank-savehist-cap (/ 1.0 (+ 1 (* 0.05 i))))))
                      (when (and sym (fboundp sym))
                        (push (cons sym w) savehist))
                      (add sym w)))
        (cl-incf user-count (length cmds)))
      ;; 2. Modes the user edits.  recentf.eld is data: read, never eval'd.
      (let* ((text (eln-opt-rank--read-file (expand-file-name "recentf.eld" profile-dir)))
             (form (and text (string-match "(setq recentf-list" text)
                        (ignore-errors (car (read-from-string text (match-beginning 0))))))
             (files (and (eq (car-safe form) 'setq) (eq (car-safe (nth 2 form)) 'quote)
                         (seq-filter #'stringp (cadr (nth 2 form)))))
             (modes (make-hash-table :test #'eq)))
        (dolist (f files)
          (let ((mode (assoc-default f auto-mode-alist #'string-match)))
            (when (consp mode) (setq mode (car mode)))
            (when (and mode (symbolp mode))
              (puthash mode (1+ (gethash mode modes 0)) modes))))
        (cl-incf user-count (length files))
        (maphash
         (lambda (mode count)
           (let ((w (log (1+ count) 2)))
             (add mode w)
             (dolist (f (eln-opt-rank--fn-values (intern-soft (format "%s-hook" mode))))
               (add f w))
             (let ((map (intern-soft (format "%s-map" mode))))
               (when (and map (boundp map) (keymapp (symbol-value map)))
                 (map-keymap-internal
                  (lambda (_ b) (when (symbolp b) (add b (* 0.1 w))))
                  (symbol-value map))))))
         modes))
      ;; 3. Hooks that run on every key or redisplay.
      (dolist (h '(pre-command-hook post-command-hook after-change-functions
                   before-change-functions post-self-insert-hook
                   window-configuration-change-hook window-scroll-functions
                   window-buffer-change-functions window-state-change-functions
                   jit-lock-functions completion-at-point-functions
                   minibuffer-setup-hook))
        (dolist (f (append (eln-opt-rank--fn-values h)
                           (let ((d (and (boundp h) (default-value h))))
                             (and (proper-list-p d) (seq-filter #'symbolp d)))))
          (add f eln-opt-rank-always-heat)))
      ;; 4. Global keymap commands: weak evidence.
      (when (keymapp global-map)
        (map-keymap (lambda (_ b) (when (symbolp b) (add b 0.05))) global-map)))
    (list :roots (let (l) (maphash (lambda (k v) (push (cons k v) l)) roots) l)
          :savehist savehist
          :user-count user-count)))

(defun eln-opt-rank--baseline (profile-dir)
  "The shipped baseline roots, weighted down as the user's own data grows.
Returns (ROOTS . FACTOR).  The baseline file names functions only."
  (let* ((file (or (and (bound-and-true-p xl-dotfiles)
                        (expand-file-name "tuned-emacs/data/eln-opt-baseline.eld" xl-dotfiles))
                   (expand-file-name "eln-opt-baseline.eld" profile-dir)))
         (form (eln-opt--read-eld file))
         (names (plist-get form :functions)))
    (cons (and names
               (seq-filter (lambda (s) (and (symbolp s) (fboundp s))) names))
          nil)))

(defun eln-opt-rank--propagate (seeds edges)
  "Heat per symbol after spreading SEEDS, an alist (SYM . WEIGHT), along EDGES."
  (let ((heat (make-hash-table :test #'eq))
        (frontier seeds))
    (dolist (s seeds)
      (puthash (car s) (+ (cdr s) (gethash (car s) heat 0.0)) heat))
    (dotimes (_ eln-opt-rank-heat-levels)
      (let ((next (make-hash-table :test #'eq)))
        (dolist (fw frontier)
          (let* ((outs (gethash (car fw) edges))
                 (share (/ (* eln-opt-rank-spread (cdr fw))
                           (max 1 (sqrt (length outs))))))
            (dolist (g outs)
              (puthash g (+ share (gethash g next 0.0)) next))))
        (setq frontier nil)
        (maphash (lambda (g w)
                   (puthash g (+ w (gethash g heat 0.0)) heat)
                   (push (cons g w) frontier))
                 next)))
    heat))

;;; ------------------------------------------------------------
;;; Ranking
;;; ------------------------------------------------------------

(defun eln-opt-rank--file-scores (heat graph)
  "Per file key: score = sum of heat x benefit, and the file's total size."
  (let ((scores (make-hash-table :test #'equal)))
    (maphash
     (lambda (sym h)
       (let ((key (gethash sym (plist-get graph :file)))
             (feat (gethash sym (plist-get graph :features))))
         (when (and key feat (> h 0))
           (cl-incf (gethash key scores 0.0) (* h (plist-get feat :benefit))))))
     heat)
    scores))

(defconst eln-opt-rank-load-min-weight 0.5
  "A root this heavy that is still an autoload gets its package loaded, so its
functions are in the graph.  Lazy packages (org, for one) are otherwise invisible.")

(defun eln-opt-rank--load-lazy-roots (roots)
  "Load the packages behind ROOTS that are still autoloads and heavy enough.
Returns the feature or file names loaded, for the report."
  (let (loaded)
    (dolist (r roots)
      (let ((s (car r)))
        (when (and (>= (cdr r) eln-opt-rank-load-min-weight)
                   (fboundp s)
                   (autoloadp (symbol-function s)))
          (let ((file (cadr (symbol-function s))))
            (when (ignore-errors (autoload-do-load (symbol-function s) s) t)
              (push (if (stringp file) (file-name-nondirectory file) (symbol-name s))
                    loaded))))))
    (delete-dups loaded)))

(defun eln-opt-rank--compute (profile-dir)
  "Full analysis.  Returns a plist: :rows (sorted), :savehist-share, and the
numbers for the header.  Roots are found first, so that lazy packages behind
heavy roots can be loaded before the graph is built."
  (let* ((roots-info (xl-perfetto-with-span "reach: roots" (eln-opt-rank--roots profile-dir)))
         (native (eln-opt-platform-available-p))
         (user-roots (plist-get roots-info :roots))
         (lazy-loaded (xl-perfetto-with-span "reach: lazy roots" (eln-opt-rank--load-lazy-roots user-roots)))
         (graph (xl-perfetto-with-span "reach: graph" (eln-opt-rank--graph profile-dir)))
         (edges (plist-get graph :edges))
         (user-count (plist-get roots-info :user-count))
         (baseline (car (eln-opt-rank--baseline profile-dir)))
         (base-factor (max 0.0 (- 1.0 (/ (float user-count) eln-opt-rank-baseline-users))))
         (base-roots (mapcar (lambda (s) (cons s (* base-factor 1.0))) baseline))
         (seeds (append user-roots base-roots))
         (heat (xl-perfetto-with-span "reach: propagate" (eln-opt-rank--propagate seeds edges)))
         (scores (xl-perfetto-with-span "reach: file scores" (eln-opt-rank--file-scores heat graph)))
         (total (let ((sum 0.0)) (maphash (lambda (_ v) (cl-incf sum v)) scores) sum))
         ;; Each savehist command's share of the whole score.  Heat is linear,
         ;; so one command's contribution is its own propagation.
         (savehist-cmds (plist-get roots-info :savehist))
         (max-share 0.0))
    (when (> total 0)
      (dolist (r savehist-cmds)
        (let* ((h (eln-opt-rank--propagate (list r) edges))
               (s (eln-opt-rank--file-scores h graph))
               (part 0.0))
          (maphash (lambda (_ v) (cl-incf part v)) s)
          (setq max-share (max max-share (/ part total))))))
    (let (rows)
      (maphash
       (lambda (key score)
         (let ((size 0) (fns nil))
           (maphash (lambda (sym k)
                      (when (equal k key)
                        (let ((feat (gethash sym (plist-get graph :features)))
                              (h (gethash sym heat 0.0)))
                          (cl-incf size (plist-get feat :size))
                          (when (> h 0)
                            (push (cons sym (* h (plist-get feat :benefit))) fns)))))
                    (plist-get graph :file))
           (when (> score 0)
             (push (list :file key :score score :size size
                         :ratio (/ score (max 0.1 (/ size 1024.0)))
                         :top (mapcar #'car (seq-take (sort fns (lambda (a b) (> (cdr a) (cdr b))))
                                                      3))
                         :defuns (let (names)
                                   (maphash (lambda (sym k)
                                              (when (equal k key) (push sym names)))
                                            (plist-get graph :file))
                                   names)
                         :path (cl-loop for s being the hash-keys of (plist-get graph :file)
                                        using (hash-values k)
                                        when (equal k key)
                                        return (gethash s (plist-get graph :path)))
                         :trusted (let ((p (cl-loop for s being the hash-keys of (plist-get graph :file)
                                                    using (hash-values k)
                                                    when (equal k key)
                                                    return (gethash s (plist-get graph :path)))))
                                    (and p (eln-opt-trusted-file-p p))))
                   rows))))
       scores)
      (list :rows (sort rows (lambda (a b) (> (plist-get a :score) (plist-get b :score))))
            :lazy-loaded lazy-loaded
            :built-rows (eln-opt-rank--tier-rows rows)
            :native native
            :platform-reason (and (not native)
                                  (or (plist-get (eln-opt--read-eld (eln-opt--platform-file))
                                                 :reason)
                                      "unknown"))
            :byte (eln-opt--byte-candidates)
            :next (and native
                       (eln-opt--plan-from-rows
                        (sort (copy-sequence rows)
                              (lambda (a b) (> (or (plist-get a :ratio) 0)
                                               (or (plist-get b :ratio) 0))))
                        (- eln-opt-worker-max
                           (min eln-opt-worker-max (length (eln-opt--byte-candidates))))))
            :failed (eln-opt--failed-load)
            :savehist-share max-share
            :functions (hash-table-count (plist-get graph :features))
            :roots (length user-roots)
            :user-count user-count
            :baseline-roots (length baseline)
            :baseline-factor base-factor
            :excluded (plist-get graph :excluded)
            :native-skipped (plist-get graph :native-skipped)
            :interpreted-skipped (plist-get graph :interpreted-skipped)))))

(defun eln-opt-rank--tier-rows (ranked)
  "One row per file at Tier 1 or 2, sorted: the elns first, then the
byte-compiled files, each by name.  A row is `:elc' when its .elc is current,
and `:eln' when eln-opt built the eln Emacs uses.  RANKED are the ranking rows:
their files with a current .elc are listed too, as byte-compiled."
  (let* ((info (eln-opt-native-list))
         (natives (plist-get info :files))
         (seen (make-hash-table :test #'equal))
         rows)
    (dolist (b (plist-get info :built))
      (let* ((src (car b))
             (stem (eln-opt--source-stem src))
             (fns (cl-loop for (file . syms) in natives
                           when (equal (eln-opt--source-stem file) stem)
                           append syms)))
        (puthash src t seen)
        (push (list :file (eln-opt-rank--file-key src) :src src
                    :elc (eln-opt--elc-current-p src) :eln t
                    :size (or (eln-opt--source-bytes src) 0)
                    :count (length fns)
                    :top (seq-take fns 3))
              rows)))
    (dolist (r ranked)
      (let* ((path (plist-get r :path))
             (src (and path (eln-opt--source-for path))))
        (when (and src (not (gethash src seen)) (eln-opt--elc-current-p src))
          (puthash src t seen)
          (push (list :file (eln-opt-rank--file-key src) :src src
                      :elc t :eln nil
                      :size (or (eln-opt--source-bytes src) 0)
                      :count (length (plist-get r :defuns))
                      :top (seq-take (plist-get r :top) 3))
                rows))))
    (sort rows (lambda (a b)
                 (if (eq (plist-get a :eln) (plist-get b :eln))
                     (string< (plist-get a :file) (plist-get b :file))
                   (plist-get a :eln))))))

(defun eln-opt-rank--when (time)
  (if time (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time time)) "-"))

(defun eln-opt-rank--format (result)
  "The report as a string.  Sections, in order: what the next pass does, the
files at Tier 1 and 2, what failed, what happens to each ranked file (with its
score and cost), and what is left out altogether."
  (let* ((built (plist-get result :built-rows))
         (next (plist-get result :next))
         (failed (plist-get result :failed))
         ;; A result without the key (built by hand) is treated as native.
         (native (if (plist-member result :native) (plist-get result :native) t))
         (next-names (mapcar #'file-name-nondirectory next))
         (rows (plist-get result :rows)))
    (with-output-to-string
      ;; 1. The answer
      (princ "eln-opt report (report only; nothing is compiled here)\n")
      (unless native
        (princ (format "Native compilation is not available in this Emacs (%s): only byte-compiling happens here.\n"
                       (or (plist-get result :platform-reason) "unknown"))))
      (princ "\n")
      (princ (eln-opt-rank--byte-line (plist-get result :byte)))
      (when native
        (princ "Next worker native-compiles: ")
        (if (null next)
            (princ "nothing.\n")
          (princ (format "%d file(s), in this order: %s\n" (length next)
                         (mapconcat #'identity next-names ", ")))))
      ;; 2. What was compiled
      (let ((elns (seq-filter (lambda (r) (plist-get r :eln)) built)))
        (princ (format "\nStatus: %d file(s) at Tier 2 (native, %d function(s)); %d file(s) at Tier 1 (byte-compiled)\n"
                       (length elns)
                       (apply #'+ (mapcar (lambda (r) (plist-get r :count)) elns))
                       (- (length built) (length elns)))))
      (when built
        (princ (format "\n%-28s  %6s  %-4s  %-4s  %9s  %s\n"
                       "file" "KB" "elc" "eln" "functions" "examples"))
        (dolist (row built)
          (princ (format "%-28s  %6.1f  %-4s  %-4s  %9d  %S\n"
                         (plist-get row :file)
                         (/ (plist-get row :size) 1024.0)
                         (if (plist-get row :elc) "yes" "no")
                         (if (plist-get row :eln) "yes" "no")
                         (plist-get row :count)
                         (plist-get row :top)))))
      ;; 3. What failed
      (when failed
        (princ "\nSkipped after a failure (until the source changes):\n")
        (dolist (e failed)
          (princ (format "  %-30s %s\n    %s\n"
                         (file-name-nondirectory (plist-get e :src))
                         (eln-opt-rank--when (plist-get e :time))
                         (or (plist-get e :reason) "")))))
      ;; 4. Why each ranked file is or is not compiled
      (princ "\nRanked files not yet native, and what happens to each\n")
      (princ "(score: expected gain; KB: bytecode size, the cost)\n")
      (if (not native)
          (princ "  Not listed: without native compilation no file here is native-compiled.\n")
        (princ (format "  %-28s  %6s  %6s  %s\n" "file" "score" "KB" "what happens")))
      (let ((n 0))
        (dolist (row (and native rows))
          (let* ((src (and (plist-get row :path)
                           (eln-opt--source-for (plist-get row :path))))
                 (why (cond ((and src (member src next)) "compile in the next worker run")
                            ((eln-opt--candidate-reason row))
                            ((and src (eln-opt--failed-p src)) "failed before; see above")
                            (t (format "candidate, but past the limit of %d per worker run"
                                       eln-opt-worker-max)))))
            (when (< n 40)
              (princ (format "  %-28s  %6.1f  %6.1f  %s\n"
                             (plist-get row :file) (plist-get row :score)
                             (/ (or (plist-get row :size) 0) 1024.0) why)))
            (cl-incf n)))
        (when (> n 40) (princ (format "  ... and %d more\n" (- n 40)))))
      ;; 5. What is left out altogether
      (princ "\nLeft out before ranking:\n")
      (princ (format "  already native in this Emacs (after init): %d function(s)\n"
                     (plist-get result :native-skipped)))
      (let ((excl (plist-get result :excluded)))
        (princ (format "  opted out with no-native-compile: %d file(s)%s\n"
                       (length excl)
                       (if excl (format ": %s"
                                        (mapconcat (lambda (e) (car e)) excl ", "))
                         ""))))
      (princ (format "  interpreted, no .elc (Crafted's modules): %d function(s)\n"
                     (plist-get result :interpreted-skipped)))
      (when (plist-get result :lazy-loaded)
        (princ (format "  loaded for the analysis, because heavy roots need them: %s\n"
                       (mapconcat #'identity (plist-get result :lazy-loaded) ", "))))
      (princ (format "  functions analysed: %d;  largest single savehist command share of the score: %.1f%%\n"
                     (plist-get result :functions)
                     (* 100 (plist-get result :savehist-share)))))))

;;; The report as a buffer.  The ranking takes seconds, so it runs in a child
;;; Emacs that loads the profile and prints this same text; the buffer shows a
;;; spinner in its mode line until the child answers (as GBlame does).

(defconst eln-opt-rank--marker "<<ELN-OPT-REPORT>>"
  "Printed on its own line before a child's report.  The child's own loading
messages also go to its output, so the parent keeps only what follows it.")

(defvar eln-opt-rank--with-marker nil
  "Non-nil in the child: print `eln-opt-rank--marker' before the report.")

(defun eln-opt-rank--report-from-output (output)
  "The report in a child's OUTPUT: what follows the marker, or nil."
  (let ((at (string-match (regexp-quote eln-opt-rank--marker) output)))
    (and at (string-trim-left
             (substring output (+ at (length eln-opt-rank--marker))) "\n+"))))

(defun eln-opt-rank--child-command (form)
  "The child Emacs command that evaluates FORM (a string of Lisp) after the
profile's init, so the child sees the same packages and load path."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory)))
    (list exe "--batch" "--init-directory" profile
          "-l" (expand-file-name "early-init.el" profile)
          "--eval" "(run-hooks (quote before-init-hook))"
          "-l" (expand-file-name "init.el" profile)
          "--eval" form)))

(defvar-local eln-opt-rank--center nil
  "The overlay that shows the loader in the middle of the window, while waiting.")

(defvar-local eln-opt-rank--busy-text "computing the ranking"
  "What the loader in the middle of the window says while the child works.")

(defun eln-opt-rank--draw-center (frame)
  "Draw FRAME in the middle of this buffer's window while the report is pending.
nil removes it.  The loader is a `before-string' on an empty placeholder line,
so no text is added and the window does not move, as in GBlame."
  (if (null frame)
      (when (overlayp eln-opt-rank--center)
        (delete-overlay eln-opt-rank--center)
        (setq eln-opt-rank--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  " eln-opt-rank--busy-text))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (pos (save-excursion
                    (goto-char (window-start window))
                    (forward-line (/ (window-body-height window) 2))
                    (line-beginning-position))))
        (unless (overlayp eln-opt-rank--center)
          (setq eln-opt-rank--center (make-overlay pos pos)))
        (move-overlay eln-opt-rank--center pos pos)
        (overlay-put eln-opt-rank--center 'before-string
                     (concat (make-string pad ?\s) text))))))

(defun eln-opt-rank--colorize (text)
  "TEXT with faces for the report buffer: the title and table header bold,
section titles bold, `yes' in the table green and `no' dimmed.  Only the
buffer gets faces; the batch output stays plain text.  Faces do not change
the width of the text, so the columns still line up."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((in-table nil) (in-left nil) (table-end 0))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring bol eol)))
          (cond
           ((string-match-p "\\`eln-opt report" line)
            (add-face-text-property bol eol 'bold))
           ((string-match-p "\\`Left out before ranking:" line)
            (setq in-left t)
            (add-face-text-property bol eol 'bold))
           ((and in-left (string-match-p "\\`  " line))
            ;; Counts are numbers; the lists of files are file names.
            (save-excursion
              (goto-char bol)
              (while (re-search-forward "[0-9]+\\(?:\\.[0-9]+\\)?%?" eol t)
                (add-face-text-property (match-beginning 0) (match-end 0)
                                        'font-lock-constant-face)))
            (when (and (string-match ": \\(.+\\)\\'" line)
                       (string-match-p "opted out\\|loaded for the analysis" line))
              (let ((start (+ bol (match-beginning 1))))
                (save-excursion
                  (goto-char start)
                  (while (re-search-forward "[^, \n]+" eol t)
                    (add-face-text-property (match-beginning 0) (match-end 0)
                                            'font-lock-type-face))))))
           ((string= line "")
            (setq in-left nil in-table nil))
           ((string-match-p "\\`file +KB +elc +eln" line)
            (setq in-table t
                  table-end (or (string-match "examples" line) (length line)))
            (add-face-text-property bol eol 'bold))
           (in-table
            ;; The file name is the first column; the examples are the
            ;; function names from the examples column on.
            (add-face-text-property bol (+ bol (or (string-match " " line) (length line)))
                                    'font-lock-type-face)
            (save-excursion
              (goto-char bol)
              (while (re-search-forward "\\_<\\(yes\\|no\\)\\_>"
                                        (min eol (+ bol table-end)) t)
                (add-face-text-property (match-beginning 0) (match-end 0)
                                        (if (equal (match-string 0) "yes") 'success 'shadow))))
            (save-excursion
              (goto-char (min eol (+ bol table-end)))
              (while (re-search-forward "[^ ()\n]+" eol t)
                (add-face-text-property (match-beginning 0) (match-end 0)
                                        'font-lock-function-name-face))))
           ((string-match "\\`  file +score +KB" line)
            (add-face-text-property bol eol 'bold))
           ((string-match "\\`  \\([^ \n]+\\) +[-0-9.]+  +[0-9.]+  " line)
            ;; A ranking row: the file name after the indent.
            (add-face-text-property (+ bol 2) (+ bol 2 (length (match-string 1 line)))
                                    'font-lock-type-face))
           ((string-match-p "\\`[A-Za-z][^\n]*:[ \t]*\\'" line)
            (add-face-text-property bol eol 'bold))))
        (forward-line 1))
      (buffer-string))))

(defconst eln-opt-report-cache-seconds 60
  "How long a finished report is reused, when nothing that the fingerprint
covers has changed.  Repeated `eln-opt-report' calls then cost nothing.")

(defvar eln-opt-rank--cache nil
  "(TIME FINGERPRINT TEXT) of the last report that finished successfully.")

(defun eln-opt-rank--fresh-cache ()
  "The cached report text, if it is recent and the fingerprint still matches."
  (let ((c eln-opt-rank--cache))
    (and c
         (< (- (float-time) (car c)) eln-opt-report-cache-seconds)
         (equal (cadr c) (eln-opt--quiet-fingerprint))
         (caddr c))))

(defun eln-opt-rank--show-async (buffer-name label form &optional busy-text)
  "Fill BUFFER-NAME with the text that the child Emacs prints for FORM.
LABEL names the loader in the mode line.  While the child works, a loader
turns in the mode line and in the middle of the window, on empty placeholder
lines, as GBlame does.  FORM must print `eln-opt-rank--marker' before its
text, so that the child's loading messages are dropped."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Empty placeholder lines: the loader sits on one of them.
        (insert (make-string 400 ?\n)))
      (special-mode)
      (setq-local eln-opt-rank--busy-text (or busy-text "computing the ranking"))
      (xl-spinner-mode-line-start label :also #'eln-opt-rank--draw-center)
      (let ((proc (xl-async-start (eln-opt-rank--child-command form) :name label)))
        (xl-async-own proc buf)
        (xl-async-slot proc 'eln-opt-rank)
        (xl-async-on-exit
         proc
         (lambda (p)
           (xl-spinner-mode-line-stop)
           (eln-opt-rank--draw-center nil)
           (let ((output (xl-async-output p))
                 (report (eln-opt-rank--report-from-output (xl-async-output p)))
                 (inhibit-read-only t))
             (erase-buffer)
             (if (and (eql (xl-async-exit-code p) 0) report
                      (not (string-empty-p (string-trim report))))
                 (progn
                   (setq eln-opt-rank--cache
                         (list (float-time) (eln-opt--quiet-fingerprint) report))
                   (insert (eln-opt-rank--colorize report)))
               (insert "Failed.\n\n" output))
             (goto-char (point-min)))))))
    (display-buffer buf)
    buf))

(defconst eln-opt-rank--report-form
  "(progn (require (quote eln-opt)) (setq eln-opt-rank--with-marker t) (eln-opt-report))"
  "The child form that prints the ranking report.")

(defun eln-opt-rank--byte-line (byte)
  "The report line for BYTE, the files the next worker byte-compiles."
  (if (null byte)
      "Next worker byte-compiles: nothing; no loaded file runs interpreted.\n"
    (format "Next worker byte-compiles first (they run interpreted now): %d file(s): %s\n"
            (length byte) (mapconcat #'file-name-nondirectory byte ", "))))

(defun eln-opt-report ()
  "Show the ranking.  Interactive: a buffer, filled in the background.
Batch: printed to stdout, computed here.  On a platform without native
compilation the same report is shown, with the native parts left out."
  (interactive)
  (cond
   (noninteractive
    (when eln-opt-rank--with-marker
      (princ (format "\n%s\n" eln-opt-rank--marker)))
    (princ (eln-opt-rank--format (eln-opt-rank--compute (eln-opt--profile-dir)))))
   (t
    (let ((buf (get-buffer-create "*eln-opt report*")))
      (cond
       ;; A child is already computing it: show that one, do not restart it.
       ((xl-async-busy-p 'eln-opt-rank buf)
        (display-buffer buf))
       ;; A recent report that still matches: show it, no child at all.
       ((eln-opt-rank--fresh-cache)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (eln-opt-rank--colorize (eln-opt-rank--fresh-cache))))
          (special-mode)
          (goto-char (point-min)))
        (display-buffer buf))
       (t
        (eln-opt-rank--show-async "*eln-opt report*" "eln-opt report"
                                  eln-opt-rank--report-form)))))))

;;; ---- In-progress marker: a compile the worker did not finish ----

;; Before each compile the worker writes in-progress.eld (source, attempts,
;; time) and removes it afterwards.  A marker left behind means that compile
;; was interrupted: Emacs quit, crashed or lost power.  The marker is read
;; only while the worker holds the lock, so the worker that wrote it is gone.
;; Its source path is used only after the trusted check, and only to record a
;; failure: it never decides what gets compiled or deleted.

(defconst eln-opt-interrupt-limit 2
  "Attempts at one file that may each be interrupted.  After this many, the
file goes on the failed list until its source changes.")

(defun eln-opt--marker-file ()
  (expand-file-name "in-progress.eld" (eln-opt--version-dir)))

(defun eln-opt--symlinked-p (file)
  "Non-nil if FILE, or the version folder it is in, is a symbolic link.
The state folder itself may be a link (a profile kept on another disk)."
  (or (file-symlink-p file)
      (file-symlink-p (eln-opt--version-dir))))

(defun eln-opt--mark-in-progress (src attempts)
  "Write the marker for SRC; nil if it cannot be written safely.  A file
that cannot be marked is not compiled."
  (and (not (eln-opt--symlinked-p (eln-opt--marker-file)))
       (condition-case nil
           (progn (eln-opt--write-eld (eln-opt--marker-file)
                                      (list :src src :attempts attempts
                                            :time (eln-opt--now)))
                  t)
         (error nil))))

(defun eln-opt--clear-marker ()
  (ignore-errors (delete-file (eln-opt--marker-file))))

;; A marker that is a symbolic link is never read or removed here: the worker
;; refuses to run instead (see `eln-opt--symlinked-p').
(defun eln-opt--settle-interrupted ()
  "Read a marker left by an interrupted run, under the lock.  Returns
\(SOURCE . ATTEMPTS) when the file may be retried, else nil.  A file at the
limit goes on the failed list here.  Untrusted or malformed markers are
dropped without being used."
  (let* ((marker (and (not (eln-opt--symlinked-p (eln-opt--marker-file)))
                      (eln-opt--read-eld (eln-opt--marker-file))))
         (src (plist-get marker :src))
         (attempts (plist-get marker :attempts)))
    (cond
     ((or (not (and (stringp src) (integerp attempts) (> attempts 0)))
          (not (file-name-absolute-p src))
          (not (eln-opt-trusted-file-p src)))
      (eln-opt--clear-marker)
      nil)
     ((>= attempts eln-opt-interrupt-limit)
      (eln-opt--record-failure src (format "interrupted %d times: Emacs quit or crashed during the compile"
                                           attempts))
      (eln-opt--clear-marker)
      nil)
     (t (cons src attempts)))))

;;; ---- The worker: one batch Emacs that compiles the candidates in turn ----

(defun eln-opt--plan-from-rows (rows &optional max)
  "Sources of ROWS (best first) to native-compile: candidates without a usable
eln that have not failed before, at most MAX (`eln-opt-worker-max')."
  (let (out)
    (dolist (row rows)
      (when (eln-opt--candidate-p row)
        (let ((src (eln-opt--source-for (plist-get row :path))))
          (unless (or (member src out) (eln-opt--built-p src) (eln-opt--failed-p src))
            (push src out)))))
    (seq-take (nreverse out) (max 0 (or max eln-opt-worker-max)))))

;;; Byte-compiling: files that run interpreted.  Needs no libgccjit, so it is
;;; the whole job where native compilation is not available (Windows), and it
;;; catches an .elc gone stale after an edit everywhere: with
;;; `load-prefer-newer', Emacs then loads the .el and runs it interpreted.

(defun eln-opt--init-file-p (file)
  "Non-nil if FILE is an init or custom file, which is never byte-compiled:
a stale init.elc is a well-known trap."
  (or (member (file-name-nondirectory file) '("init.el" "early-init.el" "custom.el"))
      (and (bound-and-true-p custom-file)
           (equal (ignore-errors (file-truename file))
                  (ignore-errors (file-truename custom-file))))))

(defun eln-opt--no-byte-compile-p (file)
  "Non-nil if FILE opts out of byte compilation with a local variable."
  (with-temp-buffer
    (insert-file-contents file)
    (string-match-p "no-byte-compile:[ \t]*t" (buffer-string))))

(defun eln-opt--elc-current-p (src)
  "Non-nil if the .elc next to SRC exists and is not older than SRC."
  (let ((elc (eln-opt--elc-for src)))
    (and (file-exists-p elc) (not (file-newer-than-file-p src elc)))))

(defun eln-opt--byte-candidates ()
  "Loaded .el files to byte-compile, most functions first: loaded from source
\(no .elc, or a stale one), under the trusted roots, defining functions, not
autoloads or init files, not opted out, and not failed before."
  (let (out)
    (dolist (entry load-history)
      (let* ((file (car entry))
             (defuns (and (stringp file) (string-suffix-p ".el" file)
                          (cl-count-if (lambda (x) (eq (car-safe x) 'defun)) (cdr entry)))))
        (when (and defuns (> defuns 0)
                   (file-readable-p file)
                   (eln-opt-rank--file-key file)
                   (not (eln-opt--init-file-p file))
                   (not (eln-opt--elc-current-p file))
                   (eln-opt-trusted-file-p file)
                   (not (eln-opt--no-byte-compile-p file))
                   (not (eln-opt--failed-p file))
                   (not (assoc file out)))
          (push (cons file defuns) out))))
    (mapcar #'car (sort out (lambda (a b) (> (cdr a) (cdr b)))))))

(defun eln-opt--byte-plan ()
  "The byte-compile candidates, after the lazy roots are loaded, so that files
behind a lazy package are seen.  The report and the worker both use this, so
they agree; without the load, a lazy package's files were missed."
  (eln-opt-rank--load-lazy-roots
   (plist-get (eln-opt-rank--roots (eln-opt--profile-dir)) :roots))
  (xl-perfetto-with-span "byte candidates" (eln-opt--byte-candidates)))

(defun eln-opt-worker-plan (&optional native)
  "The worker's plan, as (KIND . SOURCE) with KIND `byte' or `native'.
Byte-compiles come first: they are cheap, and interpreted code gains the most.
Native candidates follow only when NATIVE is non-nil, best ratio (score per KB)
first: small hot files before big ones.  At most
`eln-opt-worker-max' in all."
  (let ((bytes (seq-take (eln-opt--byte-plan) eln-opt-worker-max)))
    (append
     (mapcar (lambda (src) (cons 'byte src)) bytes)
     (and native
          (mapcar (lambda (src) (cons 'native src))
                  (eln-opt--plan-from-rows
                   (sort (copy-sequence
                          (plist-get (xl-perfetto-with-span "reach analysis" (eln-opt-rank--compute (eln-opt--profile-dir))) :rows))
                         (lambda (a b) (> (or (plist-get a :ratio) 0)
                                          (or (plist-get b :ratio) 0))))
                   (- eln-opt-worker-max (length bytes))))))))

;;; ---- First load: macOS checks each new native file once ----
;; macOS checks a native file the first time any process maps it: about 0.22 s
;; per new .eln, whatever its size, and the result is kept per file.  A GUI
;; start that is the first to load the files a worker just built pays it on
;; the main thread: 31 fresh .eln files took init from 0.9 s to 7.5 s.  So the
;; worker loads each new file once itself, in a throwaway `emacs -Q', and the
;; next start finds them checked.  A checked file costs 3 ms to load again.

(defconst eln-opt-warm-timeout 120
  "Seconds a warm-up child may run before it is stopped.")

(defun eln-opt--eln-files ()
  "Every .eln in the user's eln folder, trampolines included."
  (let ((dir (eln-opt--eln-dir)))
    (and dir (file-directory-p dir) (directory-files dir t "\\.eln\\'"))))

(defun eln-opt--warm (files)
  "Load each eln in FILES once in a throwaway `emacs -Q', on macOS only.
The point is the system's one-time check of a new native file, not the code:
`load-path' is nil, so a file's top level stops at its first `require', and
the child exits.  Its stdin is closed and it is stopped after
`eln-opt-warm-timeout' seconds.  Returns the number of files given to it."
  (let ((files (and (eq system-type 'darwin)
                    (seq-filter (lambda (f) (and f (file-exists-p f))) files))))
    (when files
      (let ((proc (xl-async-start
                   (list (expand-file-name invocation-name invocation-directory)
                         "-Q" "--batch" "--eval"
                         (format "(let ((load-path nil)) (dolist (f '%S) (ignore-errors (load f nil t t))))"
                                 files))
                   :name "eln-opt-warm"))
            (deadline (+ (float-time) eln-opt-warm-timeout)))
        (while (and (xl-async-live-p proc) (< (float-time) deadline))
          (accept-process-output proc 0.2))
        (eln-opt--kill proc)))
    (length files)))

(defun eln-opt--compile-one (kind src)
  "Compile SRC the KIND way inside a span named for the file, see `eln-opt--compile-one-1'."
  (xl-perfetto-with-span (format "%s compiling %s" kind (file-name-nondirectory src))
    (eln-opt--compile-one-1 kind src)))

(defun eln-opt--compile-one-1 (kind src)
  "Compile SRC the KIND way, `byte' or `native'.  Returns nil, or an error
string.  Success means Emacs will load the result on the next start."
  (condition-case e
      (if (eq kind 'byte)
          (let ((r (let ((byte-compile-verbose nil)) (byte-compile-file src))))
            (cond ((eq r 'no-byte-compile) "opts out with no-byte-compile")
                  ((not r) "byte-compile-file reported errors")
                  ((not (eln-opt--elc-current-p src)) "compiled, but no current .elc")))
        (native-compile (file-truename src))
        (unless (eln-opt--built-p src) "compiled, but no eln that Emacs will use"))
    (error (format "%S" e))))

(defun eln-opt--say (fmt &rest args)
  "Print one line for the main Emacs, which reads lines starting with ELN-OPT."
  (princ (concat "ELN-OPT " (apply #'format fmt args) "\n")))

(defun eln-opt-forced-plan (sources native)
  "The plan for an explicit request: byte-compile each trusted source, then
native-compile it when NATIVE.  Sources outside the trusted roots are refused
and reported; the failed list is not consulted, since the user asked."
  (let (plan)
    (dolist (src sources)
      (if (eln-opt-trusted-file-p src)
          (progn
            (push (cons 'byte src) plan)
            (when native (push (cons 'native src) plan)))
        (eln-opt--say "refused %s: not in a trusted folder" src)))
    (nreverse plan)))

(defun eln-opt--trace-follow ()
  "In the worker: record while the main Emacs records (see `xl-perfetto-follow-marker')."
  (xl-perfetto-follow-marker "eln-opt-trace"))

(defun eln-opt--trace-top-level (orig form &rest args)
  "Around `byte-compile-top-level' in the worker: one span per top-level form,
named for the defun it defines.  Costs one variable check when not recording."
  (if (not xl-perfetto--recording)
      (apply orig form args)
    (xl-perfetto-with-span (let ((head (car-safe form)))
                             (if (and (memq head '(defun defmacro defsubst cl-defun cl-defmacro))
                                      (consp (cdr form)) (symbolp (cadr form)))
                                 (format "byte %s" (cadr form))
                               (format "byte form %s" (or head "?"))))
      (apply orig form args))))

(defun eln-opt-worker (&optional sources)
  "Compile the plan one file at a time, sleeping between files.
Runs in a batch Emacs with the profile loaded, so package macros expand as
they should.  Without native compilation it only byte-compiles.  The first
error stops it, after putting that file on the failed list.  Returns the
number of files compiled, or `busy'.

With SOURCES, a list of .el files, compiles exactly those (byte, then
native), ignoring the failed list and the quiet state; the in-progress
marker and the trusted-root check still apply.

When the main Emacs records with perfetto, this run records too, into a
file named eln-opt-trace-*.json in the same folder."
  (eln-opt--trace-follow)
  (advice-add 'byte-compile-top-level :around #'eln-opt--trace-top-level)
  (unwind-protect
      (let ((count (xl-perfetto-with-span "eln-opt worker"
                     (eln-opt--worker-run sources))))
        count)
    (advice-remove 'byte-compile-top-level #'eln-opt--trace-top-level)
    (let ((file (xl-perfetto-follow-finish)))
      (when file (eln-opt--say "trace %s" file)))))

(defun eln-opt--worker-run (&optional sources)
  "Compile the plan one file at a time, sleeping between files.
Runs in a batch Emacs with the profile loaded, so package macros expand as
they should.  Without native compilation it only byte-compiles.  The first
error stops it, after putting that file on the failed list.  Returns the
number of files compiled, or `busy'.

With SOURCES, a list of .el files, compiles exactly those (byte, then
native), ignoring the failed list and the quiet state; the in-progress
marker and the trusted-root check still apply."
  (eln-opt--require-comp)
  (cond
   ((not (eln-opt--lock-acquire)) (eln-opt--say "busy") 'busy)
   (t
    (unwind-protect
        (let* ((native (eln-opt-platform-available-p))
               (refused (eln-opt--symlinked-p (eln-opt--marker-file)))
               (carry (and (not refused) (eln-opt--settle-interrupted)))
               (forced (and sources t))
               (plan (and (not refused)
                          (if forced
                              (eln-opt-forced-plan sources native)
                            (eln-opt-worker-plan native))))
               (done 0)
               (failures 0)
               (first t)
               (previous nil))
          (when refused
            (eln-opt--say "stopped: %s is a symbolic link; remove it and run again"
                          (eln-opt--marker-file)))
          ;; Nothing planned: quiet until something changes.  Anything planned
          ;; clears a quiet state from an earlier run.  A forced request says
          ;; nothing about the quiet state.
          (unless forced
            (if plan (eln-opt--clear-quiet) (eln-opt--mark-quiet)))
          (unless native (eln-opt--say "native compilation not available; byte-compile only"))
          (eln-opt--say "plan %d %s" (length plan)
                        (mapconcat (lambda (item)
                                     (concat (file-name-nondirectory (cdr item))
                                             (if (eq (car item) 'byte) "(byte)" "")))
                                   plan " "))
          (catch 'stop
            (dolist (item plan)
              ;; Pause between files, not between the byte and native steps
              ;; of one file: those run back to back.
              (unless (or first (equal (cdr item) previous))
                (sleep-for eln-opt-worker-sleep))
              (setq first nil
                    previous (cdr item))
              (eln-opt--trace-follow)
              (let* ((src (cdr item)) err
                     (attempts (if (and carry (equal (car carry) src))
                                   (1+ (cdr carry))
                                 1)))
                (eln-opt--say "start %s" src)
                (unless (eln-opt--mark-in-progress src attempts)
                  (eln-opt--say "stopped: cannot write the in-progress marker for %s; not compiled" src)
                  (throw 'stop nil))
                (setq err (eln-opt--compile-one (car item) src))
                (eln-opt--clear-marker)
                (if err
                    (progn
                      (eln-opt--record-failure src err)
                      (eln-opt--say "failed %s %s" src (eln-opt--short err))
                      (cl-incf failures))
                  (cl-incf done)
                  ;; Check the new file now, so the next start does not pay.
                  (when (eq (car item) 'native)
                    (xl-perfetto-with-span (format "warm new eln: %s" (file-name-nondirectory src))
                      (eln-opt--warm (list (eln-opt--eln-for src)))))
                  (eln-opt--say "done %s" src)))))
          ;; Also files built some other way, or before a worker was stopped
          ;; between the build and its warm-up.  Checked files cost 3 ms each.
          (when native
            (let ((n (xl-perfetto-with-span "warm all eln" (eln-opt--warm (eln-opt--eln-files)))))
              (when (> n 0) (eln-opt--say "warmed %d eln file(s)" n))))
          (eln-opt--say "finished %d%s" done
                        (if (> failures 0) (format ", failed %d" failures) ""))
          done)
      (eln-opt--lock-release)))))

(defun eln-opt-status ()
  "The user's eln count and the failed list."
  (list :elns (length (eln-opt--user-elns))
        :failed (eln-opt--failed-load)))

(defun eln-opt-reset ()
  "Delete every eln in the user's eln folder except the subr trampolines, and
the failed list and the quiet state.  With Emacs's JIT off, those elns are
eln-opt's.

Interactive: asks first, and the answer must be the word yes.  Batch (the
emacs-setup.sh command) does not ask: running that command is the
confirmation.  Returns the number of elns deleted, or nil when cancelled."
  (interactive)
  (eln-opt--require-comp)
  (if (and (not noninteractive)
           (not (yes-or-no-p
                 (format "Delete %d eln file(s) built by eln-opt, and the failed list? "
                         (length (eln-opt--user-elns))))))
      (progn (message "eln-opt: reset cancelled") nil)
    (let ((n 0))
      (dolist (eln (eln-opt--user-elns))
        (ignore-errors (comp-delete-or-replace-file eln))
        (cl-incf n))
      (ignore-errors (delete-file (eln-opt--failed-file)))
      (eln-opt--clear-marker)
      (eln-opt--clear-quiet)
      (unless noninteractive
        (message "eln-opt: deleted %d eln file(s)" n))
      n)))

;;; ---- Quiet: a worker that found nothing stops the next starts from looking ----

;; Most sessions find nothing new: the profile is tuned and stable.  Starting a
;; batch Emacs each time just to learn that wastes the machine.  So when the
;; worker finds no work it records a quiet state.  Each Emacs start checks the
;; fingerprint (cheap, no worker): if nothing changed, no worker starts.
;; `eln-opt-compile-now' is never held back.  Usage (savehist, recentf) is not in
;; the fingerprint: a new hot file from usage alone is picked up after a change
;; to the profile or the packages, or by `eln-opt-compile-now'.

(defun eln-opt--quiet-file ()
  (expand-file-name "quiet.eld" (eln-opt--version-dir)))

(defun eln-opt--own-stamps ()
  "Modification times of our own lisp files, for the quiet fingerprint."
  (when (bound-and-true-p xl-dotfiles)
    (let ((dir (expand-file-name "tuned-emacs/lisp" xl-dotfiles)))
      (delq nil
            (mapcar (lambda (f)
                      (let ((attrs (file-attributes f)))
                        (and attrs (cons f (file-attribute-modification-time attrs)))))
                    (append (and (file-directory-p dir)
                                 (directory-files dir t "\\.el\\'"))
                            (list (expand-file-name "tuned-emacs/xueliang.el" xl-dotfiles))))))))

(defun eln-opt--quiet-fingerprint ()
  "What a quiet state is about: this binary, the load path (package updates
change it) and our own lisp files (edits change them)."
  (md5 (format "%S" (list (eln-opt--binary-key) load-path (eln-opt--own-stamps)))))

(defun eln-opt--mark-quiet ()
  (eln-opt--write-eld (eln-opt--quiet-file)
                      (list :fingerprint (eln-opt--quiet-fingerprint)
                            :time (float-time))))

(defun eln-opt--clear-quiet ()
  (ignore-errors (delete-file (eln-opt--quiet-file))))

(defun eln-opt-auto--quiet-p ()
  "Non-nil when the last worker found nothing and nothing has changed since."
  (let ((q (eln-opt--read-eld (eln-opt--quiet-file))))
    (and q (equal (plist-get q :fingerprint) (eln-opt--quiet-fingerprint)))))

;;; ---- The main Emacs: start the worker once, show its log, watch it ----

;; The only part of eln-opt that runs in the main Emacs.  One idle timer, once
;; per session; no hooks.  New elns take effect on the next start.

(defgroup eln-opt nil
  "Background native compilation of the hot files."
  :group 'comp)

(defcustom eln-opt-auto-enable t
  "Non-nil means the running Emacs starts the eln-opt worker when idle."
  :type 'boolean)

(defcustom eln-opt-auto-idle 5
  "Seconds of idleness, once per session, before the worker starts."
  :type 'integer)

(defconst eln-opt-log-buffer "*eln-opt*"
  "The worker's output, and the buffer `eln-opt-compile-now' shows.")

(defvar eln-opt-auto--timer nil "The one-shot idle timer, while armed.")
(defvar eln-opt-auto--process nil "The worker, while it runs.")
(defvar eln-opt-auto--watchdog nil "The timer that watches the worker for stalls.")
(defvar eln-opt-auto--last-output 0 "When the worker last printed anything.")
(defvar eln-opt-auto--current nil "The source the worker is compiling now.")
(defvar eln-opt-auto--last-end nil "When the last worker ended, or nil.")

(defconst eln-opt-min-processors 2
  "Automatic workers start only on a machine with at least this many logical
processors.  On one, the worker competes with the session for the only core.
A manual run is not held back.")

(defun eln-opt-auto--decision (&optional force)
  "Nil when the worker may start now, otherwise a symbol saying why not.
FORCE (a manual run) ignores `eln-opt-auto-enable', the processor count and
the quiet state.  ELN_OPT=0 is the kill switch and always wins."
  (cond ((equal (getenv "ELN_OPT") "0") 'disabled)
        ((and (not force) (not eln-opt-auto-enable)) 'disabled)
        ((process-live-p eln-opt-auto--process) 'running)
        ((and (not force) (< (num-processors) eln-opt-min-processors)) 'single-core)
        ((and (not force) (eln-opt-auto--quiet-p)) 'quiet)
        (t nil)))

(defun eln-opt-auto--command (&optional sources)
  "The worker's command line: this binary and profile, at background priority
\(macOS `taskpolicy -b' puts it on the efficiency cores).  With SOURCES,
the worker compiles exactly those files."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory))
         (form (if sources
                   (format "(progn (require 'eln-opt) (eln-opt-worker '%S))" sources)
                 "(progn (require 'eln-opt) (eln-opt-worker))"))
         (emacs (list exe "--batch" "--init-directory" profile
                      "-l" (expand-file-name "early-init.el" profile)
                      "--eval" "(run-hooks (quote before-init-hook))"
                      "-l" (expand-file-name "init.el" profile)
                      "--eval" form)))
    (cond ((executable-find "taskpolicy") (append '("taskpolicy" "-b") emacs))
          ((executable-find "nice") (append '("nice" "-n" "19") emacs))
          (t emacs))))

(defun eln-opt-auto--note (text)
  "Track the file the worker is on, from the ELN-OPT lines in TEXT."
  (dolist (line (split-string text "\n" t))
    (cond ((string-match "\\`ELN-OPT start \\(.+\\)\\'" line)
           (setq eln-opt-auto--current (match-string 1 line)))
          ((string-match "\\`ELN-OPT \\(?:done\\|failed\\) " line)
           (setq eln-opt-auto--current nil)))))

(defun eln-opt-auto--filter (_proc text)
  (setq eln-opt-auto--last-output (float-time))
  (eln-opt-auto--note text)
  (when (buffer-live-p (get-buffer eln-opt-log-buffer))
    (with-current-buffer eln-opt-log-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert text))))))

(defun eln-opt-auto--summary (text)
  "One line saying what the worker did, from its output TEXT."
  (let ((done (eln-opt--count-matches "^ELN-OPT done " text))
        (failed (eln-opt--count-matches "^ELN-OPT failed " text)))
    (cond ((string-match-p "^ELN-OPT busy" text)
           "eln-opt: another worker holds the lock")
          ((> failed 0)
           (format "eln-opt: compiled %d file(s), %d failed (see %s)"
                   done failed eln-opt-log-buffer))
          ((string-match-p "^ELN-OPT finished" text)
           (if (zerop done)
               "eln-opt: nothing to compile"
             (format "eln-opt: compiled %d file(s); they load on the next start" done)))
          (t (format "eln-opt: the worker stopped early (see %s)" eln-opt-log-buffer)))))

(defun eln-opt--count-matches (regexp text)
  "How many times REGEXP matches in TEXT."
  (let ((n 0) (start 0))
    (while (string-match regexp text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defun eln-opt--kill (proc)
  "End PROC now: abort it as an `xl-async' task, then delete it if still live.
An abort runs no exit handler, so the caller ends the loader and the state."
  (when (process-live-p proc)
    (xl-async-abort proc)
    (when (process-live-p proc) (delete-process proc))))

(defun eln-opt-auto--stop-watchdog ()
  (when (timerp eln-opt-auto--watchdog) (cancel-timer eln-opt-auto--watchdog))
  (setq eln-opt-auto--watchdog nil))

(defconst eln-opt-auto-max-runs 60
  "Most automatic worker runs in one session.  Each run that compiles something
re-arms the idle timer, so a long backlog is worked off over several idle
periods; this cap keeps a session from running workers without end.")

(defvar eln-opt-auto--runs 0 "Automatic worker runs started in this session.")

(defun eln-opt-auto--exit (_proc)
  "The worker has exited normally: summarize its log and arm again if needed."
  (setq eln-opt-auto--last-end (float-time))
  (eln-opt-auto--stop-watchdog)
  (setq eln-opt-auto--process nil
        eln-opt-auto--current nil)
  (when (buffer-live-p (get-buffer eln-opt-log-buffer))
    (with-current-buffer eln-opt-log-buffer
      (xl-spinner-mode-line-stop)
      (let ((text (buffer-string)))
        (message "%s" (eln-opt-auto--summary text))
        ;; More may be waiting: compiled something, so arm again for the
        ;; next idle period.  A run that found nothing marks the quiet state.
        (when (and (> (eln-opt--count-matches "^ELN-OPT done " text) 0)
                   (< eln-opt-auto--runs eln-opt-auto-max-runs))
          (eln-opt-auto-arm))))))

(defun eln-opt-auto--watch ()
  "Kill a worker that has printed nothing for `eln-opt-worker-stall' seconds,
and put the file it was compiling on the failed list."
  (when (and (process-live-p eln-opt-auto--process)
             (> (- (float-time) eln-opt-auto--last-output) eln-opt-worker-stall))
    (when eln-opt-auto--current
      (eln-opt--record-failure eln-opt-auto--current
                               (format "stalled for %d s; killed" eln-opt-worker-stall)))
    (eln-opt--kill eln-opt-auto--process)))

(defun eln-opt-auto--start (&optional force sources)
  "Start the worker, unless eln-opt is off, runs already, or was quiet last
time (FORCE overrides those).  With SOURCES, the worker compiles exactly
those files.  Returns the process, or nil."
  (setq eln-opt-auto--timer nil)
  (unless (eln-opt-auto--decision force)
    (let ((buf (get-buffer-create eln-opt-log-buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)) (erase-buffer))
        (special-mode)
        (xl-spinner-mode-line-start "eln-opt worker"))
      (setq eln-opt-auto--last-output (float-time)
            eln-opt-auto--current nil
            eln-opt-auto--process
            (xl-async-start (eln-opt-auto--command sources)
                            :name "eln-opt-worker" :keep-input t))
      (xl-async-on-output eln-opt-auto--process #'eln-opt-auto--filter)
      (xl-async-on-exit eln-opt-auto--process #'eln-opt-auto--exit)
      (unless sources (cl-incf eln-opt-auto--runs))
      (eln-opt-auto--stop-watchdog)
      (setq eln-opt-auto--watchdog (run-with-timer 60 60 #'eln-opt-auto--watch))
      eln-opt-auto--process)))

(defun eln-opt-auto-arm ()
  "Once per session: when Emacs has been idle for `eln-opt-auto-idle' seconds,
start the worker.  Does nothing in batch, so setup, tests and the worker
itself (which loads this init too) never start one.  Without native
compilation the worker still byte-compiles."
  (when (timerp eln-opt-auto--timer) (cancel-timer eln-opt-auto--timer))
  (setq eln-opt-auto--timer nil)
  (when (and (not noninteractive) eln-opt-auto-enable)
    (setq eln-opt-auto--timer
          (run-with-idle-timer eln-opt-auto-idle nil #'eln-opt-auto--start))))

;;;###autoload
(defun eln-opt-stop-worker ()
  "Stop the background eln-opt worker now.
The file it was compiling keeps its in-progress marker, so the next run counts
that attempt as interrupted, as it would after a quit.  No automatic worker
starts again this session; `eln-opt-compile-now' still does."
  (interactive)
  (let ((proc eln-opt-auto--process))
    (cond
     ((process-live-p proc)
      (eln-opt-auto--stop-watchdog)
      (when (timerp eln-opt-auto--timer) (cancel-timer eln-opt-auto--timer))
      (setq eln-opt-auto--timer nil
            eln-opt-auto--process nil
            eln-opt-auto--current nil)
      ;; An abort ends the task without its exit handler, so the loader is
      ;; ended here.
      (when (buffer-live-p (get-buffer eln-opt-log-buffer))
        (with-current-buffer eln-opt-log-buffer
          (xl-spinner-mode-line-stop)))
      (eln-opt--kill proc)
      (message "eln-opt: worker stopped; the file it was compiling is retried next time"))
     (t (message "eln-opt: no worker is running")))))

(defconst eln-opt-manual-gap 30
  "Seconds after a worker ends before a manual start is allowed again.  Stops
repeated `eln-opt-compile-now' calls from starting one analysis after another.")

(defun eln-opt-auto--throttled-seconds ()
  "Seconds still to wait before a manual start, or nil when none is needed."
  (when eln-opt-auto--last-end
    (let ((left (- eln-opt-manual-gap (- (float-time) eln-opt-auto--last-end))))
      (and (> left 0) (ceiling left)))))

(defun eln-opt-compile-now ()
  "Start the worker now, or show the one that runs, with a loader in its log.
A worker that ended less than `eln-opt-manual-gap' seconds ago is not
started again yet.  Batch: runs the worker here."
  (interactive)
  (cond
   (noninteractive (eln-opt-worker))
   ((process-live-p eln-opt-auto--process)
    (display-buffer eln-opt-log-buffer))
   ((eln-opt-auto--throttled-seconds)
    (message "eln-opt: a worker ended a moment ago; try again in %d s"
             (eln-opt-auto--throttled-seconds)))
   (t
    (let ((proc (eln-opt-auto--start t)))
      (if proc
          (display-buffer eln-opt-log-buffer)
        (message "eln-opt: %s" (eln-opt-auto--decision t)))))))

;;; ---- An explicit request: compile the file of one function now ----

(defun eln-opt-function-source (function)
  "The .el source of the Lisp file that defines FUNCTION, or nil.
Errors name the reason: a C primitive has no Lisp file, and a function
with no file (defined in the scratch buffer, say) cannot be compiled."
  (let* ((fn (indirect-function function))
         (file (and fn (not (subrp fn)) (symbol-file function 'defun))))
    (cond
     ((null fn) (user-error "%s is not a function" function))
     ((subrp fn) (user-error "%s is a C primitive: it lives in the Emacs binary, not in a Lisp file" function))
     ((null file) (user-error "%s has no file to compile" function))
     (t (let ((src (eln-opt--source-for file)))
          (unless (and (stringp src) (file-exists-p src))
            (user-error "No source for %s: %s" function file))
          src)))))

(defun eln-opt-compile-function (function)
  "Compile now, in the background, the Lisp file that defines FUNCTION.
The file is byte-compiled and, where native compilation is available,
native-compiled, by the same worker as the automatic runs; it is watched
in the *eln-opt* buffer.  The result is used from the next start.  The
trusted-folder rule and the kill switch ELN_OPT=0 still apply.  The
failed list and the quiet state are ignored: the user asked for it."
  (interactive
   (list (intern (completing-read
                  "Compile the file of function: "
                  (let (names)
                    (mapatoms (lambda (s) (when (fboundp s) (push (symbol-name s) names))))
                    names)
                  nil t nil nil
                  (let ((here (or (function-called-at-point) (symbol-at-point))))
                    (and here (symbol-name here)))))))
  (let ((src (eln-opt-function-source function)))
    (cond
     ((process-live-p eln-opt-auto--process)
      (message "eln-opt: a worker is already running; see %s" eln-opt-log-buffer)
      (display-buffer eln-opt-log-buffer))
     (t
      (let ((proc (eln-opt-auto--start t (list src))))
        (if proc
            (progn
              (display-buffer eln-opt-log-buffer)
              (message "eln-opt: compiling %s in the background" (file-name-nondirectory src)))
          (message "eln-opt: %s" (eln-opt-auto--decision t))))))))

(provide 'eln-opt)
(provide 'eln-opt-rank)
(provide 'eln-opt-auto)

;;; eln-opt.el ends here
