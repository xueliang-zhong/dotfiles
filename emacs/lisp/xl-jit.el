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

(defun xl-jit--read-eld (file &optional max-bytes)
  "Read one Lisp form from FILE, or nil.  Only `read' is used, never `eval'.
Files larger than MAX-BYTES (default `xl-jit--max-state-bytes') are ignored."
  (when (and (file-regular-p file)
             (< (or (file-attribute-size (file-attributes file)) 0)
                (or max-bytes xl-jit--max-state-bytes)))
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

;;; ------------------------------------------------------------
;;; Step 4: ledger, lock, fingerprint, verification, audit
;;; (see xl-jit.md, "不会死循环的规则" and "安全")
;;; ------------------------------------------------------------

(defconst xl-jit-max-fails 2
  "Failures after which a source is left alone until its key changes.")

(defconst xl-jit-retry-interval (* 24 3600)
  "Seconds before a source that failed once may be tried again.")

(defconst xl-jit-lock-max-age 3600
  "Seconds after which a lock counts as stale even if its owner still exists.")

(defconst xl-jit--ledger-max-bytes (* 1024 1024)
  "Largest ledger xl-jit will read.")

(defun xl-jit--now () (float-time))

(defun xl-jit--version-dir ()
  "State folder for this Emacs build.  Ledgers never mix builds."
  (expand-file-name comp-native-version-dir xl-jit-state-dir))

(defun xl-jit--ledger-file () (expand-file-name "ledger.eld" (xl-jit--version-dir)))
(defun xl-jit--last-run-file () (expand-file-name "last-run.eld" (xl-jit--version-dir)))
(defun xl-jit--lock-dir () (expand-file-name "lock" (xl-jit--version-dir)))

(defun xl-jit--flags ()
  "Compile settings that change the output but not the eln file name."
  (list native-comp-speed native-comp-driver-options
        native-comp-compiler-options native-comp-debug))

(defun xl-jit--file-sha (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun xl-jit--file-size (file)
  (let ((attrs (file-attributes file)))
    (and attrs (file-attribute-size attrs))))

(defun xl-jit-source-key (src)
  "Ledger key for SRC: its truename, content hash and compile settings."
  (list (file-truename src) (xl-jit--file-sha (file-truename src)) (xl-jit--flags)))

;;; Ledger

(defun xl-jit--ledger-load ()
  "Entries of this build's ledger, as an alist (KEY . PLIST)."
  (let ((form (xl-jit--read-eld (xl-jit--ledger-file) xl-jit--ledger-max-bytes)))
    (and (equal (plist-get form :version) comp-native-version-dir)
         (seq-filter (lambda (e) (and (consp e) (consp (car e)) (listp (cdr e))))
                     (plist-get form :entries)))))

(defun xl-jit--ledger-save (entries)
  (xl-jit--write-eld (xl-jit--ledger-file)
                     (list :version comp-native-version-dir :entries entries)))

(defun xl-jit--ledger-get (key)
  (cdr (assoc key (xl-jit--ledger-load))))

(defun xl-jit--ledger-set (key plist)
  (xl-jit--ledger-save
   (cons (cons key plist) (assoc-delete-all key (xl-jit--ledger-load)))))

(defun xl-jit--ledger-remove (key)
  (xl-jit--ledger-save (assoc-delete-all key (xl-jit--ledger-load))))

(defun xl-jit--record-failure (key reason)
  "Count one more failure for KEY.  After `xl-jit-max-fails', it waits for a new key."
  (let ((fails (1+ (or (plist-get (xl-jit--ledger-get key) :fails) 0))))
    (xl-jit--ledger-set key (list :status 'failed :fails fails
                                  :reason reason :time (xl-jit--now)))))

(defun xl-jit--may-attempt-p (key now)
  "Non-nil if KEY should be compiled now.
Done entries are skipped while their eln exists.  A failed entry waits
`xl-jit-retry-interval' and is tried at most `xl-jit-max-fails' times.
Anything else (quarantined, unknown) is left alone."
  (let* ((e (xl-jit--ledger-get key))
         (status (plist-get e :status))
         (eln (plist-get e :eln)))
    (cond ((null e) t)
          ((eq status 'done) (not (and eln (file-exists-p eln))))
          ((eq status 'failed)
           (and (< (or (plist-get e :fails) 0) xl-jit-max-fails)
                (>= (- now (or (plist-get e :time) 0)) xl-jit-retry-interval)))
          (t nil))))

;;; Verification and audit

(defun xl-jit--child (form)
  "Evaluate FORM in a fresh batch Emacs without init.  Return (EXIT . OUTPUT).
A fresh process is the only honest test of loading: this process may already
hold a native version of the same function."
  (with-temp-buffer
    (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                nil t nil "-Q" "--batch" "--eval" (format "%S" form))))
      (cons status (buffer-string)))))

(defun xl-jit--first-defun (src)
  "Name of the first top-level defun in SRC, or nil.  SRC is read, not run."
  (with-temp-buffer
    (insert-file-contents src)
    (goto-char (point-min))
    (let ((read-eval nil) found)
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (consp form) (eq (car form) 'defun) (symbolp (cadr form)))
                (setq found (cadr form)))))
        (error nil))
      found)))

(defun xl-jit-verify (src eln)
  "Load ELN in a fresh process and check that a defun of SRC is native.
Return nil when it is, otherwise a string saying why not.  This is the only
test of \"done\": the file existing is not enough (see xl-jit.md)."
  (let ((name (xl-jit--first-defun src)))
    (if (null name)
        "no defun to check"
      (let* ((r (xl-jit--child
                 `(progn (load ,eln nil t)
                         (princ (if (native-comp-function-p (symbol-function ',name))
                                    "XL-JIT-OK" "XL-JIT-NOT-NATIVE")))))
             (out (string-trim (cdr r))))
        (cond ((/= (car r) 0) (format "child exited %s: %s" (car r) out))
              ((string-match-p "XL-JIT-OK\\'" out) nil)
              (t "loaded but not native"))))))

(defun xl-jit--discard-eln (eln)
  "Delete ELN with comp's own helper, ignoring errors."
  (ignore-errors (comp-delete-or-replace-file eln)))

(defun xl-jit--audit ()
  "Check every done entry before doing new work.
A missing eln drops its entry, so it can be built again.  An eln whose sha256
or size differs from the ledger, or that fails to load, is deleted and
counted as a failure."
  (dolist (entry (xl-jit--ledger-load))
    (let* ((key (car entry)) (p (cdr entry)) (eln (plist-get p :eln)))
      (when (eq (plist-get p :status) 'done)
        (cond
         ((not (and eln (file-exists-p eln)))
          (xl-jit--ledger-remove key))
         ((not (and (equal (xl-jit--file-sha eln) (plist-get p :sha))
                    (equal (xl-jit--file-size eln) (plist-get p :size))))
          (xl-jit--discard-eln eln)
          (xl-jit--record-failure key "sha256 or size differs from the ledger"))
         (t
          (let* ((r (xl-jit--child `(load ,eln nil t)))
                 (why (and (/= (car r) 0)
                           (format "load failed in a fresh process: %s"
                                   (string-trim (cdr r))))))
            (when why
              (xl-jit--discard-eln eln)
              (xl-jit--record-failure key why)))))))))

;;; Lock: taken by making a directory, which is atomic.  Never waits.

(defun xl-jit--owner-file (dir) (expand-file-name "owner.eld" dir))

(defun xl-jit--lock-stale-p (dir)
  (let* ((owner (xl-jit--read-eld (xl-jit--owner-file dir)))
         (pid (plist-get owner :pid))
         (time (plist-get owner :time)))
    (or (null owner)
        (not (and (integerp pid) (process-attributes pid)))
        (and time (> (- (xl-jit--now) time) xl-jit-lock-max-age)))))

(defun xl-jit--lock-take (dir)
  "Try to create DIR as the lock.  Return non-nil on success."
  (condition-case nil
      (progn
        (make-directory dir)
        (xl-jit--write-eld (xl-jit--owner-file dir)
                           (list :pid (emacs-pid) :time (xl-jit--now)))
        t)
    (file-already-exists nil)))

(defun xl-jit--lock-acquire ()
  "Return non-nil if this process now holds the lock.  A stale lock is
renamed away first, so two runners cannot both take over the same one."
  (let ((dir (xl-jit--lock-dir)))
    (make-directory (xl-jit--version-dir) t)
    (or (xl-jit--lock-take dir)
        (and (xl-jit--lock-stale-p dir)
             (condition-case nil
                 (progn (rename-file dir (make-temp-name (concat dir ".stale-")))
                        (xl-jit--lock-take dir))
               (file-error nil))))))

(defun xl-jit--lock-release ()
  "Remove the lock, but only if this process owns it."
  (let ((dir (xl-jit--lock-dir)))
    (when (equal (plist-get (xl-jit--read-eld (xl-jit--owner-file dir)) :pid)
                 (emacs-pid))
      (ignore-errors (delete-directory dir t)))))

;;; Fingerprint and the run itself

(defun xl-jit--mtime (file)
  (let ((attrs (file-attributes file)))
    (and attrs (file-attribute-modification-time attrs))))

(defun xl-jit--watch-files ()
  "Our own lisp sources, whose edits should trigger a new check."
  (when (bound-and-true-p xl-dotfiles)
    (let ((root (expand-file-name "emacs/" xl-dotfiles)))
      (cons (expand-file-name "xueliang.el" root)
            (directory-files (expand-file-name "lisp" root) t "\\.el\\'")))))

(defun xl-jit--fingerprint ()
  "Hash of the folder mtimes in `load-path' and of our own lisp sources.
The state folder is left out: the run itself changes it."
  (let* ((dirs (seq-filter (lambda (d) (and (stringp d) (file-directory-p d)))
                           load-path))
         (items (append
                 (mapcar (lambda (d) (list d (xl-jit--mtime d))) dirs)
                 (mapcar (lambda (f) (list f (xl-jit--file-size f) (xl-jit--mtime f)))
                         (xl-jit--watch-files)))))
    (secure-hash 'sha256 (prin1-to-string items))))

(defun xl-jit--next-eligible ()
  "Earliest time a failed entry may be tried again, or nil."
  (let (best)
    (dolist (e (xl-jit--ledger-load))
      (let ((p (cdr e)))
        (when (and (eq (plist-get p :status) 'failed)
                   (< (or (plist-get p :fails) 0) xl-jit-max-fails))
          (let ((at (+ (or (plist-get p :time) 0) xl-jit-retry-interval)))
            (setq best (if best (min best at) at))))))
    best))

(defun xl-jit--work ()
  "The compiling work.  Step 6 replaces this; for now nothing is compiled."
  (list :compiled 0))

(defun xl-jit--run-locked ()
  (let* ((now (xl-jit--now))
         (fp (xl-jit--fingerprint))
         (last (xl-jit--read-eld (xl-jit--last-run-file)))
         (next (plist-get last :next)))
    (if (and (equal fp (plist-get last :fingerprint))
             (or (null next) (< now next)))
        'unchanged
      (progn
        (xl-jit--audit)
        (let ((result (xl-jit--work)))
          (xl-jit--write-eld (xl-jit--last-run-file)
                             (list :fingerprint fp
                                   :next (xl-jit--next-eligible)
                                   :time (xl-jit--now)
                                   :result result))
          (cons 'ran result))))))

(defun xl-jit-run ()
  "One background run.  Return `unavailable', `busy', `unchanged', or (ran . RESULT).
Does nothing at all on a platform without native compilation."
  (cond
   ((not (xl-jit-platform-available-p)) 'unavailable)
   ((not (xl-jit--lock-acquire)) 'busy)
   (t (unwind-protect (xl-jit--run-locked)
        (xl-jit--lock-release)))))


;;; Step 4: reset (the kill switch's companion)

(defun xl-jit-reset ()
  "Delete only the eln files this build's ledger recorded, then the ledger.
Files xl-jit did not write are never touched.  Returns the number deleted."
  (let ((n 0))
    (dolist (e (xl-jit--ledger-load))
      (let ((eln (plist-get (cdr e) :eln)))
        (when (and eln (file-exists-p eln))
          (ignore-errors (delete-file eln))
          (cl-incf n))))
    (ignore-errors (delete-file (xl-jit--ledger-file)))
    (ignore-errors (delete-file (xl-jit--last-run-file)))
    n))

;;; Trusted roots: a file is compiled (or ranked for compiling) only if its
;;; truename lies under one of these.  Used by the report and by step 6.

(defun xl-jit-trusted-roots ()
  "Folders and files whose lisp may be compiled: elpa, Emacs's own lisp,
our lisp/ folder, and our xueliang.el.  A root ending in a slash is a folder."
  (delq nil
        (list (and (boundp 'package-user-dir) (file-name-as-directory package-user-dir))
              (and (boundp 'lisp-directory) (file-name-as-directory lisp-directory))
              (and (bound-and-true-p xl-dotfiles)
                   (file-name-as-directory (expand-file-name "emacs/lisp" xl-dotfiles)))
              (and (bound-and-true-p xl-dotfiles)
                   (expand-file-name "emacs/xueliang.el" xl-dotfiles)))))

(defun xl-jit-trusted-file-p (file)
  "Non-nil if FILE's truename is under a trusted root.  A symlink that leaves
the roots is refused, because its truename is what gets compiled."
  (let ((true (ignore-errors (file-truename
                              ;; load-history names the .elc once compiled.
                              (replace-regexp-in-string "\\.elc\\'" ".el" file)))))
    (and true
         (cl-some (lambda (root)
                    (let ((r (ignore-errors (file-truename root))))
                      (and r (if (string-suffix-p "/" root)
                                 (string-prefix-p r true)
                               (string= r true)))))
                  (xl-jit-trusted-roots)))))

(provide 'xl-jit)
;;; xl-jit.el ends here
