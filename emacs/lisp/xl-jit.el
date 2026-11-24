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
`defalias\=' is left out on purpose: (defalias \='x #\='subr) only aliases.")

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

(defun xl-jit--short (text)
  "The part of TEXT that says what went wrong, at most 300 characters."
  (let* ((start (or (string-match "Error:" text) 0))
         (piece (string-trim (substring text start))))
    (if (> (length piece) 300) (substring piece 0 300) piece)))

(defun xl-jit--child (form &optional profile)
  "Evaluate FORM in a fresh batch Emacs.  Return (EXIT . OUTPUT).
A fresh process is the only honest test of loading: this process may already
hold a native version of the same function.  It gets this process's
`load-path', because an eln can require its dependencies when it loads (winum
requires dash).  With PROFILE, it first loads that profile, because a file of
ours can depend on what init defines: xueliang.el uses `xl-leader-map'."
  (with-temp-buffer
    (let* ((exe (expand-file-name invocation-name invocation-directory))
           (init (and profile (expand-file-name "init.el" profile)))
           (args (if (and init (file-exists-p init))
                     (list "--batch" "--init-directory" profile
                           "-l" (expand-file-name "early-init.el" profile)
                           "--eval" "(run-hooks (quote before-init-hook))"
                           "-l" init)
                   (list "-Q" "--batch")))
           (status (apply #'call-process exe nil t nil
                          (append args
                                  (list "--eval"
                                        (format "%S" `(progn (setq load-path ',load-path)
                                                             ,form)))))))
      (cons status (buffer-string)))))

(defun xl-jit--child-in-profile (form)
  "`xl-jit--child' with this run's profile loaded first."
  (xl-jit--child form (xl-jit--profile-dir)))

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

(defun xl-jit-verify (src eln &optional names)
  "Load ELN in a fresh process and check that one of NAMES is native.
NAMES are the functions the ranking found in SRC.  Without them, the first
top-level defun of SRC is checked.  Return nil when it is, otherwise a string
saying why not.  This is the only test of \"done\": the file existing is not
enough (see xl-jit.md).  A file can define its functions through macros, so
its defuns need not be visible when read."
  (let ((names (or (seq-filter #'symbolp names)
                   (let ((first (xl-jit--first-defun src))) (and first (list first))))))
    (if (null names)
        "no function to check"
      (let* ((r (xl-jit--child-in-profile
                 `(progn (load ,eln nil t)
                         (princ (if (seq-some (lambda (s)
                                                (and (fboundp s)
                                                     (native-comp-function-p (symbol-function s))))
                                              ',names)
                                    "XL-JIT-OK" "XL-JIT-NOT-NATIVE")))))
             (out (string-trim (cdr r))))
        (cond ((/= (car r) 0) (format "child exited %s: %s" (car r) (xl-jit--short out)))
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
          (let* ((r (xl-jit--child-in-profile `(load ,eln nil t)))
                 (why (and (/= (car r) 0)
                           (format "load failed in a fresh process: %s"
                                   (xl-jit--short (cdr r))))))
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

;;; ------------------------------------------------------------
;;; Step 6: Tier 2, background native compilation
;;; (xl-jit.md, "流水线" steps 5-6, "安全", "DoS")
;;; ------------------------------------------------------------

(defconst xl-jit-compile-budget 3
  "Most files one run compiles.  Each costs a child Emacs with the profile.")

(defconst xl-jit-min-ratio 5.0
  "Lowest score/KB a file needs to be a candidate.  Filters only; the order of
the candidates is by score.  Uncalibrated: set it from a profiler run.")

(defconst xl-jit-max-source-bytes (* 200 1024)
  "Largest source file xl-jit will compile.")

(defconst xl-jit-max-total-bytes (* 200 1024 1024)
  "Most eln bytes xl-jit keeps in total.  Past it, runs compile nothing.")

(defconst xl-jit-compile-timeout 180
  "Seconds one child compile may take before it is killed and counted failed.")

(defvar xl-jit-crash-dir (expand-file-name "~/Library/Logs/DiagnosticReports/")
  "Where macOS writes crash reports.  Tests rebind it.")

(defconst xl-jit--crash-read-bytes (* 2 1024 1024)
  "Most of each crash report that is read.")

(defun xl-jit--profile-dir ()
  "The profile this run belongs to: the batch's user-emacs-directory."
  (file-name-as-directory user-emacs-directory))

(defun xl-jit--run-with-timeout (args seconds)
  "Run the Emacs binary with ARGS for at most SECONDS.
Return `ok' on exit 0, otherwise a string saying why not.  A child that runs
too long is killed, so a hung compile cannot hold the lock forever."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (buf (generate-new-buffer " *xl-jit-child*"))
         (proc (make-process :name "xl-jit-child" :buffer buf :noquery t
                             :command (cons exe args)))
         (deadline (+ (xl-jit--now) seconds)))
    (unwind-protect
        (progn
          (while (and (process-live-p proc) (< (xl-jit--now) deadline))
            (accept-process-output proc 0.5))
          (cond
           ((process-live-p proc)
            (signal-process proc 'KILL)
            (format "timed out after %d s" seconds))
           ((zerop (process-exit-status proc)) 'ok)
           (t (format "exit %d: %s" (process-exit-status proc)
                      (xl-jit--short
                       (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max))))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun xl-jit--child-compile (src eln)
  "Compile SRC to ELN in a child Emacs that loads the profile first.
The profile is needed: without its packages, macros such as evil's would be
compiled as calls.  `native-comp-speed' is capped at 2 (xl-jit.md, \"实测\")."
  (let* ((profile (xl-jit--profile-dir))
         (form `(progn (setq native-comp-speed (min 2 native-comp-speed))
                       (native-compile ,src ,eln))))
    (xl-jit--run-with-timeout
     (list "--batch" "--init-directory" profile
           "-l" (expand-file-name "early-init.el" profile)
           "--eval" "(run-hooks (quote before-init-hook))"
           "-l" (expand-file-name "init.el" profile)
           "--eval" (format "%S" form))
     xl-jit-compile-timeout)))

(defun xl-jit--read-text (file max-bytes)
  (when (file-readable-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 max-bytes)
      (buffer-string))))

(defun xl-jit--eln-bytes ()
  "Total size of the elns the ledger records as done."
  (let ((sum 0))
    (dolist (e (xl-jit--ledger-load))
      (when (eq (plist-get (cdr e) :status) 'done)
        (cl-incf sum (or (plist-get (cdr e) :size) 0))))
    sum))

(defun xl-jit--crash-scan (since)
  "Quarantine done elns that a crash report newer than SINCE names.
An eln named in a crash is deleted and its key quarantined for good: a
crash is not retried.  Returns the number quarantined."
  (let ((n 0))
    (when (file-directory-p xl-jit-crash-dir)
      (dolist (f (directory-files xl-jit-crash-dir t "\\`Emacs.*\\.ips\\'"))
        (let ((mtime (xl-jit--mtime f)))
          (when (and mtime (> (float-time mtime) since))
            (let ((text (or (xl-jit--read-text f xl-jit--crash-read-bytes) "")))
              (dolist (e (xl-jit--ledger-load))
                (let ((eln (plist-get (cdr e) :eln)))
                  (when (and eln (eq (plist-get (cdr e) :status) 'done)
                             (string-match-p (regexp-quote (file-name-nondirectory eln))
                                             text))
                    (xl-jit--discard-eln eln)
                    (xl-jit--ledger-set (car e)
                                        (list :status 'quarantined
                                              :reason "named in a crash report"
                                              :time (xl-jit--now)))
                    (cl-incf n)))))))))
    n))

(defun xl-jit--candidate-reason (row)
  "Why ranking ROW is not a candidate, as a string; nil if it is one.
Checked again here, not trusted from the ranking: the source is re-tested
against the trusted roots and the opt-out cookie, and must be a reasonable size."
  (let* ((path (plist-get row :path))
         (src (and path (replace-regexp-in-string "\\.elc\\'" ".el" path))))
    (cond
     ((null path) "no source path recorded")
     ((null (plist-get row :ratio)) "no ratio")
     ((< (plist-get row :ratio) xl-jit-min-ratio)
      (format "ratio %.1f is below %.1f" (plist-get row :ratio) xl-jit-min-ratio))
     ((not (file-readable-p src))
      (cond ((file-exists-p (concat src ".gz"))
             "Emacs ships only .el.gz for this library: not supported yet")
            ((string-suffix-p ".elc" path) "no .el source next to the .elc")
            (t "source not readable")))
     ((not (xl-jit-trusted-file-p src)) "outside the trusted roots")
     ((xl-jit-rank--no-native-p src) "opts out with no-native-compile")
     ((> (or (xl-jit--file-size src) 0) xl-jit-max-source-bytes)
      (format "source is %d KB, over the %d KB limit"
              (/ (xl-jit--file-size src) 1024) (/ xl-jit-max-source-bytes 1024)))
     (t nil))))

(defun xl-jit--candidate-p (row)
  "Non-nil if ranking ROW may be compiled."
  (null (xl-jit--candidate-reason row)))

(declare-function xl-jit-rank--compute "xl-jit-rank")
(declare-function xl-jit-rank--no-native-p "xl-jit-rank")

(defun xl-jit--compile-one (src &optional names)
  "Compile and verify SRC; NAMES are its functions from the ranking.
Return `done', `skipped', or `failed'."
  (let* ((true (file-truename src))
         (key (xl-jit-source-key true))
         (eln (comp-el-to-eln-filename true))
         (now (xl-jit--now)))
    (if (not (xl-jit--may-attempt-p key now))
        'skipped
      (let* ((built (xl-jit--child-compile true eln))
             (reason (cond ((not (eq built 'ok)) built)
                           ((not (file-exists-p eln)) "no eln written")
                           (t (xl-jit-verify true eln names)))))
        (if reason
            (progn
              (xl-jit--discard-eln eln)
              (xl-jit--record-failure key reason)
              'failed)
          (xl-jit--ledger-set key (list :status 'done :eln eln
                                        :sha (xl-jit--file-sha eln)
                                        :size (xl-jit--file-size eln)
                                        :time now))
          'done)))))

(defun xl-jit--work ()
  "One compile round: crash scan, then up to `xl-jit-compile-budget' files
from the ranking, if the total eln size allows it."
  (require 'xl-jit-rank)
  (let* ((last (xl-jit--read-eld (xl-jit--last-run-file)))
         (since (or (plist-get last :time) 0))
         (quarantined (xl-jit--crash-scan since))
         (capped (>= (xl-jit--eln-bytes) xl-jit-max-total-bytes))
         (compiled 0) (failed 0) (skipped 0))
    (unless capped
      (let* ((rows (plist-get (xl-jit-rank--compute (xl-jit--profile-dir)) :rows))
             (cands (seq-take (seq-filter #'xl-jit--candidate-p rows)
                              xl-jit-compile-budget)))
        (dolist (row cands)
          (let ((src (replace-regexp-in-string "\\.elc\\'" ".el" (plist-get row :path))))
            (pcase (xl-jit--compile-one src (plist-get row :defuns))
              ('done (cl-incf compiled))
              ('failed (cl-incf failed))
              (_ (cl-incf skipped)))))))
    (list :compiled compiled :failed failed :skipped skipped
          :quarantined quarantined :capped capped)))

(defun xl-jit-status ()
  "Counts by ledger status, eln bytes, and the last run's result."
  (let ((counts nil))
    (dolist (e (xl-jit--ledger-load))
      (let ((st (plist-get (cdr e) :status)))
        (setf (alist-get st counts 0) (1+ (alist-get st counts 0)))))
    (list :ledger counts
          :eln-bytes (xl-jit--eln-bytes)
          :eln-cap xl-jit-max-total-bytes
          :last-run (xl-jit--read-eld (xl-jit--last-run-file)))))

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

;;; Query: which functions are native-compiled in this Emacs

(defun xl-jit--source-stem (file)
  "FILE's truename without .elc or .el, for matching a loaded file to a source."
  (file-name-sans-extension
   (or (ignore-errors (file-truename file)) file)))

(defun xl-jit-native-list ()
  "Natively compiled functions in this Emacs, grouped by source file.
Returns a plist:
  :built    the files xl-jit built, from the ledger, as (SOURCE . ELN)
  :files    alist (FILE . SYMBOLS) for every file that has a native function,
            FILE being what `symbol-file' reports; sorted by file name
  :total    how many functions are native in all
Emacs's own preloaded native code is in :files too, so `:total' is large."
  (let ((built (let (out)
                 (dolist (e (xl-jit--ledger-load))
                   (when (and (eq (plist-get (cdr e) :status) 'done)
                              (plist-get (cdr e) :eln)
                              (file-exists-p (plist-get (cdr e) :eln)))
                     (push (cons (car (car e)) (plist-get (cdr e) :eln)) out)))
                 (sort out (lambda (a b) (string< (car a) (car b))))))
        (groups (make-hash-table :test #'equal))
        (total 0))
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (not (macrop s))
                  (ignore-errors (native-comp-function-p (indirect-function s))))
         (cl-incf total)
         (let ((file (or (symbol-file s 'defun) "(no file)")))
           (push s (gethash file groups))))))
    (let (files)
      (maphash (lambda (f syms) (push (cons f (sort syms #'string<)) files)) groups)
      (list :built built
            :files (sort files (lambda (a b) (string< (car a) (car b))))
            :total total))))

(defun xl-jit-native-report ()
  "Show the report of native functions: `xl-jit-report', in its own buffer."
  (interactive)
  (require 'xl-jit-rank)
  (if noninteractive
      (xl-jit-report)
    (xl-jit-rank--show-async "*xl-jit native*")))

(provide 'xl-jit)
;;; xl-jit.el ends here
