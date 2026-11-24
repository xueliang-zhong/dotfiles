;;; xl-jit.el --- tiered native compilation for Emacs, in the background -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Xueliang Zhong

;; Author: Xueliang Zhong <xueliang.zhong@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, compile, performance

;;; Commentary:

;; xl-jit makes the code you use often native-compiled, in a background Emacs,
;; without turning on Emacs's global JIT.  Nothing in the running session is
;; profiled or hooked except one timer (`xl-jit-auto-arm'):
;;
;; - `xl-jit-auto-arm' starts a background pass once Emacs has been open a
;;   while and the user is idle, at most once a day, never on battery.
;; - A pass (`xl-jit-run') ranks the hot files from byte code and the usage
;;   records (savehist, recentf), compiles the best few in child Emacs
;;   processes, verifies each eln, and records what it did in a ledger.
;; - New elns take effect on the next start.
;; - `xl-jit-report' and `xl-jit-native-report' show the ranking, what was built,
;;   and why each file was or was not compiled.
;;
;; Without native compilation (no libgccjit), nothing is ever scheduled.
;; The design notes are in emacs/ideas/xl-jit.md of the dotfiles repository.
;;
;; Requires xl-spinner and xl-async for the background views.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'comp nil t)
(require 'xl-spinner)
(require 'xl-async)

;; Design: emacs/ideas/xl-jit.md.  This file is loaded only by a background
;; batch run (see `xl-jit-platform-report' and emacs-setup.sh `jit'), never by
;; init.el.  Step 1 of the checklist: decide whether this Emacs binary can
;; compile native code at all, and remember the answer.

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
          ((eq status 'missing) t)
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
          ;; Recorded, not silently dropped: something outside xl-jit removed
          ;; the eln.  The next pass may build it again.
          (xl-jit--ledger-set key (list :status 'missing :time (xl-jit--now)
                                        :reason "eln file gone; removed outside xl-jit?")))
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

(defconst xl-jit-min-ratio 3.0
  "Lowest score/KB a file needs to be a candidate.  Filters only; the order of
the candidates is by score.  Uncalibrated: set it from a profiler run.")

(defconst xl-jit-max-source-bytes (* 500 1024)
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

(defun xl-jit-run-summary (result)
  "One readable line saying what RESULT of `xl-jit-run' did."
  (pcase result
    ('unavailable "xl-jit: native compilation is not available here; nothing done.")
    ('busy "xl-jit: another run holds the lock; nothing done.")
    ('unchanged "xl-jit: nothing changed since the last run; nothing done.")
    (`(ran . ,r)
     (let ((compiled (plist-get r :compiled))
           (failed (plist-get r :failed))
           (skipped (plist-get r :skipped))
           (quarantined (plist-get r :quarantined)))
       (format (concat "xl-jit: compiled %d file(s), failed %d, quarantined %d.\n"
                       "      %d candidate(s) were already built or waiting to retry.%s")
               compiled failed quarantined skipped
               (if (plist-get r :capped)
                   "\n      Stopped early: the total eln size is at its cap."
                 ""))))
    (_ (format "xl-jit: %S" result))))

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
  (if noninteractive
      (xl-jit-report)
    (xl-jit-rank--show-async "*xl-jit native*")))

;;; ---- Report: the ranking, the built files, the skips ----

;; Step 5 of emacs/ideas/xl-jit.md.  Nothing here compiles anything.  A child
;; Emacs with the profile loaded reads the byte code, the call graph, and the
;; usage records (savehist, recentf) as data, and prints a ranking.  Entry
;; points: `xl-jit-report' (M-x), and `emacs-setup.sh jit-report'.

;;; ------------------------------------------------------------
;;; Settings.  The weights are starting values; calibrate them against a
;;; profiler run before trusting the order (xl-jit.md, "校准").
;;; ------------------------------------------------------------

(defconst xl-jit-rank-heat-levels 4
  "How many call levels heat spreads through.")

(defconst xl-jit-rank-spread 0.5
  "Share of a caller's heat passed to each callee (divided by sqrt of callees).")

(defconst xl-jit-rank-loop-factor 4.0
  "Weight of code inside a loop, per nesting level.  A recursive function
counts as one level deeper.")

(defconst xl-jit-rank-savehist-cap 1.0
  "Heat of the most recent savehist command.  Older commands decay, and no
command can weigh more than this, so one habit cannot dominate.")

(defconst xl-jit-rank-always-heat 5.0
  "Heat of hooks that run on every keystroke or redisplay.")

(defconst xl-jit-rank-baseline-users 50
  "When this many savehist commands and recentf files exist, the baseline is
gone.  Below it the baseline's weight falls in step with the user's data.")

(defconst xl-jit-rank--inline-ops
  '(byte-car byte-cdr byte-cons byte-eq byte-plus byte-diff byte-add1 byte-sub1
    byte-lss byte-gtr byte-leq byte-geq byte-eqlsign byte-nth byte-nthcdr
    byte-setcar byte-setcdr byte-aref byte-aset byte-car-safe byte-cdr-safe
    byte-memq byte-consp byte-listp byte-not byte-mult byte-max byte-min
    byte-symbolp byte-stringp byte-integerp byte-numberp byte-length byte-elt
    byte-nreverse)
  "Opcodes native code runs much faster than byte code.  Count them.")

(defconst xl-jit-rank--buffer-ops
  '(byte-insert byte-goto-char byte-forward-line byte-point byte-char-after
    byte-following-char byte-preceding-char byte-buffer-substring
    byte-delete-region byte-skip-chars-forward byte-skip-chars-backward
    byte-forward-char byte-forward-word byte-insertN byte-point-max byte-point-min
    byte-set-buffer byte-save-excursion byte-save-restriction byte-narrow-to-region)
  "Opcodes whose time is spent in C.  Native code changes little.")

(defconst xl-jit-rank--c-heavy
  '(insert insert-before-markers re-search-forward re-search-backward
    search-forward search-backward looking-at looking-back string-match
    replace-match replace-regexp-in-string buffer-substring
    buffer-substring-no-properties put-text-property add-text-properties
    propertize all-completions try-completion completion-all-completions
    sort json-parse-string json-parse-buffer read format message
    call-process make-process process-send-string delete-region
    overlay-put make-overlay font-lock-ensure)
  "Functions called from Lisp whose time is spent in C.")

(defconst xl-jit-rank--alloc-heavy
  '(substring concat format split-string string-join string-trim
    make-string number-to-string mapconcat)
  "Calls that allocate.  Measured: native code gains little where they dominate
(xl-jit.md, \"实测\"), because time goes to allocation and GC.")

(defconst xl-jit-rank--dynamic-callers
  '(funcall apply run-hooks run-hook-with-args run-hook-with-args-until-success
    run-hook-with-args-until-failure run-hook-wrapped mapcar mapc mapcan
    seq-map seq-filter seq-do cl-mapcar))

;;; ------------------------------------------------------------
;;; Byte-code features of one function (nested closures included)
;;; ------------------------------------------------------------

(defun xl-jit-rank--lap (fn)
  (seq-filter #'consp
              (byte-decompile-bytecode (aref fn 1) (aref fn 2))))

(defun xl-jit-rank--closures (fn)
  "FN and every byte-code closure nested in its constants."
  (let ((out (list fn)))
    (mapc (lambda (c)
            (when (byte-code-function-p c)
              (setq out (nconc out (xl-jit-rank--closures c)))))
          (aref fn 2))
    out))

(defun xl-jit-rank--one (fn self)
  "Benefit and call sites of one byte-code object FN, nested closures excluded.
Returns (:benefit B :loops N :calls LIST :varrefs LIST :dynamic BOOL :size N)."
  (let* ((ops (vconcat (xl-jit-rank--lap fn)))
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
             (w (expt xl-jit-rank-loop-factor
                      (+ (aref depth i) (if recursive 1 0)))))
        (cond
         ((memq head xl-jit-rank--inline-ops) (cl-incf benefit w))
         ((memq head xl-jit-rank--buffer-ops) (cl-decf benefit (* 2 w)))
         ((eq head 'byte-varref)
          (let ((v (car-safe (cdr op)))) (when (symbolp v) (push v varrefs))))
         ((eq head 'byte-constant)
          (let ((c (car-safe (cdr op))))
            (when (and (symbolp c) c (fboundp c))
              (push c calls)
              (cond ((memq c xl-jit-rank--c-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c xl-jit-rank--alloc-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c xl-jit-rank--dynamic-callers)
                     (setq dynamic t) (cl-incf benefit (* 0.5 w)))
                    ((subrp (indirect-function c)) nil)
                    (t (cl-incf benefit (* 0.5 w))))))))))
    (list :benefit benefit :loops loops :calls calls :varrefs varrefs
          :dynamic dynamic :size (length (aref fn 1)))))

(defun xl-jit-rank--features (sym)
  "Summed features of SYM's definition and its nested closures."
  (let ((def (indirect-function sym))
        (b 0.0) (loops 0) (size 0) calls vars dyn)
    (when (byte-code-function-p def)
      (dolist (f (xl-jit-rank--closures def))
        (let ((r (xl-jit-rank--one f sym)))
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

(defun xl-jit-rank--file-key (file)
  "Name used in the ranking: basename without .elc, .el or .gz.
Returns nil for autoload files, which only hold stubs."
  (let ((base (file-name-nondirectory file)))
    (unless (string-match-p "-autoloads\\.elc?\\'" base)
      (replace-regexp-in-string "\\.\\(elc\\|el\\)\\(\\.gz\\)?\\'" "" base))))

(defvar xl-jit-rank--no-native-cache (make-hash-table :test #'equal))

(defun xl-jit-rank--no-native-p (file)
  "Non-nil if FILE opts out of native or byte compilation with a local variable."
  (let ((true (or (ignore-errors (file-truename file)) file)))
    (let ((hit (gethash true xl-jit-rank--no-native-cache 'unknown)))
      (if (not (eq hit 'unknown))
          hit
        (puthash true
                 (and (file-readable-p true)
                      (with-temp-buffer
                        (insert-file-contents true)
                        (let ((text (buffer-string)))
                          (or (string-match-p "no-native-compile:[ \t]*t" text)
                              (string-match-p "no-byte-compile:[ \t]*t" text)))))
                 xl-jit-rank--no-native-cache)))))

;;; ------------------------------------------------------------
;;; Graph: functions, call edges, and roots with evidence weights
;;; ------------------------------------------------------------

(defun xl-jit-rank--fn-values (v)
  "Functions named by the value of variable V: one function or a list of them."
  (when (and (symbolp v) (boundp v))
    (let ((val (symbol-value v)))
      (cond ((and (symbolp val) val (fboundp val)) (list val))
            ((and (consp val) (proper-list-p val))
             (seq-filter (lambda (x) (and (symbolp x) x (not (eq x t)) (fboundp x)))
                         val))))))

(defun xl-jit-rank--read-file (file)
  (when (file-readable-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun xl-jit-rank--graph (profile-dir)
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
    (dolist (entry load-history)
      (let* ((file (car entry))
             (key (and (stringp file) (xl-jit-rank--file-key file))))
        (when key
          (dolist (d (cdr entry))
            (when (and (consp d) (eq (car d) 'defun) (symbolp (cdr d))
                       (fboundp (cdr d)))
              (let ((sym (cdr d)))
                (cond
                 ((native-comp-function-p (indirect-function sym))
                  (cl-incf native-skipped))
                 ((xl-jit-rank--no-native-p file)
                  (cl-pushnew (cons key "no-native-compile or no-byte-compile") excluded
                              :test #'equal))
                 ;; Interpreted functions are skipped, not compiled here: the
                 ;; report must have no side effects.  Our own lisp is byte
                 ;; code once compile-lisp has run.
                 ((byte-code-function-p (indirect-function sym))
                  (puthash sym key file-of)
                  (puthash sym file path-of)
                  (puthash sym (xl-jit-rank--features sym) features))
                 (t (cl-incf interpreted-skipped)))))))))
    ;; Edges: constants, plus function-valued variables read by funcall/apply.
    (maphash
     (lambda (sym feat)
       (dolist (c (plist-get feat :calls))
         (unless (eq c sym) (puthash sym (cons c (gethash sym edges)) edges)))
       (when (plist-get feat :dynamic)
         (dolist (v (plist-get feat :varrefs))
           (dolist (f (xl-jit-rank--fn-values v))
             (unless (eq f sym) (puthash sym (cons f (gethash sym edges)) edges))))))
     features)
    ;; Advice: calling the original also runs its advice.
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (advice--p (advice--symbol-function s)))
         (advice-mapc (lambda (f _) (when (and (symbolp f) (not (eq f s)))
                                      (puthash s (cons f (gethash s edges)) edges)))
                      s))))
    (list :features features :file file-of :path path-of :edges edges
          :excluded excluded :native-skipped native-skipped
          :interpreted-skipped interpreted-skipped
          :profile profile-dir)))

(defun xl-jit-rank--roots (profile-dir)
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
      (let* ((text (xl-jit-rank--read-file (expand-file-name "history" profile-dir)))
             (cmds (and text (string-match "(setq extended-command-history '" text)
                        (seq-filter #'stringp
                                    (ignore-errors
                                     (car (read-from-string text (match-end 0))))))))
        (cl-loop for c in cmds for i from 0
                 do (let ((sym (intern-soft c))
                          (w (min xl-jit-rank-savehist-cap (/ 1.0 (+ 1 (* 0.05 i))))))
                      (when (and sym (fboundp sym))
                        (push (cons sym w) savehist))
                      (add sym w)))
        (cl-incf user-count (length cmds)))
      ;; 2. Modes the user edits.  recentf.eld is data: read, never eval'd.
      (let* ((text (xl-jit-rank--read-file (expand-file-name "recentf.eld" profile-dir)))
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
             (dolist (f (xl-jit-rank--fn-values (intern-soft (format "%s-hook" mode))))
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
        (dolist (f (append (xl-jit-rank--fn-values h)
                           (let ((d (and (boundp h) (default-value h))))
                             (and (proper-list-p d) (seq-filter #'symbolp d)))))
          (add f xl-jit-rank-always-heat)))
      ;; 4. Global keymap commands: weak evidence.
      (when (keymapp global-map)
        (map-keymap (lambda (_ b) (when (symbolp b) (add b 0.05))) global-map)))
    (list :roots (let (l) (maphash (lambda (k v) (push (cons k v) l)) roots) l)
          :savehist savehist
          :user-count user-count)))

(defun xl-jit-rank--baseline (profile-dir)
  "The shipped baseline roots, weighted down as the user's own data grows.
Returns (ROOTS . FACTOR).  The baseline file names functions only."
  (let* ((file (or (and (bound-and-true-p xl-dotfiles)
                        (expand-file-name "emacs/jit-baseline.eld" xl-dotfiles))
                   (expand-file-name "jit-baseline.eld" profile-dir)))
         (form (xl-jit--read-eld file))
         (names (plist-get form :functions)))
    (cons (and names
               (seq-filter (lambda (s) (and (symbolp s) (fboundp s))) names))
          nil)))

(defun xl-jit-rank--propagate (seeds edges)
  "Heat per symbol after spreading SEEDS, an alist (SYM . WEIGHT), along EDGES."
  (let ((heat (make-hash-table :test #'eq))
        (frontier seeds))
    (dolist (s seeds)
      (puthash (car s) (+ (cdr s) (gethash (car s) heat 0.0)) heat))
    (dotimes (_ xl-jit-rank-heat-levels)
      (let ((next (make-hash-table :test #'eq)))
        (dolist (fw frontier)
          (let* ((outs (gethash (car fw) edges))
                 (share (/ (* xl-jit-rank-spread (cdr fw))
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

(defun xl-jit-rank--file-scores (heat graph)
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

(defconst xl-jit-rank-load-min-weight 0.5
  "A root this heavy that is still an autoload gets its package loaded, so its
functions are in the graph.  Lazy packages (org, for one) are otherwise invisible.")

(defun xl-jit-rank--load-lazy-roots (roots)
  "Load the packages behind ROOTS that are still autoloads and heavy enough.
Returns the feature or file names loaded, for the report."
  (let (loaded)
    (dolist (r roots)
      (let ((s (car r)))
        (when (and (>= (cdr r) xl-jit-rank-load-min-weight)
                   (fboundp s)
                   (autoloadp (symbol-function s)))
          (let ((file (cadr (symbol-function s))))
            (when (ignore-errors (autoload-do-load (symbol-function s) s) t)
              (push (if (stringp file) (file-name-nondirectory file) (symbol-name s))
                    loaded))))))
    (delete-dups loaded)))

(defun xl-jit-rank--compute (profile-dir)
  "Full analysis.  Returns a plist: :rows (sorted), :savehist-share, and the
numbers for the header.  Roots are found first, so that lazy packages behind
heavy roots can be loaded before the graph is built."
  (let* ((roots-info (xl-jit-rank--roots profile-dir))
         (user-roots (plist-get roots-info :roots))
         (lazy-loaded (xl-jit-rank--load-lazy-roots user-roots))
         (graph (xl-jit-rank--graph profile-dir))
         (edges (plist-get graph :edges))
         (user-count (plist-get roots-info :user-count))
         (baseline (car (xl-jit-rank--baseline profile-dir)))
         (base-factor (max 0.0 (- 1.0 (/ (float user-count) xl-jit-rank-baseline-users))))
         (base-roots (mapcar (lambda (s) (cons s (* base-factor 1.0))) baseline))
         (seeds (append user-roots base-roots))
         (heat (xl-jit-rank--propagate seeds edges))
         (scores (xl-jit-rank--file-scores heat graph))
         (total (let ((sum 0.0)) (maphash (lambda (_ v) (cl-incf sum v)) scores) sum))
         ;; Each savehist command's share of the whole score.  Heat is linear,
         ;; so one command's contribution is its own propagation.
         (savehist-cmds (plist-get roots-info :savehist))
         (max-share 0.0))
    (when (> total 0)
      (dolist (r savehist-cmds)
        (let* ((h (xl-jit-rank--propagate (list r) edges))
               (s (xl-jit-rank--file-scores h graph))
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
                                    (and p (xl-jit-trusted-file-p p))))
                   rows))))
       scores)
      (list :rows (sort rows (lambda (a b) (> (plist-get a :score) (plist-get b :score))))
            :lazy-loaded lazy-loaded
            :built-rows (xl-jit-rank--built-rows)
            :next (seq-take (seq-filter #'xl-jit--candidate-p
                                        (sort (copy-sequence rows)
                                              (lambda (a b) (> (plist-get a :score) (plist-get b :score)))))
                            xl-jit-compile-budget)
            :ledger (xl-jit-rank--ledger-summary)
            :savehist-share max-share
            :functions (hash-table-count (plist-get graph :features))
            :roots (length user-roots)
            :user-count user-count
            :baseline-roots (length baseline)
            :baseline-factor base-factor
            :excluded (plist-get graph :excluded)
            :native-skipped (plist-get graph :native-skipped)
            :interpreted-skipped (plist-get graph :interpreted-skipped)))))

(defun xl-jit-rank--built-rows ()
  "One row per file xl-jit built and that is native now.
These files drop out of the ranking, because their functions are native, so
they are listed here with native \"xl-jit\".  Their KB is the eln size."
  (let* ((info (xl-jit-native-list))
         (natives (plist-get info :files))
         rows)
    (dolist (b (plist-get info :built))
      (let* ((src (car b))
             (eln (cdr b))
             (stem (xl-jit--source-stem src))
             (fns (cl-loop for (file . syms) in natives
                           when (equal (xl-jit--source-stem file) stem)
                           append syms)))
        (push (list :file (xl-jit-rank--file-key src) :native "xl-jit"
                    :path src :trusted t :ratio nil :score nil
                    :size (or (xl-jit--file-size eln) 0)
                    :count (length fns)
                    :top (seq-take fns 3))
              rows)))
    (sort rows (lambda (a b) (string< (plist-get a :file) (plist-get b :file))))))

(defun xl-jit-rank--line (rank score kb trusted native file top)
  "One report row.  Every row, ranked or built, uses this one layout."
  (format "%4s  %6s  %6s  %-7s  %-6s  %-28s  %s\n"
          rank score kb trusted native file top))

(defun xl-jit-rank--ledger-summary ()
  "Every ledger entry as a plist, newest first, with the source name."
  (let (out)
    (dolist (e (xl-jit--ledger-load))
      (let ((p (cdr e)))
        (push (list :file (file-name-nondirectory (car (car e)))
                    :status (plist-get p :status)
                    :time (plist-get p :time)
                    :fails (or (plist-get p :fails) 0)
                    :reason (plist-get p :reason)
                    :kb (and (plist-get p :size) (/ (plist-get p :size) 1024.0)))
              out)))
    (sort out (lambda (a b) (> (or (plist-get a :time) 0) (or (plist-get b :time) 0))))))

(defun xl-jit-rank--when (time)
  (if time (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time time)) "-"))

(defun xl-jit-rank--format (result)
  "The report as a string.  Sections, in order: what the next pass does, what
was compiled, what failed, why each ranked file was skipped, what is left out
altogether.  The ranking table comes last."
  (let* ((built (plist-get result :built-rows))
         (next (plist-get result :next))
         (ledger (plist-get result :ledger))
         (done (seq-filter (lambda (e) (eq (plist-get e :status) 'done)) ledger))
         (bad (seq-filter (lambda (e) (not (eq (plist-get e :status) 'done))) ledger))
         (rows (plist-get result :rows)))
    (with-output-to-string
      ;; 1. The answer
      (princ "xl-jit report (report only; nothing is compiled here)\n\n")
      (princ "Next background pass compiles: ")
      (if (null next)
          (princ "nothing.\n")
        (princ (format "%d file(s), in this order: %s\n" (length next)
                       (mapconcat (lambda (r) (plist-get r :file)) next ", "))))
      ;; 2. What was compiled
      (princ (format "\nCompiled by xl-jit and native now: %d file(s), %d function(s)\n"
                     (length built) (apply #'+ (mapcar (lambda (r) (plist-get r :count)) built))))
      (when built
        (princ (format "\n%-28s  %6s  %-7s  %-6s  %9s  %s\n"
                       "file" "KB" "trusted" "native" "functions" "examples"))
        (dolist (row built)
          (princ (format "%-28s  %6.1f  %-7s  %-6s  %9d  %S\n"
                         (plist-get row :file)
                         (/ (plist-get row :size) 1024.0)
                         "yes" "yes"
                         (plist-get row :count)
                         (plist-get row :top)))))
      (when done
        (princ "\nEntries in the ledger, status done (time of the build):\n")
        (dolist (e done)
          (princ (format "  %-30s %s  %5.1f KB\n" (plist-get e :file)
                         (xl-jit-rank--when (plist-get e :time))
                         (or (plist-get e :kb) 0)))))
      ;; 3. What failed or was held back
      (when bad
        (princ "\nFailed or quarantined (not retried until the key changes, or for a day):\n")
        (dolist (e bad)
          (princ (format "  %-30s %-11s %s  fails %d\n    %s\n"
                         (plist-get e :file) (plist-get e :status)
                         (xl-jit-rank--when (plist-get e :time))
                         (plist-get e :fails)
                         (or (plist-get e :reason) "")))))
      ;; 4. Why each ranked file is or is not compiled
      (princ "\nRanked files not yet native, and what happens to each:\n")
      (let ((n 0))
        (dolist (row rows)
          (let ((why (if (memq row next)
                         "compile in the next pass"
                       (or (xl-jit--candidate-reason row)
                           "candidate, but past the budget of 3 per pass"))))
            (when (< n 40)
              (princ (format "  %-28s score %6.1f  %s\n"
                             (plist-get row :file) (plist-get row :score) why)))
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
                     (* 100 (plist-get result :savehist-share))))
      ;; 6. The table
      (princ "\nRanking (score: expected gain; KB: bytecode size, the cost):\n")
      (princ (xl-jit-rank--line "rank" "score" "KB" "trusted" "native" "file" "[top functions]"))
      (cl-loop for row in (seq-take rows 25) for i from 1
               do (princ (xl-jit-rank--line
                          i
                          (format "%.1f" (plist-get row :score))
                          (format "%.1f" (/ (plist-get row :size) 1024.0))
                          (if (plist-get row :trusted) "yes" "NO")
                          "no" (plist-get row :file)
                          (format "%S" (plist-get row :top))))))))

;;; The report as a buffer.  The ranking takes seconds, so it runs in a child
;;; Emacs that loads the profile and prints this same text; the buffer shows a
;;; spinner in its mode line until the child answers (as GBlame does).

(defconst xl-jit-rank--marker "<<XL-JIT-REPORT>>"
  "Printed on its own line before a child's report.  The child's own loading
messages also go to its output, so the parent keeps only what follows it.")

(defvar xl-jit-rank--with-marker nil
  "Non-nil in the child: print `xl-jit-rank--marker' before the report.")

(defun xl-jit-rank--report-from-output (output)
  "The report in a child's OUTPUT: what follows the marker, or nil."
  (let ((at (string-match (regexp-quote xl-jit-rank--marker) output)))
    (and at (string-trim-left
             (substring output (+ at (length xl-jit-rank--marker))) "\n+"))))

(defun xl-jit-rank--child-command ()
  "The child that prints the report: this binary, this profile, the same
init, then `xl-jit-report' in batch."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory)))
    (list exe "--batch" "--init-directory" profile
          "-l" (expand-file-name "early-init.el" profile)
          "--eval" "(run-hooks (quote before-init-hook))"
          "-l" (expand-file-name "init.el" profile)
          "--eval" "(progn (require (quote xl-jit)) (setq xl-jit-rank--with-marker t) (xl-jit-report))")))

(defvar-local xl-jit-rank--center nil
  "The overlay that shows the loader in the middle of the window, while waiting.")

(defun xl-jit-rank--draw-center (frame)
  "Draw FRAME in the middle of this buffer's window while the report is pending.
nil removes it.  The loader is a `before-string' on an empty placeholder line,
so no text is added and the window does not move, as in GBlame."
  (if (null frame)
      (when (overlayp xl-jit-rank--center)
        (delete-overlay xl-jit-rank--center)
        (setq xl-jit-rank--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  computing the ranking"))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (pos (save-excursion
                    (goto-char (window-start window))
                    (forward-line (/ (window-body-height window) 2))
                    (line-beginning-position))))
        (unless (overlayp xl-jit-rank--center)
          (setq xl-jit-rank--center (make-overlay pos pos)))
        (move-overlay xl-jit-rank--center pos pos)
        (overlay-put xl-jit-rank--center 'before-string
                     (concat (make-string pad ?\s) text))))))

(defun xl-jit-rank--show-async (buffer-name)
  "Fill BUFFER-NAME with the report, computed in a child Emacs.
While it computes, a loader turns in the mode line and in the middle of the
window, on empty placeholder lines, as GBlame does."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Empty placeholder lines: the loader sits on one of them.
        (insert (make-string 400 ?\n)))
      (special-mode)
      (xl-spinner-mode-line-start "xl-jit report" :also #'xl-jit-rank--draw-center)
      (xl-async-run
       :name "xl-jit-report"
       :owner buf
       :slot 'xl-jit-report
       :command (xl-jit-rank--child-command)
       :on-done
       (lambda (output code)
         (xl-spinner-mode-line-stop)
         (xl-jit-rank--draw-center nil)
         (let ((report (xl-jit-rank--report-from-output output))
               (inhibit-read-only t))
           (erase-buffer)
           (if (and (eql code 0) report (not (string-empty-p (string-trim report))))
               (insert report)
             (insert "The report failed.\n\n" output))
           (goto-char (point-min))))))
    (display-buffer buf)
    buf))

(defun xl-jit-report ()
  "Show the ranking.  Interactive: a buffer, filled in the background.
Batch: printed to stdout, computed here."
  (interactive)
  (if noninteractive
      (progn
        (when xl-jit-rank--with-marker
          (princ (format "\n%s\n" xl-jit-rank--marker)))
        (princ (xl-jit-rank--format (xl-jit-rank--compute (xl-jit--profile-dir)))))
    (xl-jit-rank--show-async "*xl-jit report*")))

;;; ---- Trigger: when a background pass starts ----

;; The only part of xl-jit that lives in the main Emacs (emacs/ideas/xl-jit.md).
;; It uses timers only, no hooks.  After Emacs has been open for
;; `xl-jit-auto-uptime', and the user has then been idle for `xl-jit-auto-idle',
;; it starts one background batch Emacs that runs `xl-jit-run' (lock,
;; fingerprint, audit, compile) at background priority.  Nothing is loaded into
;; this Emacs: new elns take effect on the next start.  Without native
;; compilation (no libgccjit), no timer is ever armed.

;; M-x entry points defined in xl-jit.el; autoloaded so startup does not load it.

(defgroup xl-jit nil
  "Background native compilation of the hot files."
  :group 'comp)

(defcustom xl-jit-auto-enable t
  "Non-nil means the running Emacs starts xl-jit in the background."
  :type 'boolean)

(defcustom xl-jit-auto-uptime (* 10 60)
  "Seconds Emacs must have been open before a background run is considered.
A short-lived Emacs (a quick edit, `emacsclient -a \"\"') never starts one."
  :type 'integer)

(defcustom xl-jit-auto-idle 60
  "Seconds of idleness after which the background run starts."
  :type 'integer)

(defcustom xl-jit-auto-interval (* 24 3600)
  "Least time between two background runs, from the last run's record."
  :type 'integer)

(defcustom xl-jit-auto-retry (* 30 60)
  "When a run is not allowed now (on battery, one still running), check again
after this many seconds."
  :type 'integer)

(defvar xl-jit-auto--timer nil "The pending timer, idle or not.")
(defvar xl-jit-auto--process nil "The background batch Emacs, while it runs.")
(defvar xl-jit-auto-last-result nil
  "What the last background run printed, for example (ran :compiled 1 ...).")

;;; Conditions

(defun xl-jit-auto--last-run-file ()
  "The record the background run writes.  Its mtime says when it last ran."
  (expand-file-name (concat "xl-jit/" comp-native-version-dir "/last-run.eld")
                    user-emacs-directory))

(defun xl-jit-auto--seconds-since-last-run ()
  "Seconds since the last recorded run, or nil if there is none."
  (let ((attrs (and (boundp 'comp-native-version-dir)
                    (file-attributes (xl-jit-auto--last-run-file)))))
    (and attrs (float-time (time-since (file-attribute-modification-time attrs))))))

(defun xl-jit-auto--on-battery-p ()
  "Non-nil when the machine runs on battery.  Unknown counts as not."
  (require 'battery nil t)
  (let* ((fn (bound-and-true-p battery-status-function))
         (line (and fn (ignore-errors (cdr (assq ?L (funcall fn)))))))
    (and (stringp line)
         (string-match-p "\\`\\(bat\\|off-line\\)" (downcase line)))))

(defun xl-jit-auto--uptime ()
  (float-time (time-since before-init-time)))

(defun xl-jit-auto--decision ()
  "Nil when a background run may start now, otherwise a symbol saying why not.
`disabled' and `unavailable' are final for this session; the others are
retried later."
  (let ((since (xl-jit-auto--seconds-since-last-run)))
    (cond ((or (not xl-jit-auto-enable) (equal (getenv "XL_JIT") "0")) 'disabled)
          ((not (and (fboundp 'native-comp-available-p) (native-comp-available-p)))
           'unavailable)
          ((process-live-p xl-jit-auto--process) 'running)
          ((< (xl-jit-auto--uptime) xl-jit-auto-uptime) 'too-early)
          ((and since (< since xl-jit-auto-interval)) 'recent)
          ((xl-jit-auto--on-battery-p) 'battery)
          (t nil))))

(defun xl-jit-auto--delay (why)
  "Seconds to wait before checking again after WHY, or nil to stop."
  (pcase why
    ((or 'disabled 'unavailable) nil)
    ('too-early (max 1 (- xl-jit-auto-uptime (xl-jit-auto--uptime))))
    ('recent (max xl-jit-auto-retry
                  (- xl-jit-auto-interval (or (xl-jit-auto--seconds-since-last-run) 0))))
    (_ xl-jit-auto-retry)))

;;; Timers

(defun xl-jit-auto--cancel ()
  (when (timerp xl-jit-auto--timer) (cancel-timer xl-jit-auto--timer))
  (setq xl-jit-auto--timer nil))

(defun xl-jit-auto--schedule (seconds)
  (xl-jit-auto--cancel)
  (setq xl-jit-auto--timer (run-with-timer seconds nil #'xl-jit-auto--tick)))

(defun xl-jit-auto--tick ()
  "Check the conditions.  If they hold, wait for the user to go idle."
  (xl-jit-auto--cancel)
  (let ((why (xl-jit-auto--decision)))
    (if (null why)
        (setq xl-jit-auto--timer
              (run-with-idle-timer xl-jit-auto-idle nil #'xl-jit-auto--start))
      (let ((delay (xl-jit-auto--delay why)))
        (when delay (xl-jit-auto--schedule delay))))))

(defun xl-jit-auto-arm ()
  "Arm the first check.  Does nothing in batch, so setup, tests, and the
background run itself (which loads this init too) never start another."
  (unless noninteractive
    (xl-jit-auto--schedule (max 1 (- xl-jit-auto-uptime (xl-jit-auto--uptime))))))

;;; The background run

(defun xl-jit-auto--command ()
  "Command line of the background run: the same binary and profile, at
background priority (macOS `taskpolicy -b' puts it on the efficiency cores)."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory))
         (emacs (list exe "--batch" "--init-directory" profile
                      "-l" (expand-file-name "early-init.el" profile)
                      "--eval" "(run-hooks (quote before-init-hook))"
                      "-l" (expand-file-name "init.el" profile)
                      "--eval" "(progn (require 'xl-jit) (prin1 (xl-jit-run)) (terpri))")))
    (cond ((executable-find "taskpolicy") (append '("taskpolicy" "-b") emacs))
          ((executable-find "nice") (append '("nice" "-n" "19") emacs))
          (t emacs))))

(defun xl-jit-auto--sentinel (proc _event)
  (unless (process-live-p proc)
    (setq xl-jit-auto--process nil
          xl-jit-auto-last-result
          (with-current-buffer (process-buffer proc)
            (save-excursion
              (goto-char (point-max))
              ;; The result is the last line that reads as a form.
              (when (re-search-backward "^\\((ran .*)\\|unchanged\\|busy\\|unavailable\\)$" nil t)
                (ignore-errors (let ((read-eval nil)) (read (match-string 0))))))))
    (xl-jit-auto--schedule xl-jit-auto-interval)))

(defun xl-jit-auto--start ()
  "Start the background run, unless the conditions changed while idle."
  (xl-jit-auto--cancel)
  (let ((why (xl-jit-auto--decision)))
    (if why
        (let ((delay (xl-jit-auto--delay why)))
          (when delay (xl-jit-auto--schedule delay)))
      (let ((buf (get-buffer-create " *xl-jit*")))
        (with-current-buffer buf (erase-buffer))
        (setq xl-jit-auto--process
              (make-process :name "xl-jit" :buffer buf :noquery t
                            :connection-type 'pipe
                            :command (xl-jit-auto--command)
                            :sentinel #'xl-jit-auto--sentinel))))))

(provide 'xl-jit)
(provide 'xl-jit-rank)
(provide 'xl-jit-auto)

;;; xl-jit.el ends here
