;;; eln-opt.el --- tiered native compilation for Emacs, in the background -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Xueliang Zhong

;; Author: Xueliang Zhong <xueliang.zhong@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, compile, performance

;;; Commentary:

;; eln-opt makes the code you use often native-compiled, in a background Emacs,
;; without turning on Emacs's global JIT.  Nothing in the running session is
;; profiled or hooked except one timer (`eln-opt-auto-arm'):
;;
;; - `eln-opt-auto-arm' starts a background pass once Emacs has been open a
;;   while and the user is idle, at most once an hour.
;; - A pass (`eln-opt-run') ranks the hot files from byte code and the usage
;;   records (savehist, recentf), compiles the best few in child Emacs
;;   processes, verifies each eln, and records what it did in a ledger.
;; - New elns take effect on the next start.
;; - `eln-opt-report' shows the ranking, what was built,
;;   and why each file was or was not compiled.
;;
;; Without native compilation (no libgccjit), nothing is ever scheduled.
;; The design notes are in emacs/ideas/eln-opt.md of the dotfiles repository.
;;
;; Requires xl-async (which holds the loader too) for the background views.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'comp nil t)
(require 'xl-async)

;; Design: emacs/ideas/eln-opt.md.  This file is loaded only by a background
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
      (princ (format "eln-opt: native compilation not available, skipped (%s)\n"
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
       (not (memq s native-comp-never-optimize-functions))))

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
                            (ignore-errors (byte-decompile-bytecode code consts))))
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
  "Non-nil if a trampoline for S is already in one of the eln-cache folders."
  (let ((name (comp-trampoline-filename s)))
    (cl-some (lambda (dir)
               (file-exists-p
                (expand-file-name name (expand-file-name comp-native-version-dir dir))))
             native-comp-eln-load-path)))

(defun eln-opt-trampolines-missing ()
  "Predicted primitives whose trampoline is not cached yet."
  (cl-remove-if #'eln-opt--trampoline-cached-p (eln-opt-trampolines-predict)))

(defun eln-opt-trampolines-build ()
  "Build the missing trampolines, at most `eln-opt-trampoline-limit' of them.
Returns `unavailable' on a platform without native compilation, otherwise a
plist (:built N :failed N :left N).  A failure is counted and skipped; it is
not retried within the same run."
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
;;; Step 4: ledger, lock, fingerprint, verification, audit
;;; (see eln-opt.md, "不会死循环的规则" and "安全")
;;; ------------------------------------------------------------

(defconst eln-opt-max-fails 2
  "Failures after which a source is left alone until its key changes.")

(defconst eln-opt-retry-interval (* 24 3600)
  "Seconds before a source that failed once may be tried again.")

(defconst eln-opt-lock-max-age 3600
  "Seconds after which a lock counts as stale even if its owner still exists.")

(defconst eln-opt--ledger-max-bytes (* 1024 1024)
  "Largest ledger eln-opt will read.")

(defun eln-opt--now () (float-time))

(defun eln-opt--version-dir ()
  "State folder for this Emacs build.  Ledgers never mix builds."
  (expand-file-name comp-native-version-dir eln-opt-state-dir))

(defun eln-opt--ledger-file () (expand-file-name "ledger.eld" (eln-opt--version-dir)))
(defun eln-opt--last-run-file () (expand-file-name "last-run.eld" (eln-opt--version-dir)))
(defun eln-opt--lock-dir () (expand-file-name "lock" (eln-opt--version-dir)))

(defun eln-opt--flags ()
  "Compile settings that change the output but not the eln file name."
  (list native-comp-speed native-comp-driver-options
        native-comp-compiler-options native-comp-debug))

(defun eln-opt--file-sha (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun eln-opt--file-size (file)
  (let ((attrs (file-attributes file)))
    (and attrs (file-attribute-size attrs))))

(defun eln-opt-source-key (src)
  "Ledger key for SRC: its truename, content hash and compile settings."
  (list (file-truename src) (eln-opt--file-sha (file-truename src)) (eln-opt--flags)))

;;; Ledger

(defun eln-opt--ledger-load ()
  "Entries of this build's ledger, as an alist (KEY . PLIST)."
  (let ((form (eln-opt--read-eld (eln-opt--ledger-file) eln-opt--ledger-max-bytes)))
    (and (equal (plist-get form :version) comp-native-version-dir)
         (seq-filter (lambda (e) (and (consp e) (consp (car e)) (listp (cdr e))))
                     (plist-get form :entries)))))

(defun eln-opt--ledger-save (entries)
  (eln-opt--write-eld (eln-opt--ledger-file)
                     (list :version comp-native-version-dir :entries entries)))

(defun eln-opt--ledger-get (key)
  (cdr (assoc key (eln-opt--ledger-load))))

(defun eln-opt--ledger-set (key plist)
  (eln-opt--ledger-save
   (cons (cons key plist) (assoc-delete-all key (eln-opt--ledger-load)))))

(defun eln-opt--ledger-remove (key)
  (eln-opt--ledger-save (assoc-delete-all key (eln-opt--ledger-load))))

(defun eln-opt--record-failure (key reason)
  "Count one more failure for KEY.  After `eln-opt-max-fails', it waits for a new key."
  (let ((fails (1+ (or (plist-get (eln-opt--ledger-get key) :fails) 0))))
    (eln-opt--ledger-set key (list :status 'failed :fails fails
                                  :reason reason :time (eln-opt--now)))))

(defun eln-opt--may-attempt-p (key now)
  "Non-nil if KEY should be compiled now.
Done entries are skipped while their eln exists.  A failed entry waits
`eln-opt-retry-interval' and is tried at most `eln-opt-max-fails' times.
Anything else (quarantined, unknown) is left alone."
  (let* ((e (eln-opt--ledger-get key))
         (status (plist-get e :status))
         (eln (plist-get e :eln)))
    (cond ((null e) t)
          ((eq status 'missing) t)
          ((eq status 'done) (not (and eln (file-exists-p eln))))
          ((eq status 'failed)
           (and (< (or (plist-get e :fails) 0) eln-opt-max-fails)
                (>= (- now (or (plist-get e :time) 0)) eln-opt-retry-interval)))
          (t nil))))

;;; Verification and audit

(defun eln-opt--short (text)
  "The part of TEXT that says what went wrong, at most 300 characters."
  (let* ((start (or (string-match "Error:" text) 0))
         (piece (string-trim (substring text start))))
    (if (> (length piece) 300) (substring piece 0 300) piece)))

(defun eln-opt--child (form &optional profile)
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

(defun eln-opt--child-in-profile (form)
  "`eln-opt--child' with this run's profile loaded first."
  (eln-opt--child form (eln-opt--profile-dir)))

(defun eln-opt--first-defun (src)
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

(defun eln-opt-verify (src eln &optional names)
  "Load ELN in a fresh process and check that one of NAMES is native.
NAMES are the functions the ranking found in SRC.  Without them, the first
top-level defun of SRC is checked.  Return nil when it is, otherwise a string
saying why not.  This is the only test of \"done\": the file existing is not
enough (see eln-opt.md).  A file can define its functions through macros, so
its defuns need not be visible when read."
  (let ((names (or (seq-filter #'symbolp names)
                   (let ((first (eln-opt--first-defun src))) (and first (list first))))))
    (if (null names)
        "no function to check"
      (let* ((r (eln-opt--child-in-profile
                 `(progn (load ,eln nil t)
                         (princ (if (seq-some (lambda (s)
                                                (and (fboundp s)
                                                     (native-comp-function-p (symbol-function s))))
                                              ',names)
                                    "XL-JIT-OK" "XL-JIT-NOT-NATIVE")))))
             (out (string-trim (cdr r))))
        (cond ((/= (car r) 0) (format "child exited %s: %s" (car r) (eln-opt--short out)))
              ((string-match-p "XL-JIT-OK\\'" out) nil)
              (t "loaded but not native"))))))

(defun eln-opt--discard-eln (eln)
  "Delete ELN with comp's own helper, ignoring errors."
  (ignore-errors (comp-delete-or-replace-file eln)))

(defun eln-opt--audit ()
  "Check every done entry before doing new work.
A missing eln drops its entry, so it can be built again.  An eln whose sha256
or size differs from the ledger, or that fails to load, is deleted and
counted as a failure."
  (dolist (entry (eln-opt--ledger-load))
    (let* ((key (car entry)) (p (cdr entry)) (eln (plist-get p :eln)))
      (when (eq (plist-get p :status) 'done)
        (cond
         ((not (and eln (file-exists-p eln)))
          ;; Recorded, not silently dropped: something outside eln-opt removed
          ;; the eln.  The next pass may build it again.
          (eln-opt--ledger-set key (list :status 'missing :time (eln-opt--now)
                                        :reason "eln file gone; removed outside eln-opt?")))
         ((not (and (equal (eln-opt--file-sha eln) (plist-get p :sha))
                    (equal (eln-opt--file-size eln) (plist-get p :size))))
          (eln-opt--discard-eln eln)
          (eln-opt--record-failure key "sha256 or size differs from the ledger"))
         (t
          (let* ((r (eln-opt--child-in-profile `(load ,eln nil t)))
                 (why (and (/= (car r) 0)
                           (format "load failed in a fresh process: %s"
                                   (eln-opt--short (cdr r))))))
            (when why
              (eln-opt--discard-eln eln)
              (eln-opt--record-failure key why)))))))))

;;; Lock: taken by making a directory, which is atomic.  Never waits.

(defun eln-opt--owner-file (dir) (expand-file-name "owner.eld" dir))

(defun eln-opt--lock-stale-p (dir)
  (let* ((owner (eln-opt--read-eld (eln-opt--owner-file dir)))
         (pid (plist-get owner :pid))
         (time (plist-get owner :time)))
    (or (null owner)
        (not (and (integerp pid) (process-attributes pid)))
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

(defun eln-opt--lock-acquire ()
  "Return non-nil if this process now holds the lock.  A stale lock is
renamed away first, so two runners cannot both take over the same one."
  (let ((dir (eln-opt--lock-dir)))
    (make-directory (eln-opt--version-dir) t)
    (or (eln-opt--lock-take dir)
        (and (eln-opt--lock-stale-p dir)
             (condition-case nil
                 (progn (rename-file dir (make-temp-name (concat dir ".stale-")))
                        (eln-opt--lock-take dir))
               (file-error nil))))))

(defun eln-opt--lock-release ()
  "Remove the lock, but only if this process owns it."
  (let ((dir (eln-opt--lock-dir)))
    (when (equal (plist-get (eln-opt--read-eld (eln-opt--owner-file dir)) :pid)
                 (emacs-pid))
      (ignore-errors (delete-directory dir t)))))

;;; Fingerprint and the run itself

(defun eln-opt--mtime (file)
  (let ((attrs (file-attributes file)))
    (and attrs (file-attribute-modification-time attrs))))

(defun eln-opt--watch-files ()
  "Our own lisp sources, whose edits should trigger a new check."
  (when (bound-and-true-p xl-dotfiles)
    (let ((root (expand-file-name "emacs/" xl-dotfiles)))
      (cons (expand-file-name "xueliang.el" root)
            (directory-files (expand-file-name "lisp" root) t "\\.el\\'")))))

(defun eln-opt--fingerprint ()
  "Hash of the folder mtimes in `load-path' and of our own lisp sources.
The state folder is left out: the run itself changes it."
  (let* ((dirs (seq-filter (lambda (d) (and (stringp d) (file-directory-p d)))
                           load-path))
         (items (append
                 (mapcar (lambda (d) (list d (eln-opt--mtime d))) dirs)
                 (mapcar (lambda (f) (list f (eln-opt--file-size f) (eln-opt--mtime f)))
                         (eln-opt--watch-files)))))
    (secure-hash 'sha256 (prin1-to-string items))))

(defun eln-opt--next-eligible ()
  "Earliest time a failed entry may be tried again, or nil."
  (let (best)
    (dolist (e (eln-opt--ledger-load))
      (let ((p (cdr e)))
        (when (and (eq (plist-get p :status) 'failed)
                   (< (or (plist-get p :fails) 0) eln-opt-max-fails))
          (let ((at (+ (or (plist-get p :time) 0) eln-opt-retry-interval)))
            (setq best (if best (min best at) at))))))
    best))

;;; ------------------------------------------------------------
;;; Step 6: Tier 2, background native compilation
;;; (eln-opt.md, "流水线" steps 5-6, "安全", "DoS")
;;; ------------------------------------------------------------

(defconst eln-opt-compile-budget 5
  "Most files one run compiles.  Each costs a child Emacs with the profile.")

(defconst eln-opt-min-ratio 3.0
  "Lowest score/KB a file needs to be a candidate.  Filters only; the order of
the candidates is by score.  Uncalibrated: set it from a profiler run.")

(defconst eln-opt-max-source-bytes (* 500 1024)
  "Largest source file eln-opt will compile.")

(defconst eln-opt-max-total-bytes (* 200 1024 1024)
  "Most eln bytes eln-opt keeps in total.  Past it, runs compile nothing.")

(defconst eln-opt-compile-timeout 300
  "Seconds one child compile may take before it is killed and counted failed.")

(defvar eln-opt-crash-dir (expand-file-name "~/Library/Logs/DiagnosticReports/")
  "Where macOS writes crash reports.  Tests rebind it.")

(defconst eln-opt--crash-read-bytes (* 2 1024 1024)
  "Most of each crash report that is read.")

(defun eln-opt--profile-dir ()
  "The profile this run belongs to: the batch's user-emacs-directory."
  (file-name-as-directory user-emacs-directory))

(defun eln-opt--run-with-timeout (args seconds)
  "Run the Emacs binary with ARGS for at most SECONDS.
Return `ok' on exit 0, otherwise a string saying why not.  A child that runs
too long is killed, so a hung compile cannot hold the lock forever."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (buf (generate-new-buffer " *eln-opt-child*"))
         (proc (make-process :name "eln-opt-child" :buffer buf :noquery t
                             :command (cons exe args)))
         (deadline (+ (eln-opt--now) seconds)))
    (unwind-protect
        (progn
          (while (and (process-live-p proc) (< (eln-opt--now) deadline))
            (accept-process-output proc 0.5))
          (cond
           ((process-live-p proc)
            (signal-process proc 'KILL)
            (format "timed out after %d s" seconds))
           ((zerop (process-exit-status proc)) 'ok)
           (t (format "exit %d: %s" (process-exit-status proc)
                      (eln-opt--short
                       (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max))))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun eln-opt--child-compile (src eln)
  "Compile SRC to ELN in a child Emacs that loads the profile first.
The profile is needed: without its packages, macros such as evil's would be
compiled as calls.  `native-comp-speed' is capped at 2 (eln-opt.md, \"实测\")."
  (let* ((profile (eln-opt--profile-dir))
         (form `(progn (setq native-comp-speed (min 2 native-comp-speed))
                       (native-compile ,src ,eln))))
    (eln-opt--run-with-timeout
     (list "--batch" "--init-directory" profile
           "-l" (expand-file-name "early-init.el" profile)
           "--eval" "(run-hooks (quote before-init-hook))"
           "-l" (expand-file-name "init.el" profile)
           "--eval" (format "%S" form))
     eln-opt-compile-timeout)))

(defun eln-opt--read-text (file max-bytes)
  (when (file-readable-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 max-bytes)
      (buffer-string))))

(defun eln-opt--eln-bytes ()
  "Total size of the elns the ledger records as done."
  (let ((sum 0))
    (dolist (e (eln-opt--ledger-load))
      (when (eq (plist-get (cdr e) :status) 'done)
        (cl-incf sum (or (plist-get (cdr e) :size) 0))))
    sum))

(defun eln-opt--crash-scan (since)
  "Quarantine done elns that a crash report newer than SINCE names.
An eln named in a crash is deleted and its key quarantined for good: a
crash is not retried.  Returns the number quarantined."
  (let ((n 0))
    (when (file-directory-p eln-opt-crash-dir)
      (dolist (f (directory-files eln-opt-crash-dir t "\\`Emacs.*\\.ips\\'"))
        (let ((mtime (eln-opt--mtime f)))
          (when (and mtime (> (float-time mtime) since))
            (let ((text (or (eln-opt--read-text f eln-opt--crash-read-bytes) "")))
              (dolist (e (eln-opt--ledger-load))
                (let ((eln (plist-get (cdr e) :eln)))
                  (when (and eln (eq (plist-get (cdr e) :status) 'done)
                             (string-match-p (regexp-quote (file-name-nondirectory eln))
                                             text))
                    (eln-opt--discard-eln eln)
                    (eln-opt--ledger-set (car e)
                                        (list :status 'quarantined
                                              :reason "named in a crash report"
                                              :time (eln-opt--now)))
                    (cl-incf n)))))))))
    n))

(defun eln-opt--candidate-reason (row)
  "Why ranking ROW is not a candidate, as a string; nil if it is one.
Checked again here, not trusted from the ranking: the source is re-tested
against the trusted roots and the opt-out cookie, and must be a reasonable size."
  (let* ((path (plist-get row :path))
         (src (and path (replace-regexp-in-string "\\.elc\\'" ".el" path))))
    (cond
     ((null path) "no source path recorded")
     ((null (plist-get row :ratio)) "no ratio")
     ((< (plist-get row :ratio) eln-opt-min-ratio)
      (format "ratio %.1f is below %.1f" (plist-get row :ratio) eln-opt-min-ratio))
     ((not (file-readable-p src))
      (cond ((file-exists-p (concat src ".gz"))
             "Emacs ships only .el.gz for this library: not supported yet")
            ((string-suffix-p ".elc" path) "no .el source next to the .elc")
            (t "source not readable")))
     ((not (eln-opt-trusted-file-p src)) "outside the trusted roots")
     ((eln-opt-rank--no-native-p src) "opts out with no-native-compile")
     ((> (or (eln-opt--file-size src) 0) eln-opt-max-source-bytes)
      (format "source is %d KB, over the %d KB limit"
              (/ (eln-opt--file-size src) 1024) (/ eln-opt-max-source-bytes 1024)))
     (t nil))))

(defun eln-opt--candidate-p (row)
  "Non-nil if ranking ROW may be compiled."
  (null (eln-opt--candidate-reason row)))

(defun eln-opt--compile-one (src &optional names)
  "Compile and verify SRC; NAMES are its functions from the ranking.
Return `done', `skipped', or `failed'."
  (let* ((true (file-truename src))
         (key (eln-opt-source-key true))
         (eln (comp-el-to-eln-filename true))
         (now (eln-opt--now)))
    (if (not (eln-opt--may-attempt-p key now))
        'skipped
      (let* ((built (eln-opt--child-compile true eln))
             (reason (cond ((not (eq built 'ok)) built)
                           ((not (file-exists-p eln)) "no eln written")
                           (t (eln-opt-verify true eln names)))))
        (if reason
            (progn
              (eln-opt--discard-eln eln)
              (eln-opt--record-failure key reason)
              'failed)
          (eln-opt--ledger-set key (list :status 'done :eln eln
                                        :sha (eln-opt--file-sha eln)
                                        :size (eln-opt--file-size eln)
                                        :time now))
          'done)))))

(defun eln-opt--work ()
  "One compile round: crash scan, then up to `eln-opt-compile-budget' files
from the ranking, if the total eln size allows it."
  (let* ((last (eln-opt--read-eld (eln-opt--last-run-file)))
         (since (or (plist-get last :time) 0))
         (quarantined (eln-opt--crash-scan since))
         (capped (>= (eln-opt--eln-bytes) eln-opt-max-total-bytes))
         (compiled 0) (failed 0) (skipped 0) (built nil))
    (unless capped
      (let* ((rows (plist-get (eln-opt-rank--compute (eln-opt--profile-dir)) :rows))
             (cands (seq-take (seq-filter #'eln-opt--candidate-p rows)
                              eln-opt-compile-budget)))
        (dolist (row cands)
          (let ((src (replace-regexp-in-string "\\.elc\\'" ".el" (plist-get row :path))))
            (pcase (eln-opt--compile-one src (plist-get row :defuns))
              ('done (cl-incf compiled)
                     (push (file-name-nondirectory src) built))
              ('failed (cl-incf failed))
              (_ (cl-incf skipped)))))))
    (list :compiled compiled :failed failed :skipped skipped
          :quarantined quarantined :capped capped :built (reverse built))))

(defun eln-opt-status ()
  "Counts by ledger status, eln bytes, and the last run's result."
  (let ((counts nil))
    (dolist (e (eln-opt--ledger-load))
      (let ((st (plist-get (cdr e) :status)))
        (setf (alist-get st counts 0) (1+ (alist-get st counts 0)))))
    (list :ledger counts
          :eln-bytes (eln-opt--eln-bytes)
          :eln-cap eln-opt-max-total-bytes
          :last-run (eln-opt--read-eld (eln-opt--last-run-file)))))

(defun eln-opt--run-locked ()
  (let* ((now (eln-opt--now))
         (fp (eln-opt--fingerprint))
         (last (eln-opt--read-eld (eln-opt--last-run-file)))
         (next (plist-get last :next)))
    (if (and (equal fp (plist-get last :fingerprint))
             (or (null next) (< now next)))
        'unchanged
      (progn
        (eln-opt--audit)
        (let ((result (eln-opt--work)))
          (eln-opt--write-eld (eln-opt--last-run-file)
                             (list :fingerprint fp
                                   :next (eln-opt--next-eligible)
                                   :time (eln-opt--now)
                                   :result result))
          (cons 'ran result))))))

(defun eln-opt-run-summary (result)
  "One readable line saying what RESULT of `eln-opt-run' did."
  (pcase result
    ('unavailable "eln-opt: native compilation is not available here; nothing done.")
    ('busy "eln-opt: another run holds the lock; nothing done.")
    ('unchanged "eln-opt: nothing changed since the last run; nothing done.")
    (`(ran . ,r)
     (let ((compiled (plist-get r :compiled))
           (failed (plist-get r :failed))
           (skipped (plist-get r :skipped))
           (quarantined (plist-get r :quarantined)))
       (format (concat "eln-opt: compiled %d file(s), failed %d, quarantined %d.\n"
                       "      %d candidate(s) were already built or waiting to retry.%s")
               compiled failed quarantined skipped
               (concat
                (if (plist-get r :capped)
                    "\n      Stopped early: the total eln size is at its cap."
                  "")
                (if (plist-get r :built)
                    (format "\n      Built now: %s."
                            (mapconcat #'identity (plist-get r :built) ", "))
                  "")))))
    (_ (format "eln-opt: %S" result))))

(defun eln-opt-run ()
  "One background run.  Return `unavailable', `busy', `unchanged', or (ran . RESULT).
Does nothing at all on a platform without native compilation."
  (cond
   ((not (eln-opt-platform-available-p)) 'unavailable)
   ((not (eln-opt--lock-acquire)) 'busy)
   (t (unwind-protect (eln-opt--run-locked)
        (eln-opt--lock-release)))))

;;; Step 4: reset (the kill switch's companion)

(defun eln-opt-reset ()
  "Delete only the eln files this build's ledger recorded, then the ledger.
Files eln-opt did not write are never touched.  Returns the number deleted."
  (let ((n 0))
    (dolist (e (eln-opt--ledger-load))
      (let ((eln (plist-get (cdr e) :eln)))
        (when (and eln (file-exists-p eln))
          (ignore-errors (delete-file eln))
          (cl-incf n))))
    (ignore-errors (delete-file (eln-opt--ledger-file)))
    (ignore-errors (delete-file (eln-opt--last-run-file)))
    n))

;;; Trusted roots: a file is compiled (or ranked for compiling) only if its
;;; truename lies under one of these.  Used by the report and by step 6.

(defun eln-opt-trusted-roots ()
  "Folders and files whose lisp may be compiled: elpa, Emacs's own lisp,
our lisp/ folder, and our xueliang.el.  A root ending in a slash is a folder."
  (delq nil
        (list (and (boundp 'package-user-dir) (file-name-as-directory package-user-dir))
              (and (boundp 'lisp-directory) (file-name-as-directory lisp-directory))
              (and (bound-and-true-p xl-dotfiles)
                   (file-name-as-directory (expand-file-name "emacs/lisp" xl-dotfiles)))
              (and (bound-and-true-p xl-dotfiles)
                   (expand-file-name "emacs/xueliang.el" xl-dotfiles)))))

(defun eln-opt-trusted-file-p (file)
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
                  (eln-opt-trusted-roots)))))

;;; Query: which functions are native-compiled in this Emacs

(defun eln-opt--source-stem (file)
  "FILE's truename without .elc or .el, for matching a loaded file to a source."
  (file-name-sans-extension
   (or (ignore-errors (file-truename file)) file)))

(defun eln-opt-native-list ()
  "Natively compiled functions in this Emacs, grouped by source file.
Returns a plist:
  :built    the files eln-opt built, from the ledger, as (SOURCE . ELN)
  :files    alist (FILE . SYMBOLS) for every file that has a native function,
            FILE being what `symbol-file' reports; sorted by file name
  :total    how many functions are native in all
Emacs's own preloaded native code is in :files too, so `:total' is large."
  (let ((built (let (out)
                 (dolist (e (eln-opt--ledger-load))
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

;;; ---- Report: the ranking, the built files, the skips ----

;; Step 5 of emacs/ideas/eln-opt.md.  Nothing here compiles anything.  A child
;; Emacs with the profile loaded reads the byte code, the call graph, and the
;; usage records (savehist, recentf) as data, and prints a ranking.  Entry
;; points: `eln-opt-report' (M-x), and `emacs-setup.sh eln-opt-report'.

;;; ------------------------------------------------------------
;;; Settings.  The weights are starting values; calibrate them against a
;;; profiler run before trusting the order (eln-opt.md, "校准").
;;; ------------------------------------------------------------

(defconst eln-opt-rank-heat-levels 4
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

(defun eln-opt-rank--lap (fn)
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
             (w (expt eln-opt-rank-loop-factor
                      (+ (aref depth i) (if recursive 1 0)))))
        (cond
         ((memq head eln-opt-rank--inline-ops) (cl-incf benefit w))
         ((memq head eln-opt-rank--buffer-ops) (cl-decf benefit (* 2 w)))
         ((eq head 'byte-varref)
          (let ((v (car-safe (cdr op)))) (when (symbolp v) (push v varrefs))))
         ((eq head 'byte-constant)
          (let ((c (car-safe (cdr op))))
            (when (and (symbolp c) c (fboundp c))
              (push c calls)
              (cond ((memq c eln-opt-rank--c-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c eln-opt-rank--alloc-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c eln-opt-rank--dynamic-callers)
                     (setq dynamic t) (cl-incf benefit (* 0.5 w)))
                    ((subrp (indirect-function c)) nil)
                    (t (cl-incf benefit (* 0.5 w))))))))))
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
Returns nil for autoload files, which only hold stubs."
  (let ((base (file-name-nondirectory file)))
    (unless (string-match-p "-autoloads\\.elc?\\'" base)
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
    (dolist (entry load-history)
      (let* ((file (car entry))
             (key (and (stringp file) (eln-opt-rank--file-key file))))
        (when key
          (dolist (d (cdr entry))
            (when (and (consp d) (eq (car d) 'defun) (symbolp (cdr d))
                       (fboundp (cdr d)))
              (let ((sym (cdr d)))
                (cond
                 ((native-comp-function-p (indirect-function sym))
                  (cl-incf native-skipped))
                 ((eln-opt-rank--no-native-p file)
                  (cl-pushnew (cons key "no-native-compile or no-byte-compile") excluded
                              :test #'equal))
                 ;; Interpreted functions are skipped, not compiled here: the
                 ;; report must have no side effects.  Our own lisp is byte
                 ;; code once compile-lisp has run.
                 ((byte-code-function-p (indirect-function sym))
                  (puthash sym key file-of)
                  (puthash sym file path-of)
                  (puthash sym (eln-opt-rank--features sym) features))
                 (t (cl-incf interpreted-skipped)))))))))
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
                        (expand-file-name "emacs/eln-opt-baseline.eld" xl-dotfiles))
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
  (let* ((roots-info (eln-opt-rank--roots profile-dir))
         (user-roots (plist-get roots-info :roots))
         (lazy-loaded (eln-opt-rank--load-lazy-roots user-roots))
         (graph (eln-opt-rank--graph profile-dir))
         (edges (plist-get graph :edges))
         (user-count (plist-get roots-info :user-count))
         (baseline (car (eln-opt-rank--baseline profile-dir)))
         (base-factor (max 0.0 (- 1.0 (/ (float user-count) eln-opt-rank-baseline-users))))
         (base-roots (mapcar (lambda (s) (cons s (* base-factor 1.0))) baseline))
         (seeds (append user-roots base-roots))
         (heat (eln-opt-rank--propagate seeds edges))
         (scores (eln-opt-rank--file-scores heat graph))
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
            :built-rows (eln-opt-rank--built-rows)
            :next (seq-take (seq-filter #'eln-opt--candidate-p
                                        (sort (copy-sequence rows)
                                              (lambda (a b) (> (plist-get a :score) (plist-get b :score)))))
                            eln-opt-compile-budget)
            :ledger (eln-opt-rank--ledger-summary)
            :savehist-share max-share
            :functions (hash-table-count (plist-get graph :features))
            :roots (length user-roots)
            :user-count user-count
            :baseline-roots (length baseline)
            :baseline-factor base-factor
            :excluded (plist-get graph :excluded)
            :native-skipped (plist-get graph :native-skipped)
            :interpreted-skipped (plist-get graph :interpreted-skipped)))))

(defun eln-opt-rank--built-rows ()
  "One row per file eln-opt built and that is native now.
These files drop out of the ranking, because their functions are native, so
they are listed here with native \"eln-opt\".  Their KB is the eln size."
  (let* ((info (eln-opt-native-list))
         (natives (plist-get info :files))
         rows)
    (dolist (b (plist-get info :built))
      (let* ((src (car b))
             (eln (cdr b))
             (stem (eln-opt--source-stem src))
             (fns (cl-loop for (file . syms) in natives
                           when (equal (eln-opt--source-stem file) stem)
                           append syms)))
        (push (list :file (eln-opt-rank--file-key src) :native "eln-opt"
                    :path src :trusted t :ratio nil :score nil
                    :size (or (eln-opt--file-size eln) 0)
                    :count (length fns)
                    :top (seq-take fns 3))
              rows)))
    (sort rows (lambda (a b) (string< (plist-get a :file) (plist-get b :file))))))

(defun eln-opt-rank--line (rank score kb trusted native file top)
  "One report row.  Every row, ranked or built, uses this one layout."
  (format "%4s  %6s  %6s  %-7s  %-6s  %-28s  %s\n"
          rank score kb trusted native file top))

(defun eln-opt-rank--ledger-summary ()
  "Every ledger entry as a plist, newest first, with the source name."
  (let (out)
    (dolist (e (eln-opt--ledger-load))
      (let ((p (cdr e)))
        (push (list :file (file-name-nondirectory (car (car e)))
                    :status (plist-get p :status)
                    :time (plist-get p :time)
                    :fails (or (plist-get p :fails) 0)
                    :reason (plist-get p :reason)
                    :kb (and (plist-get p :size) (/ (plist-get p :size) 1024.0)))
              out)))
    (sort out (lambda (a b) (> (or (plist-get a :time) 0) (or (plist-get b :time) 0))))))

(defun eln-opt-rank--when (time)
  (if time (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time time)) "-"))

(defun eln-opt-rank--format (result)
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
      (princ "eln-opt report (report only; nothing is compiled here)\n\n")
      (princ "Next background pass compiles: ")
      (if (null next)
          (princ "nothing.\n")
        (princ (format "%d file(s), in this order: %s\n" (length next)
                       (mapconcat (lambda (r) (plist-get r :file)) next ", "))))
      ;; 2. What was compiled
      (princ (format "\nCompiled by eln-opt and native now: %d file(s), %d function(s)\n"
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
                         (eln-opt-rank--when (plist-get e :time))
                         (or (plist-get e :kb) 0)))))
      ;; 3. What failed or was held back
      (when bad
        (princ "\nFailed or quarantined (not retried until the key changes, or for a day):\n")
        (dolist (e bad)
          (princ (format "  %-30s %-11s %s  fails %d\n    %s\n"
                         (plist-get e :file) (plist-get e :status)
                         (eln-opt-rank--when (plist-get e :time))
                         (plist-get e :fails)
                         (or (plist-get e :reason) "")))))
      ;; 4. Why each ranked file is or is not compiled
      (princ "\nRanked files not yet native, and what happens to each:\n")
      (let ((n 0))
        (dolist (row rows)
          (let ((why (if (memq row next)
                         "compile in the next pass"
                       (or (eln-opt--candidate-reason row)
                           (format "candidate, but past the budget of %d per pass" eln-opt-compile-budget)))))
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
      (princ (eln-opt-rank--line "rank" "score" "KB" "trusted" "native" "file" "[top functions]"))
      (cl-loop for row in (seq-take rows 25) for i from 1
               do (princ (eln-opt-rank--line
                          i
                          (format "%.1f" (plist-get row :score))
                          (format "%.1f" (/ (plist-get row :size) 1024.0))
                          (if (plist-get row :trusted) "yes" "NO")
                          "no" (plist-get row :file)
                          (format "%S" (plist-get row :top))))))))

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
      (xl-async-run
       :name label
       :owner buf
       :slot 'eln-opt-rank
       :command (eln-opt-rank--child-command form)
       :on-done
       (lambda (output code)
         (xl-spinner-mode-line-stop)
         (eln-opt-rank--draw-center nil)
         (let ((report (eln-opt-rank--report-from-output output))
               (inhibit-read-only t))
           (erase-buffer)
           (if (and (eql code 0) report (not (string-empty-p (string-trim report))))
               (insert report)
             (insert "Failed.\n\n" output))
           (goto-char (point-min))))))
    (display-buffer buf)
    buf))

(defconst eln-opt-rank--report-form
  "(progn (require (quote eln-opt)) (setq eln-opt-rank--with-marker t) (eln-opt-report))"
  "The child form that prints the ranking report.")

(defun eln-opt-run-now ()
  "Run one eln-opt pass now, in the background.  Interactive only.
Compiles up to `eln-opt-compile-budget' files, as the daily pass does, but
without waiting for the daily timer.  A buffer shows the summary when done,
and a loader turns while it works.  Batch: runs it here and prints the summary."
  (interactive)
  (if noninteractive
      (princ (concat (eln-opt-run-summary (eln-opt-run)) "\n"))
    (eln-opt-rank--show-async
     "*eln-opt run*" "eln-opt run"
     (concat "(progn (require (quote eln-opt)) "
             "(princ (format \"\\n%s\\n\" " (prin1-to-string eln-opt-rank--marker) ")) "
             "(princ (concat (eln-opt-run-summary (eln-opt-run)) \"\\n\\n\")) "
             "(eln-opt-report))")
     "running an eln-opt pass")))

(defun eln-opt-report ()
  "Show the ranking.  Interactive: a buffer, filled in the background.
Batch: printed to stdout, computed here."
  (interactive)
  (if noninteractive
      (progn
        (when eln-opt-rank--with-marker
          (princ (format "\n%s\n" eln-opt-rank--marker)))
        (princ (eln-opt-rank--format (eln-opt-rank--compute (eln-opt--profile-dir)))))
    (eln-opt-rank--show-async "*eln-opt report*" "eln-opt report" eln-opt-rank--report-form)))

;;; ---- Trigger: when a background pass starts ----

;; The only part of eln-opt that lives in the main Emacs (emacs/ideas/eln-opt.md).
;; It uses timers only, no hooks.  After Emacs has been open for
;; `eln-opt-auto-uptime', and the user has then been idle for `eln-opt-auto-idle',
;; it starts one background batch Emacs that runs `eln-opt-run' (lock,
;; fingerprint, audit, compile) at background priority.  Nothing is loaded into
;; this Emacs: new elns take effect on the next start.  Without native
;; compilation (no libgccjit), no timer is ever armed.

;; M-x entry points defined in eln-opt.el; autoloaded so startup does not load it.

(defgroup eln-opt nil
  "Background native compilation of the hot files."
  :group 'comp)

(defcustom eln-opt-auto-enable t
  "Non-nil means the running Emacs starts eln-opt in the background."
  :type 'boolean)

(defcustom eln-opt-auto-uptime (* 1 60)
  "Seconds Emacs must have been open before a background run is considered.
A short-lived Emacs (a quick edit, `emacsclient -a \"\"') never starts one."
  :type 'integer)

(defcustom eln-opt-auto-idle 60
  "Seconds of idleness after which the background run starts."
  :type 'integer)

(defcustom eln-opt-auto-interval 3600
  "Least time between two background runs, from the last run's record.
A pass with nothing changed returns at once, so this can be short.  It must
stay above zero: a run that ends schedules the next check this far away."
  :type 'integer)

(defcustom eln-opt-auto-retry (* 30 60)
  "When a run is not allowed now (one still running, or too recent), check again
after this many seconds."
  :type 'integer)

(defvar eln-opt-auto--timer nil "The pending timer, idle or not.")
(defvar eln-opt-auto--process nil "The background batch Emacs, while it runs.")
(defvar eln-opt-auto-last-result nil
  "What the last background run printed, for example (ran :compiled 1 ...).")

;;; Conditions

(defun eln-opt-auto--last-run-file ()
  "The record the background run writes.  Its mtime says when it last ran."
  (expand-file-name (concat "eln-opt/" comp-native-version-dir "/last-run.eld")
                    user-emacs-directory))

(defun eln-opt-auto--seconds-since-last-run ()
  "Seconds since the last recorded run, or nil if there is none."
  (let ((attrs (and (boundp 'comp-native-version-dir)
                    (file-attributes (eln-opt-auto--last-run-file)))))
    (and attrs (float-time (time-since (file-attribute-modification-time attrs))))))

(defun eln-opt-auto--uptime ()
  (float-time (time-since before-init-time)))

(defun eln-opt-auto--decision ()
  "Nil when a background run may start now, otherwise a symbol saying why not.
`disabled' and `unavailable' are final for this session; the others are
retried later."
  (let ((since (eln-opt-auto--seconds-since-last-run)))
    (cond ((or (not eln-opt-auto-enable) (equal (getenv "ELN_OPT") "0")) 'disabled)
          ((not (and (fboundp 'native-comp-available-p) (native-comp-available-p)))
           'unavailable)
          ((process-live-p eln-opt-auto--process) 'running)
          ((< (eln-opt-auto--uptime) eln-opt-auto-uptime) 'too-early)
          ((and since (< since eln-opt-auto-interval)) 'recent)
          (t nil))))

(defun eln-opt-auto--delay (why)
  "Seconds to wait before checking again after WHY, or nil to stop."
  (pcase why
    ((or 'disabled 'unavailable) nil)
    ('too-early (max 1 (- eln-opt-auto-uptime (eln-opt-auto--uptime))))
    ('recent (max eln-opt-auto-retry
                  (- eln-opt-auto-interval (or (eln-opt-auto--seconds-since-last-run) 0))))
    (_ eln-opt-auto-retry)))

;;; Timers

(defun eln-opt-auto--cancel ()
  (when (timerp eln-opt-auto--timer) (cancel-timer eln-opt-auto--timer))
  (setq eln-opt-auto--timer nil))

(defun eln-opt-auto--schedule (seconds)
  (eln-opt-auto--cancel)
  (setq eln-opt-auto--timer (run-with-timer seconds nil #'eln-opt-auto--tick)))

(defun eln-opt-auto--tick ()
  "Check the conditions.  If they hold, wait for the user to go idle."
  (eln-opt-auto--cancel)
  (let ((why (eln-opt-auto--decision)))
    (if (null why)
        (setq eln-opt-auto--timer
              (run-with-idle-timer eln-opt-auto-idle nil #'eln-opt-auto--start))
      (let ((delay (eln-opt-auto--delay why)))
        (when delay (eln-opt-auto--schedule delay))))))

(defun eln-opt-auto-arm ()
  "Arm the first check.  Does nothing in batch, so setup, tests, and the
background run itself (which loads this init too) never start another."
  (unless noninteractive
    (eln-opt-auto--schedule (max 1 (- eln-opt-auto-uptime (eln-opt-auto--uptime))))))

;;; The background run

(defun eln-opt-auto--command ()
  "Command line of the background run: the same binary and profile, at
background priority (macOS `taskpolicy -b' puts it on the efficiency cores)."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory))
         (emacs (list exe "--batch" "--init-directory" profile
                      "-l" (expand-file-name "early-init.el" profile)
                      "--eval" "(run-hooks (quote before-init-hook))"
                      "-l" (expand-file-name "init.el" profile)
                      "--eval" "(progn (require 'eln-opt) (prin1 (eln-opt-run)) (terpri))")))
    (cond ((executable-find "taskpolicy") (append '("taskpolicy" "-b") emacs))
          ((executable-find "nice") (append '("nice" "-n" "19") emacs))
          (t emacs))))

(defun eln-opt-auto--sentinel (proc _event)
  (unless (process-live-p proc)
    (setq eln-opt-auto--process nil
          eln-opt-auto-last-result
          (with-current-buffer (process-buffer proc)
            (save-excursion
              (goto-char (point-max))
              ;; The result is the last line that reads as a form.
              (when (re-search-backward "^\\((ran .*)\\|unchanged\\|busy\\|unavailable\\)$" nil t)
                (ignore-errors (let ((read-eval nil)) (read (match-string 0))))))))
    (eln-opt-auto--schedule eln-opt-auto-interval)))

(defun eln-opt-auto--start ()
  "Start the background run, unless the conditions changed while idle."
  (eln-opt-auto--cancel)
  (let ((why (eln-opt-auto--decision)))
    (if why
        (let ((delay (eln-opt-auto--delay why)))
          (when delay (eln-opt-auto--schedule delay)))
      (let ((buf (get-buffer-create " *eln-opt*")))
        (with-current-buffer buf (erase-buffer))
        (setq eln-opt-auto--process
              (make-process :name "eln-opt" :buffer buf :noquery t
                            :connection-type 'pipe
                            :command (eln-opt-auto--command)
                            :sentinel #'eln-opt-auto--sentinel))))))

(provide 'eln-opt)
(provide 'eln-opt-rank)
(provide 'eln-opt-auto)

;;; eln-opt.el ends here
