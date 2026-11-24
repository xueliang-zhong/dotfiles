;;; eln-opt-tests.el --- ERT tests for eln-opt -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L emacs/lisp -l emacs/tests/eln-opt-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Every test works in its own temporary state folder.  Each one checks the
;; final state and that a second run does no compilation at all.

(require 'ert)
(require 'cl-lib)
(require 'eln-opt)
(require 'eln-opt)
(defvar xl-dotfiles nil "Bound by the profile; tests bind it to a temporary folder.")

(defmacro eln-opt-test--with-state (&rest body)
  "Run BODY with `eln-opt-state-dir' in a fresh temporary folder."
  (declare (indent 0))
  `(let ((eln-opt-state-dir (make-temp-file "eln-opt-state-" t)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory eln-opt-state-dir t)))))

(defmacro eln-opt-test--count-canary (counter &rest body)
  "Run BODY with `eln-opt--canary' counting its calls into COUNTER."
  (declare (indent 1))
  `(let ((orig (symbol-function 'eln-opt--canary)))
     (cl-letf (((symbol-function 'eln-opt--canary)
                (lambda () (cl-incf ,counter) (funcall orig))))
       ,@body)))

(ert-deftest eln-opt-unavailable-writes-only-platform-file ()
  "Without native compilation, a run stops at the gate and writes one file."
  (eln-opt-test--with-state
    (let ((canary-calls 0))
      (eln-opt-test--count-canary canary-calls
        (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
          (should (eq (eln-opt-run) 'unavailable))
          (should (eq (eln-opt-run) 'unavailable))))
      (should (= canary-calls 0))
      (should (equal (directory-files eln-opt-state-dir nil "\\`[^.]")
                     '("platform.eld"))))))

(ert-deftest eln-opt-canary-failure-trips-breaker ()
  "A canary that fails is cached as unavailable and not retried."
  (eln-opt-test--with-state
    (let ((compile-calls 0))
      (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
                ((symbol-function 'native-compile)
                 (lambda (&rest _) (cl-incf compile-calls) (error "libgccjit missing"))))
        (should-not (eln-opt-platform-available-p))
        (should (eq (eln-opt-run) 'unavailable))
        (should-not (eln-opt-platform-available-p)))
      (should (= compile-calls 1))
      (let ((cached (eln-opt--read-eld (eln-opt--platform-file))))
        (should-not (plist-get cached :available))
        (should (string-match-p "libgccjit missing" (plist-get cached :reason)))))))

(ert-deftest eln-opt-binary-change-rechecks-once ()
  "A new binary key triggers exactly one new check, then the cache holds."
  (eln-opt-test--with-state
    (let ((canary-calls 0))
      (eln-opt-test--count-canary canary-calls
        (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
                  ((symbol-function 'eln-opt--canary) (lambda () (cl-incf canary-calls) nil)))
          (should (eln-opt-platform-available-p))
          (should (= canary-calls 1))
          (should (eln-opt-platform-available-p))
          (should (= canary-calls 1))
          ;; Pretend the binary was replaced: the stored key no longer matches.
          (eln-opt--write-eld (eln-opt--platform-file)
                             (list :key '("/old/emacs" 1 nil) :available nil :reason "old"))
          (should (eln-opt-platform-available-p))
          (should (= canary-calls 2))
          (should (eln-opt-platform-available-p))
          (should (= canary-calls 2)))))))

(ert-deftest eln-opt-real-canary-when-native-is-built-in ()
  "On a native build the real canary passes and is cached as available."
  (skip-unless (native-comp-available-p))
  (eln-opt-test--with-state
    (let ((canary-calls 0))
      (eln-opt-test--count-canary canary-calls
        (should (eln-opt-platform-available-p))
        (should (eq (plist-get (eln-opt--read-eld (eln-opt--platform-file)) :available) t))
        (should (eln-opt-platform-available-p))
        (should (= canary-calls 1))))
    (should (equal (directory-files eln-opt-state-dir nil "\\`[^.]")
                   '("platform.eld")))))

(defvar eln-opt-test--evaluated nil)

(ert-deftest eln-opt-state-file-is-read-with-read-only ()
  "A state file with #. is read but its form never runs."
  (eln-opt-test--with-state
    (make-directory eln-opt-state-dir t)
    (setq eln-opt-test--evaluated nil)
    (with-temp-file (eln-opt--platform-file)
      (insert "#.(setq eln-opt-test--evaluated t)"))
    (should (null (eln-opt--read-eld (eln-opt--platform-file))))
    (should-not eln-opt-test--evaluated)))

;;; Step 2: trampoline prediction and build

(defvar eln-opt-test--toy-loaded nil
  "Set by the toy .elc's top-level code.  Stays nil when the file is only read.")

(defun eln-opt-test--toy-elc (dir name source)
  "Byte-compile SOURCE into DIR/NAME.el and return the .elc path."
  (let* ((el (expand-file-name (concat name ".el") dir))
         (elc (concat el "c")))
    (with-temp-file el (insert source))
    (let ((byte-compile-warnings nil))
      (byte-compile-file el))
    elc))

(defconst eln-opt-test--toy-source
  ";;; toy.el -*- lexical-binding: t; -*-
(defvar eln-opt-test--toy-loaded nil)
(setq eln-opt-test--toy-loaded t)
(advice-add 'buffer-name :around (lambda (f &rest a) (apply f a)))
(defun eln-opt-toy-cl-letf ()
  (cl-letf (((symbol-function 'message) #'ignore)) 1))
(defun eln-opt-toy-alias () (defalias 'eln-opt-toy-x #'car))
")

(defmacro eln-opt-test--with-toy (elc-var &rest body)
  "Compile the toy into a temporary folder on `load-path', bind ELC-VAR, run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "eln-opt-toy-" t))
          (load-path (cons dir load-path))
          (,elc-var (eln-opt-test--toy-elc dir "toy" eln-opt-test--toy-source)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(ert-deftest eln-opt-predict-finds-advice-and-letf-but-not-aliases ()
  "The toy's advice-add and cl-letf targets are predicted; defalias is not."
  (eln-opt-test--with-toy elc
    (let ((only-toy (eln-opt--scan-elc elc)))
      (should (memq 'buffer-name only-toy))
      (should (memq 'message only-toy))
      (should-not (memq 'car only-toy)))))

(ert-deftest eln-opt-predict-never-loads-the-files ()
  "Scanning reads .elc files as data: the toy's top-level code does not run."
  (setq eln-opt-test--toy-loaded nil)
  (eln-opt-test--with-toy elc
    (eln-opt--scan-elc elc)
    (eln-opt-trampolines-predict)
    (should-not eln-opt-test--toy-loaded)
    (should-not (fboundp 'eln-opt-toy-cl-letf))))

(ert-deftest eln-opt-build-respects-the-limit-and-skips-cached ()
  "At most the limit is built; cached ones are skipped; a second run builds 0."
  (eln-opt-test--with-state
    (let* ((cache (make-temp-file "eln-opt-tramp-cache-" t))
           (native-comp-eln-load-path (list (file-name-as-directory cache)))
           (names (cl-loop for i below 60 collect (intern (format "eln-opt-fake-%d" i))))
           (compiled 0))
      (unwind-protect
          (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () t))
                    ((symbol-function 'eln-opt-trampolines-predict) (lambda () names))
                    ((symbol-function 'comp-trampoline-compile)
                     (lambda (s)
                       (cl-incf compiled)
                       (let ((f (expand-file-name
                                 (comp-trampoline-filename s)
                                 (expand-file-name comp-native-version-dir cache))))
                         (make-directory (file-name-directory f) t)
                         (write-region "" nil f)))))
            (should (equal (eln-opt-trampolines-build)
                           (list :built 50 :failed 0 :left 10)))
            (should (= compiled 50))
            (should (equal (eln-opt-trampolines-build)
                           (list :built 10 :failed 0 :left 0)))
            (should (= compiled 60))
            (should (equal (eln-opt-trampolines-build)
                           (list :built 0 :failed 0 :left 0)))
            (should (= compiled 60)))
        (ignore-errors (delete-directory cache t))))))

(ert-deftest eln-opt-build-counts-failures-and-does-not-retry ()
  "A failing compile is counted once and skipped; the run goes on."
  (eln-opt-test--with-state
    (let ((native-comp-eln-load-path (list (make-temp-file "eln-opt-tc-" t)))
          (calls 0))
      (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () t))
                ((symbol-function 'eln-opt-trampolines-predict)
                 (lambda () '(eln-opt-bad-1 eln-opt-bad-2)))
                ((symbol-function 'comp-trampoline-compile)
                 (lambda (_) (cl-incf calls) (error "no libgccjit"))))
        (should (equal (eln-opt-trampolines-build) (list :built 0 :failed 2 :left 0)))
        (should (= calls 2))))))

(ert-deftest eln-opt-build-does-nothing-when-unavailable ()
  "Without the platform gate, no trampoline is compiled."
  (eln-opt-test--with-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
                ((symbol-function 'comp-trampoline-compile)
                 (lambda (_) (cl-incf calls))))
        (should (eq (eln-opt-trampolines-build) 'unavailable))
        (should (= calls 0))))))

(ert-deftest eln-opt-real-trampoline-lands-in-cache ()
  "On a native build, one real trampoline is compiled into a temporary cache."
  (skip-unless (native-comp-available-p))
  (eln-opt-test--with-state
    (let* ((cache (make-temp-file "eln-opt-real-cache-" t))
           (native-comp-eln-load-path (list (file-name-as-directory cache))))
      (unwind-protect
          (progn
            (should-not (eln-opt--trampoline-cached-p 'buffer-name))
            (comp-trampoline-compile 'buffer-name)
            (should (eln-opt--trampoline-cached-p 'buffer-name)))
        (ignore-errors (delete-directory cache t))))))

;;; Step 3: a stale .elc never shadows a newer .el

(ert-deftest eln-opt-stale-elc-never-shadows-newer-el ()
  "With `load-prefer-newer', a .el newer than its .elc is the one that loads."
  (let* ((dir (make-temp-file "eln-opt-stale-" t))
         (el (expand-file-name "eln-opt-stale-toy.el" dir))
         (elc (concat el "c")))
    (unwind-protect
        (let ((load-prefer-newer t)
              (byte-compile-verbose nil))
          (with-temp-file el
            (insert ";;; eln-opt-stale-toy.el -*- lexical-binding: t; -*-\n"
                    "(defun eln-opt-stale-toy-value () 1)\n"))
          (byte-compile-file el)
          (with-temp-file el
            (insert ";;; eln-opt-stale-toy.el -*- lexical-binding: t; -*-\n"
                    "(defun eln-opt-stale-toy-value () 2)\n"))
          ;; The .elc is older than the new source, as after an edit.
          (set-file-times elc (time-subtract (current-time) 100))
          (set-file-times el (current-time))
          (load el nil t)
          (should (= (eln-opt-stale-toy-value) 2))
          (should-not (byte-code-function-p (symbol-function 'eln-opt-stale-toy-value))))
      (ignore-errors (fmakunbound 'eln-opt-stale-toy-value))
      (ignore-errors (delete-directory dir t)))))

;;; Step 4: ledger, lock, fingerprint, verification, audit

(defun eln-opt-test--dead-pid ()
  "A pid that no process has right now."
  (let ((p 999999))
    (while (process-attributes p) (setq p (1- p)))
    p))

(defun eln-opt-test--toy-src (dir name)
  (let ((src (expand-file-name (concat name ".el") dir)))
    (with-temp-file src
      (insert (format ";;; %s.el -*- lexical-binding: t; -*-\n(defun %s-fn () 1)\n"
                      name name)))
    src))

(ert-deftest eln-opt-ledger-failure-policy ()
  "A failure waits a day; two failures end it until the key changes."
  (eln-opt-test--with-state
    (let* ((key '("/src/a.el" "sha" (1)))
           (now (eln-opt--now)))
      (should (eln-opt--may-attempt-p key now))
      (eln-opt--ledger-set key (list :status 'failed :fails 1 :time now))
      (should-not (eln-opt--may-attempt-p key now))
      (should (eln-opt--may-attempt-p key (+ now eln-opt-retry-interval 1)))
      (eln-opt--ledger-set key (list :status 'failed :fails 2 :time now))
      (should-not (eln-opt--may-attempt-p key (+ now (* 10 eln-opt-retry-interval))))
      (eln-opt--ledger-set key (list :status 'quarantined))
      (should-not (eln-opt--may-attempt-p key (+ now (* 10 eln-opt-retry-interval)))))))

(ert-deftest eln-opt-source-key-changes-with-compile-settings ()
  "Changing a setting that does not show in the eln name still changes the key."
  (eln-opt-test--with-state
    (let* ((dir (make-temp-file "eln-opt-key-" t))
           (src (eln-opt-test--toy-src dir "keyed"))
           (k1 (eln-opt-source-key src))
           (k2 (let ((native-comp-speed 1)) (eln-opt-source-key src))))
      (should-not (equal k1 k2))
      (should (equal k1 (eln-opt-source-key src))))))

(ert-deftest eln-opt-lock-is-exclusive-and-takes-over-stale ()
  "A live owner keeps the lock; a dead owner's lock is taken over once."
  (eln-opt-test--with-state
    (should (eln-opt--lock-acquire))
    (should-not (eln-opt--lock-acquire))
    (eln-opt--lock-release)
    (should-not (file-exists-p (eln-opt--lock-dir)))
    ;; Simulate a crashed run: its lock names a process that does not exist.
    (make-directory (eln-opt--lock-dir) t)
    (eln-opt--write-eld (eln-opt--owner-file (eln-opt--lock-dir))
                       (list :pid (eln-opt-test--dead-pid) :time (eln-opt--now)))
    (should (eln-opt--lock-acquire))
    (should (= (plist-get (eln-opt--read-eld (eln-opt--owner-file (eln-opt--lock-dir))) :pid)
               (emacs-pid)))
    (eln-opt--lock-release)))

(ert-deftest eln-opt-lock-old-owner-counts-as-stale ()
  "A lock older than `eln-opt-lock-max-age' is stale even if its pid lives."
  (eln-opt-test--with-state
    (make-directory (eln-opt--lock-dir) t)
    (eln-opt--write-eld (eln-opt--owner-file (eln-opt--lock-dir))
                       (list :pid (emacs-pid)
                             :time (- (eln-opt--now) eln-opt-lock-max-age 10)))
    (should (eln-opt--lock-stale-p (eln-opt--lock-dir)))
    (should (eln-opt--lock-acquire))
    (eln-opt--lock-release)))

(ert-deftest eln-opt-run-busy-when-locked ()
  "A run that cannot get the lock does nothing."
  (eln-opt-test--with-state
    (let ((work 0))
      (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () t))
                ((symbol-function 'eln-opt--work) (lambda () (cl-incf work) (list))))
        (should (eln-opt--lock-acquire))
        (should (eq (eln-opt-run) 'busy))
        (eln-opt--lock-release)
        (should (= work 0))))))

(ert-deftest eln-opt-second-run-does-no-work ()
  "With nothing changed, the second run skips the work entirely."
  (eln-opt-test--with-state
    (let ((work 0))
      (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () t))
                ((symbol-function 'eln-opt--work) (lambda () (cl-incf work) (list :compiled 0))))
        (should (eq (car (eln-opt-run)) 'ran))
        (should (= work 1))
        (should (eq (eln-opt-run) 'unchanged))
        (should (= work 1))
        (should-not (file-exists-p (eln-opt--lock-dir)))))))

(ert-deftest eln-opt-audit-records-a-missing-eln-and-allows-a-rebuild ()
  "A done entry whose eln is gone is recorded as missing, with a reason, and
may be built again.  It is not dropped silently."
  (eln-opt-test--with-state
    (let ((key '("/src/gone.el" "sha" (1))))
      (eln-opt--ledger-set key (list :status 'done :eln "/no/such.eln" :sha "x" :size 1))
      (eln-opt--audit)
      (should (eq (plist-get (eln-opt--ledger-get key) :status) 'missing))
      (should (string-match-p "gone" (plist-get (eln-opt--ledger-get key) :reason)))
      (should (eln-opt--may-attempt-p key (eln-opt--now))))))

(ert-deftest eln-opt-audit-discards-tampered-and-broken-eln ()
  "A changed or unloadable eln is deleted and counted as a failure."
  (eln-opt-test--with-state
    (let* ((eln (expand-file-name "bad.eln" eln-opt-state-dir))
           (key '("/src/bad.el" "sha" (1))))
      (make-directory eln-opt-state-dir t)
      ;; Recorded hash does not match the file: deleted at once.
      (with-temp-file eln (insert "not an eln"))
      (eln-opt--ledger-set key (list :status 'done :eln eln :sha "wrong" :size 1))
      (eln-opt--audit)
      (should-not (file-exists-p eln))
      (should (= (plist-get (eln-opt--ledger-get key) :fails) 1))
      (should (eq (plist-get (eln-opt--ledger-get key) :status) 'failed))
      ;; Recorded hash matches but the file cannot be loaded: deleted too.
      (with-temp-file eln (insert "not an eln either"))
      (eln-opt--ledger-set key (list :status 'done :eln eln
                                    :sha (eln-opt--file-sha eln)
                                    :size (eln-opt--file-size eln)))
      (eln-opt--audit)
      (should-not (file-exists-p eln))
      (should (eq (plist-get (eln-opt--ledger-get key) :status) 'failed)))))

(ert-deftest eln-opt-child-sees-our-load-path ()
  "A fresh child can require a file that is only on our load-path."
  (let* ((dir (make-temp-file "eln-opt-dep-" t))
         (load-path (cons dir load-path)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "eln-opt-dep-probe.el" dir)
            (insert "(provide 'eln-opt-dep-probe)\n"))
          (let ((r (eln-opt--child '(require 'eln-opt-dep-probe))))
            (should (= (car r) 0))))
      (delete-directory dir t))))

(ert-deftest eln-opt-verify-accepts-native-and-rejects-garbage ()
  "Verification loads the eln and checks that the defun is native."
  (skip-unless (native-comp-available-p))
  (eln-opt-test--with-state
    (let* ((dir (make-temp-file "eln-opt-verify-" t))
           (src (eln-opt-test--toy-src dir "verified"))
           (eln (expand-file-name "verified.eln" dir)))
      (native-compile src eln)
      (should (null (eln-opt-verify src eln)))
      (with-temp-file eln (insert "garbage"))
      (should (stringp (eln-opt-verify src eln))))))

;; Step 4: reset removes only what the ledger recorded

(ert-deftest eln-opt-reset-removes-only-recorded-elns ()
  "Reset deletes recorded elns and the ledger, and leaves other files alone."
  (eln-opt-test--with-state
    (let* ((recorded (expand-file-name "recorded.eln" eln-opt-state-dir))
           (other (expand-file-name "not-ours.eln" eln-opt-state-dir)))
      (make-directory eln-opt-state-dir t)
      (with-temp-file recorded (insert "x"))
      (with-temp-file other (insert "y"))
      (eln-opt--ledger-set '("/src/r.el" "sha" (1))
                          (list :status 'done :eln recorded :sha "s" :size 1))
      (should (= (eln-opt-reset) 1))
      (should-not (file-exists-p recorded))
      (should-not (file-exists-p (eln-opt--ledger-file)))
      (should (file-exists-p other))
      (should (= (eln-opt-reset) 0)))))

;;; Step 5: the ranking report (no compiling)

(ert-deftest eln-opt-rank-file-key-drops-autoloads ()
  "Autoload stubs are not a place to compile; other names lose .el/.elc/.gz."
  (should (null (eln-opt-rank--file-key "/x/cape-autoloads.el")))
  (should (null (eln-opt-rank--file-key "/x/evil-autoloads.elc")))
  (should (equal (eln-opt-rank--file-key "/x/winum.elc") "winum"))
  (should (equal (eln-opt-rank--file-key "/x/org/org.el.gz") "org")))

(ert-deftest eln-opt-rank-respects-no-native-cookie ()
  "A file that opts out with a local variable is recognised."
  (let ((dir (make-temp-file "eln-opt-cookie-" t)))
    (unwind-protect
        (let ((opt (expand-file-name "opt.el" dir))
              (plain (expand-file-name "plain.el" dir)))
          (with-temp-file opt
            (insert ";;; opt.el -*- lexical-binding: t; -*-\n;; Local Variables:\n;; no-native-compile: t\n;; End:\n"))
          (with-temp-file plain (insert ";;; plain.el -*- lexical-binding: t; -*-\n"))
          (should (eln-opt-rank--no-native-p opt))
          (should-not (eln-opt-rank--no-native-p plain)))
      (delete-directory dir t))))

(ert-deftest eln-opt-rank-heat-spreads-with-decay-and-stops-at-the-level-limit ()
  "Heat falls along a call chain and stops after `eln-opt-rank-heat-levels' steps."
  (let* ((edges (make-hash-table :test #'eq))
         (chain '(c0 c1 c2 c3 c4 c5 c6)))
    (cl-loop for (a b) on chain while b do (puthash a (list b) edges))
    (let ((heat (eln-opt-rank--propagate '((c0 . 1.0)) edges)))
      (should (> (gethash 'c1 heat) (gethash 'c2 heat)))
      (should (> (gethash 'c2 heat) 0))
      (should (= (gethash 'c6 heat 0.0) 0.0))
      (should (= (gethash 'c5 heat 0.0) 0.0)))))

(ert-deftest eln-opt-rank-savehist-is-capped-and-kill-ring-is-not-read ()
  "Savehist commands weigh at most the cap; the kill ring is never parsed."
  (let ((dir (make-temp-file "eln-opt-profile-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "history" dir)
            (insert "(setq extended-command-history '(\"forward-char\" \"backward-char\"))\n"
                    "(setq kill-ring '(\"text\" \"forward-word\"))\n"))
          (with-temp-file (expand-file-name "recentf.eld" dir)
            (insert "(setq recentf-list '(\"/a/b.el\"))\n"))
          (let* ((info (eln-opt-rank--roots dir))
                 (saved (plist-get info :savehist)))
            (should (= (length saved) 2))
            (dolist (r saved)
              (should (<= (cdr r) eln-opt-rank-savehist-cap)))
            (should-not (assq 'forward-word (plist-get info :roots)))))
      (delete-directory dir t))))

(ert-deftest eln-opt-trusted-file-refuses-symlink-out-of-the-roots ()
  "A symlink inside our lisp/ that points outside is not trusted."
  (let* ((root (make-temp-file "eln-opt-dot-" t))
         (outside (make-temp-file "eln-opt-out-" t))
         (lisp (expand-file-name "emacs/lisp" root))
         (xl-dotfiles (file-name-as-directory root)))
    (unwind-protect
        (progn
          (make-directory lisp t)
          (with-temp-file (expand-file-name "inside.el" lisp) (insert ";;; inside\n"))
          (with-temp-file (expand-file-name "outside.el" outside) (insert ";;; outside\n"))
          (make-symbolic-link (expand-file-name "outside.el" outside)
                              (expand-file-name "link.el" lisp))
          (should (eln-opt-trusted-file-p (expand-file-name "inside.el" lisp)))
          (should-not (eln-opt-trusted-file-p (expand-file-name "link.el" lisp)))
          (should-not (eln-opt-trusted-file-p (expand-file-name "outside.el" outside))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest eln-opt-rank-graph-never-compiles-interpreted-code ()
  "The report must have no side effects: an interpreted function stays interpreted."
  (let* ((name (intern "eln-opt-rank-test-interpreted"))
         (src (format "(defun %s (x) (+ x 1))" name)))
    (unwind-protect
        (progn
          ;; Put a defun in load-history, interpreted, under a fake file name.
          (let ((load-file-name "/tmp/eln-opt-rank-test-fake.el"))
            (eval (read src) t))
          (push (cons "/tmp/eln-opt-rank-test-fake.el"
                      (list (cons 'defun name)))
                load-history)
          (eln-opt-rank--graph user-emacs-directory)
          (should (interpreted-function-p (symbol-function name))))
      (setq load-history (seq-remove (lambda (e) (equal (car e) "/tmp/eln-opt-rank-test-fake.el"))
                                     load-history))
      (fmakunbound name))))

;;; Step 6: background compile, under stubs for the child process

(defmacro eln-opt-test--with-tier2 (&rest body)
  "Temporary state folder, and a temporary dotfiles tree with our lisp/."
  (declare (indent 0))
  `(eln-opt-test--with-state
     (let* ((root (make-temp-file "eln-opt-tier2-" t))
            (xl-dotfiles (file-name-as-directory root))
            (eln-opt-crash-dir (expand-file-name "crashes/" root))
            (lisp (expand-file-name "emacs/lisp" root)))
       (make-directory lisp t)
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-directory root t))))))

(defun eln-opt-test--row (path ratio)
  (list :file (file-name-sans-extension (file-name-nondirectory path))
        :path path :ratio ratio :score 1.0 :size 1000 :top nil :trusted t))

(defun eln-opt-test--src (dir name)
  (let ((src (expand-file-name (concat name ".el") dir)))
    (with-temp-file src (insert (format ";;; %s.el\n(defun %s-fn () 1)\n" name name)))
    src))

(ert-deftest eln-opt-timeout-kills-a-child-that-runs-too-long ()
  "A child that outlives its time is killed and reported; a quick one is ok."
  (skip-unless (file-executable-p (expand-file-name invocation-name invocation-directory)))
  (should (eq (eln-opt--run-with-timeout '("-Q" "--batch" "--eval" "(princ 1)") 60) 'ok))
  (let ((r (eln-opt--run-with-timeout '("-Q" "--batch" "--eval" "(sleep-for 30)") 1)))
    (should (stringp r))
    (should (string-match-p "timed out" r))))

(ert-deftest eln-opt-candidate-refuses-untrusted-optout-big-and-low-ranked ()
  "Each rule that keeps a file out of the compile list is enforced here."
  (eln-opt-test--with-tier2
    (let* ((good (eln-opt-test--src lisp "good"))
           (outside (make-temp-file "eln-opt-out-" t))
           (outside-src (eln-opt-test--src outside "escaped"))
           (link (expand-file-name "link.el" lisp))
           (optout (eln-opt-test--src lisp "optout"))
           (big (expand-file-name "big.el" lisp)))
      (make-symbolic-link outside-src link)
      (with-temp-file optout (insert ";;; optout.el\n;; Local Variables:\n;; no-native-compile: t\n;; End:\n"))
      (with-temp-file big (insert (make-string (+ 10 eln-opt-max-source-bytes) ?x)))
      (should (eln-opt--candidate-p (eln-opt-test--row good 100.0)))
      (should-not (eln-opt--candidate-p (eln-opt-test--row link 100.0)))
      (should-not (eln-opt--candidate-p (eln-opt-test--row optout 100.0)))
      (should-not (eln-opt--candidate-p (eln-opt-test--row big 100.0)))
      (should-not (eln-opt--candidate-p (eln-opt-test--row good 1.0)))
      (delete-directory outside t))))

(ert-deftest eln-opt-work-compiles_budget_then_nothing_on_the_second_run ()
  "Within the budget, verified elns are recorded; a second run compiles none."
  (eln-opt-test--with-tier2
    (let* ((srcs (mapcar (lambda (n) (eln-opt-test--src lisp n)) '("a1" "a2" "a3" "a4")))
           (rows (mapcar (lambda (f) (eln-opt-test--row f 100.0)) srcs))
           (children 0))
      (cl-letf (((symbol-function 'eln-opt-rank--compute)
                 (lambda (_) (list :rows rows)))
                ((symbol-function 'eln-opt--child-compile)
                 (lambda (src eln)
                   (cl-incf children)
                   (make-directory (file-name-directory eln) t)
                   (with-temp-file eln (insert "fake eln"))
                   'ok))
                ((symbol-function 'eln-opt-verify) (lambda (_src _eln &optional _names) nil)))
        (let ((first (eln-opt--work)))
          (should (= (plist-get first :compiled) (min 4 eln-opt-compile-budget)))
          (should (= children (min 4 eln-opt-compile-budget))))
        (let ((second (eln-opt--work)))
          (should (= (plist-get second :compiled) 0))
          (should (= children (min 4 eln-opt-compile-budget))))))))

(ert-deftest eln-opt-timeout-is-not-retried-within-the-day ()
  "A compile that times out is counted failed, and is not attempted again soon."
  (eln-opt-test--with-tier2
    (let* ((src (eln-opt-test--src lisp "slow"))
           (rows (list (eln-opt-test--row src 100.0)))
           (children 0))
      (cl-letf (((symbol-function 'eln-opt-rank--compute) (lambda (_) (list :rows rows)))
                ((symbol-function 'eln-opt--child-compile)
                 (lambda (_ _) (cl-incf children) "timed out after 180 s")))
        (should (= (plist-get (eln-opt--work) :failed) 1))
        (should (= (plist-get (eln-opt--work) :compiled) 0))
        (should (= children 1))))))

(ert-deftest eln-opt-crash-report-quarantines-the-named-eln ()
  "An eln named in a fresh crash report is deleted and its key quarantined."
  (eln-opt-test--with-tier2
    (let* ((eln (expand-file-name "crashy-abc-def.eln" eln-opt-state-dir))
           (key '("/src/crashy.el" "sha" (1)))
           (report-dir eln-opt-crash-dir))
      (make-directory eln-opt-state-dir t)
      (with-temp-file eln (insert "eln"))
      (eln-opt--ledger-set key (list :status 'done :eln eln :sha "s" :size 3))
      (make-directory report-dir t)
      (with-temp-file (expand-file-name "Emacs-2026-11-24.ips" report-dir)
        (insert "{\"threads\": [{\"frames\": [\"crashy-abc-def.eln\"]}]}"))
      (should (= (eln-opt--crash-scan 0) 1))
      (should-not (file-exists-p eln))
      (should (eq (plist-get (eln-opt--ledger-get key) :status) 'quarantined))
      (should-not (eln-opt--may-attempt-p key (+ (eln-opt--now) (* 10 eln-opt-retry-interval)))))))

(ert-deftest eln-opt-total-size-cap-stops_compiling ()
  "Past the total eln size cap, a run compiles nothing and says so."
  (eln-opt-test--with-tier2
    (let* ((src (eln-opt-test--src lisp "capped"))
           (children 0))
      (eln-opt--ledger-set '("/src/old.el" "sha" (1))
                          (list :status 'done :eln "/x" :sha "s" :size 100))
      (cl-letf (((symbol-function 'eln-opt-rank--compute)
                 (lambda (_) (list :rows (list (eln-opt-test--row src 100.0)))))
                ((symbol-function 'eln-opt--child-compile)
                 (lambda (_ _) (cl-incf children) 'ok)))
        (let ((eln-opt-max-total-bytes 50))
          (let ((r (eln-opt--work)))
            (should (plist-get r :capped))
            (should (= children 0))))))))

;;; The in-Emacs trigger (`eln-opt-auto-arm' and its timers), never in batch

(defmacro eln-opt-test--with-auto (&rest body)
  "A temporary profile folder, an Emacs that has been open for an hour, on AC
power, native compilation available, and timers recorded instead of armed."
  (declare (indent 0))
  `(let* ((user-emacs-directory (file-name-as-directory (make-temp-file "eln-opt-auto-" t)))
          (before-init-time (time-subtract nil 3600))
          (eln-opt-auto-enable t)
          (eln-opt-auto--process nil)
          (eln-opt-auto--timer nil)
          (timers nil))
     (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
               ((symbol-function 'eln-opt-auto--on-battery-p) (lambda () nil))
               ((symbol-function 'run-with-timer)
                (lambda (secs _repeat fn) (push (list 'timer secs fn) timers) 'fake-timer))
               ((symbol-function 'run-with-idle-timer)
                (lambda (secs _repeat fn) (push (list 'idle secs fn) timers) 'fake-idle))
               ((symbol-function 'eln-opt-auto--cancel)
                (lambda () (setq eln-opt-auto--timer nil))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-directory user-emacs-directory t))))))

(defun eln-opt-test--touch-last-run (age)
  "Write a last-run record that is AGE seconds old."
  (let ((f (eln-opt-auto--last-run-file)))
    (make-directory (file-name-directory f) t)
    (with-temp-file f (insert "(:time 0)\n"))
    (set-file-times f (time-subtract nil age))))

(ert-deftest eln-opt-auto-decides-when-to-run ()
  "Each reason not to run is reported; with none left, it may run."
  (eln-opt-test--with-auto
    (should (null (eln-opt-auto--decision)))
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (should (eq (eln-opt-auto--decision) 'unavailable)))
    (let ((eln-opt-auto-enable nil))
      (should (eq (eln-opt-auto--decision) 'disabled)))
    (let ((process-environment (cons "ELN_OPT=0" process-environment)))
      (should (eq (eln-opt-auto--decision) 'disabled)))
    (let ((before-init-time (time-subtract nil 30)))
      (should (eq (eln-opt-auto--decision) 'too-early)))
    (cl-letf (((symbol-function 'eln-opt-auto--on-battery-p) (lambda () t)))
      (should (eq (eln-opt-auto--decision) 'battery)))
    (eln-opt-test--touch-last-run 3600)
    (should (eq (eln-opt-auto--decision) 'recent))
    (eln-opt-test--touch-last-run (+ eln-opt-auto-interval 60))
    (should (null (eln-opt-auto--decision)))))

(ert-deftest eln-opt-auto-tick-waits-for-idle-or-checks-again-later ()
  "Allowed: an idle timer.  Too early: a timer for the rest.  Unavailable: none."
  (eln-opt-test--with-auto
    (eln-opt-auto--tick)
    (should (equal (car timers) (list 'idle eln-opt-auto-idle #'eln-opt-auto--start)))
    (setq timers nil)
    (let ((before-init-time (time-subtract nil 60)))
      (eln-opt-auto--tick)
      (should (eq (car (car timers)) 'timer))
      (should (<= (abs (- (nth 1 (car timers)) (- eln-opt-auto-uptime 60))) 2)))
    (setq timers nil)
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (eln-opt-auto--tick)
      (should (null timers)))))

(ert-deftest eln-opt-auto-never-arms-in-batch ()
  "Setup, tests, and the background run itself load init in batch: no timer."
  (eln-opt-test--with-auto
    (let ((noninteractive t))
      (eln-opt-auto-arm)
      (should (null timers)))
    (let ((noninteractive nil))
      (eln-opt-auto-arm)
      (should (eq (car (car timers)) 'timer)))))

(ert-deftest eln-opt-auto-start-rechecks-and-spawns-nothing-when-not-allowed ()
  "If the user went on battery while idle, no process starts."
  (eln-opt-test--with-auto
    (let ((spawned 0))
      (cl-letf (((symbol-function 'eln-opt-auto--on-battery-p) (lambda () t))
                ((symbol-function 'make-process) (lambda (&rest _) (cl-incf spawned) nil)))
        (eln-opt-auto--start)
        (should (= spawned 0))
        (should (equal (car timers) (list 'timer eln-opt-auto-retry #'eln-opt-auto--tick)))))))

(ert-deftest eln-opt-auto-command-runs-at-background-priority ()
  "The background run uses taskpolicy -b when present, nice -n 19 otherwise."
  (eln-opt-test--with-auto
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (and (equal p "taskpolicy") "/usr/sbin/taskpolicy"))))
      (should (equal (seq-take (eln-opt-auto--command) 2) '("taskpolicy" "-b"))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (and (equal p "nice") "/usr/bin/nice"))))
      (should (equal (seq-take (eln-opt-auto--command) 3) '("nice" "-n" "19"))))
    (should (member "--batch" (eln-opt-auto--command)))))

;;; Query: which functions are native

(ert-deftest eln-opt-native-list-shows-a-native-toy ()
  "A function that is native in this Emacs shows up in the native list."
  (skip-unless (native-comp-available-p))
  (eln-opt-test--with-state
    (let* ((dir (make-temp-file "eln-opt-native-toy-" t))
           (src (eln-opt-test--toy-src dir "native-toy"))
           (eln (expand-file-name "native-toy.eln" dir)))
      (unwind-protect
          (progn
            (native-compile src eln)
            (load eln nil t)
            (let* ((info (eln-opt-native-list))
                   (names (apply #'append (mapcar #'cdr (plist-get info :files)))))
              (should (memq 'native-toy-fn names))
              (should (> (plist-get info :total) 0))
              (should (stringp (with-output-to-string
                                 (let ((noninteractive t))
                                   (eln-opt-native-report)))))))
        (ignore-errors (fmakunbound 'native-toy-fn))
        (ignore-errors (delete-directory dir t))))))

;;; The report view: same format, computed in a child, with a spinner

(ert-deftest eln-opt-report-parses-the-child-output-after-the-marker ()
  "Loading messages before the marker are dropped; without it, nothing is shown."
  (should (equal (eln-opt-rank--report-from-output
                  (concat "Loading x.el...\nnoise\n\n" eln-opt-rank--marker "\nreport line\n"))
                 "report line\n"))
  (should (null (eln-opt-rank--report-from-output "Loading x.el...\nno marker\n"))))

(ert-deftest eln-opt-report-child-is-a-fresh-batch-with-the-profile ()
  "The child loads this profile's init and prints the report with the marker."
  (let ((cmd (eln-opt-rank--child-command eln-opt-rank--report-form)))
    (should (member "--batch" cmd))
    (should (member "--init-directory" cmd))
    (should (cl-some (lambda (a) (and (stringp a) (string-suffix-p "init.el" a))) cmd))
    (should (cl-some (lambda (a) (and (stringp a) (string-match-p "eln-opt-report" a))) cmd))))

;;; The one-line summary of a run

(ert-deftest eln-opt-run-summary-says-what-happened-in-words ()
  "Each result of `eln-opt-run' is one readable sentence, not Lisp data."
  (should (string-match-p "not available" (eln-opt-run-summary 'unavailable)))
  (should (string-match-p "nothing changed" (eln-opt-run-summary 'unchanged)))
  (should (string-match-p "compiled 2 file(s), failed 0"
                          (eln-opt-run-summary '(ran :compiled 2 :failed 0 :skipped 1
                                                    :quarantined 0 :capped nil))))
  (should (string-match-p "Stopped early"
                          (eln-opt-run-summary '(ran :compiled 0 :failed 0 :skipped 0
                                                    :quarantined 0 :capped t)))))

(provide 'eln-opt-tests)
;;; eln-opt-tests.el ends here
