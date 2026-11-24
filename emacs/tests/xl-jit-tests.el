;;; xl-jit-tests.el --- ERT tests for xl-jit -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L emacs/lisp -l emacs/tests/xl-jit-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Every test works in its own temporary state folder.  Each one checks the
;; final state and that a second run does no compilation at all.

(require 'ert)
(require 'cl-lib)
(require 'xl-jit)
(require 'xl-jit-rank)
(require 'xl-jit-auto)
(defvar xl-dotfiles nil "Bound by the profile; tests bind it to a temporary folder.")

(defmacro xl-jit-test--with-state (&rest body)
  "Run BODY with `xl-jit-state-dir' in a fresh temporary folder."
  (declare (indent 0))
  `(let ((xl-jit-state-dir (make-temp-file "xl-jit-state-" t)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory xl-jit-state-dir t)))))

(defmacro xl-jit-test--count-canary (counter &rest body)
  "Run BODY with `xl-jit--canary' counting its calls into COUNTER."
  (declare (indent 1))
  `(let ((orig (symbol-function 'xl-jit--canary)))
     (cl-letf (((symbol-function 'xl-jit--canary)
                (lambda () (cl-incf ,counter) (funcall orig))))
       ,@body)))

(ert-deftest xl-jit-unavailable-writes-only-platform-file ()
  "Without native compilation, a run stops at the gate and writes one file."
  (xl-jit-test--with-state
    (let ((canary-calls 0))
      (xl-jit-test--count-canary canary-calls
        (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
          (should (eq (xl-jit-run) 'unavailable))
          (should (eq (xl-jit-run) 'unavailable))))
      (should (= canary-calls 0))
      (should (equal (directory-files xl-jit-state-dir nil "\\`[^.]")
                     '("platform.eld"))))))

(ert-deftest xl-jit-canary-failure-trips-breaker ()
  "A canary that fails is cached as unavailable and not retried."
  (xl-jit-test--with-state
    (let ((compile-calls 0))
      (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
                ((symbol-function 'native-compile)
                 (lambda (&rest _) (cl-incf compile-calls) (error "libgccjit missing"))))
        (should-not (xl-jit-platform-available-p))
        (should (eq (xl-jit-run) 'unavailable))
        (should-not (xl-jit-platform-available-p)))
      (should (= compile-calls 1))
      (let ((cached (xl-jit--read-eld (xl-jit--platform-file))))
        (should-not (plist-get cached :available))
        (should (string-match-p "libgccjit missing" (plist-get cached :reason)))))))

(ert-deftest xl-jit-binary-change-rechecks-once ()
  "A new binary key triggers exactly one new check, then the cache holds."
  (xl-jit-test--with-state
    (let ((canary-calls 0))
      (xl-jit-test--count-canary canary-calls
        (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
                  ((symbol-function 'xl-jit--canary) (lambda () (cl-incf canary-calls) nil)))
          (should (xl-jit-platform-available-p))
          (should (= canary-calls 1))
          (should (xl-jit-platform-available-p))
          (should (= canary-calls 1))
          ;; Pretend the binary was replaced: the stored key no longer matches.
          (xl-jit--write-eld (xl-jit--platform-file)
                             (list :key '("/old/emacs" 1 nil) :available nil :reason "old"))
          (should (xl-jit-platform-available-p))
          (should (= canary-calls 2))
          (should (xl-jit-platform-available-p))
          (should (= canary-calls 2)))))))

(ert-deftest xl-jit-real-canary-when-native-is-built-in ()
  "On a native build the real canary passes and is cached as available."
  (skip-unless (native-comp-available-p))
  (xl-jit-test--with-state
    (let ((canary-calls 0))
      (xl-jit-test--count-canary canary-calls
        (should (xl-jit-platform-available-p))
        (should (eq (plist-get (xl-jit--read-eld (xl-jit--platform-file)) :available) t))
        (should (xl-jit-platform-available-p))
        (should (= canary-calls 1))))
    (should (equal (directory-files xl-jit-state-dir nil "\\`[^.]")
                   '("platform.eld")))))

(defvar xl-jit-test--evaluated nil)

(ert-deftest xl-jit-state-file-is-read-with-read-only ()
  "A state file with #. is read but its form never runs."
  (xl-jit-test--with-state
    (make-directory xl-jit-state-dir t)
    (setq xl-jit-test--evaluated nil)
    (with-temp-file (xl-jit--platform-file)
      (insert "#.(setq xl-jit-test--evaluated t)"))
    (should (null (xl-jit--read-eld (xl-jit--platform-file))))
    (should-not xl-jit-test--evaluated)))

;;; Step 2: trampoline prediction and build

(defvar xl-jit-test--toy-loaded nil
  "Set by the toy .elc's top-level code.  Stays nil when the file is only read.")

(defun xl-jit-test--toy-elc (dir name source)
  "Byte-compile SOURCE into DIR/NAME.el and return the .elc path."
  (let* ((el (expand-file-name (concat name ".el") dir))
         (elc (concat el "c")))
    (with-temp-file el (insert source))
    (let ((byte-compile-warnings nil))
      (byte-compile-file el))
    elc))

(defconst xl-jit-test--toy-source
  ";;; toy.el -*- lexical-binding: t; -*-
(defvar xl-jit-test--toy-loaded nil)
(setq xl-jit-test--toy-loaded t)
(advice-add 'buffer-name :around (lambda (f &rest a) (apply f a)))
(defun xl-jit-toy-cl-letf ()
  (cl-letf (((symbol-function 'message) #'ignore)) 1))
(defun xl-jit-toy-alias () (defalias 'xl-jit-toy-x #'car))
")

(defmacro xl-jit-test--with-toy (elc-var &rest body)
  "Compile the toy into a temporary folder on `load-path', bind ELC-VAR, run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "xl-jit-toy-" t))
          (load-path (cons dir load-path))
          (,elc-var (xl-jit-test--toy-elc dir "toy" xl-jit-test--toy-source)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(ert-deftest xl-jit-predict-finds-advice-and-letf-but-not-aliases ()
  "The toy's advice-add and cl-letf targets are predicted; defalias is not."
  (xl-jit-test--with-toy elc
    (let ((only-toy (xl-jit--scan-elc elc)))
      (should (memq 'buffer-name only-toy))
      (should (memq 'message only-toy))
      (should-not (memq 'car only-toy)))))

(ert-deftest xl-jit-predict-never-loads-the-files ()
  "Scanning reads .elc files as data: the toy's top-level code does not run."
  (setq xl-jit-test--toy-loaded nil)
  (xl-jit-test--with-toy elc
    (xl-jit--scan-elc elc)
    (xl-jit-trampolines-predict)
    (should-not xl-jit-test--toy-loaded)
    (should-not (fboundp 'xl-jit-toy-cl-letf))))

(ert-deftest xl-jit-build-respects-the-limit-and-skips-cached ()
  "At most the limit is built; cached ones are skipped; a second run builds 0."
  (xl-jit-test--with-state
    (let* ((cache (make-temp-file "xl-jit-tramp-cache-" t))
           (native-comp-eln-load-path (list (file-name-as-directory cache)))
           (names (cl-loop for i below 60 collect (intern (format "xl-jit-fake-%d" i))))
           (compiled 0))
      (unwind-protect
          (cl-letf (((symbol-function 'xl-jit-platform-available-p) (lambda () t))
                    ((symbol-function 'xl-jit-trampolines-predict) (lambda () names))
                    ((symbol-function 'comp-trampoline-compile)
                     (lambda (s)
                       (cl-incf compiled)
                       (let ((f (expand-file-name
                                 (comp-trampoline-filename s)
                                 (expand-file-name comp-native-version-dir cache))))
                         (make-directory (file-name-directory f) t)
                         (write-region "" nil f)))))
            (should (equal (xl-jit-trampolines-build)
                           (list :built 50 :failed 0 :left 10)))
            (should (= compiled 50))
            (should (equal (xl-jit-trampolines-build)
                           (list :built 10 :failed 0 :left 0)))
            (should (= compiled 60))
            (should (equal (xl-jit-trampolines-build)
                           (list :built 0 :failed 0 :left 0)))
            (should (= compiled 60)))
        (ignore-errors (delete-directory cache t))))))

(ert-deftest xl-jit-build-counts-failures-and-does-not-retry ()
  "A failing compile is counted once and skipped; the run goes on."
  (xl-jit-test--with-state
    (let ((native-comp-eln-load-path (list (make-temp-file "xl-jit-tc-" t)))
          (calls 0))
      (cl-letf (((symbol-function 'xl-jit-platform-available-p) (lambda () t))
                ((symbol-function 'xl-jit-trampolines-predict)
                 (lambda () '(xl-jit-bad-1 xl-jit-bad-2)))
                ((symbol-function 'comp-trampoline-compile)
                 (lambda (_) (cl-incf calls) (error "no libgccjit"))))
        (should (equal (xl-jit-trampolines-build) (list :built 0 :failed 2 :left 0)))
        (should (= calls 2))))))

(ert-deftest xl-jit-build-does-nothing-when-unavailable ()
  "Without the platform gate, no trampoline is compiled."
  (xl-jit-test--with-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
                ((symbol-function 'comp-trampoline-compile)
                 (lambda (_) (cl-incf calls))))
        (should (eq (xl-jit-trampolines-build) 'unavailable))
        (should (= calls 0))))))

(ert-deftest xl-jit-real-trampoline-lands-in-cache ()
  "On a native build, one real trampoline is compiled into a temporary cache."
  (skip-unless (native-comp-available-p))
  (xl-jit-test--with-state
    (let* ((cache (make-temp-file "xl-jit-real-cache-" t))
           (native-comp-eln-load-path (list (file-name-as-directory cache))))
      (unwind-protect
          (progn
            (should-not (xl-jit--trampoline-cached-p 'buffer-name))
            (comp-trampoline-compile 'buffer-name)
            (should (xl-jit--trampoline-cached-p 'buffer-name)))
        (ignore-errors (delete-directory cache t))))))

;;; Step 3: a stale .elc never shadows a newer .el

(ert-deftest xl-jit-stale-elc-never-shadows-newer-el ()
  "With `load-prefer-newer', a .el newer than its .elc is the one that loads."
  (let* ((dir (make-temp-file "xl-jit-stale-" t))
         (el (expand-file-name "xl-jit-stale-toy.el" dir))
         (elc (concat el "c")))
    (unwind-protect
        (let ((load-prefer-newer t)
              (byte-compile-verbose nil))
          (with-temp-file el
            (insert ";;; xl-jit-stale-toy.el -*- lexical-binding: t; -*-\n"
                    "(defun xl-jit-stale-toy-value () 1)\n"))
          (byte-compile-file el)
          (with-temp-file el
            (insert ";;; xl-jit-stale-toy.el -*- lexical-binding: t; -*-\n"
                    "(defun xl-jit-stale-toy-value () 2)\n"))
          ;; The .elc is older than the new source, as after an edit.
          (set-file-times elc (time-subtract (current-time) 100))
          (set-file-times el (current-time))
          (load el nil t)
          (should (= (xl-jit-stale-toy-value) 2))
          (should-not (byte-code-function-p (symbol-function 'xl-jit-stale-toy-value))))
      (ignore-errors (fmakunbound 'xl-jit-stale-toy-value))
      (ignore-errors (delete-directory dir t)))))

;;; Step 4: ledger, lock, fingerprint, verification, audit

(defun xl-jit-test--dead-pid ()
  "A pid that no process has right now."
  (let ((p 999999))
    (while (process-attributes p) (setq p (1- p)))
    p))

(defun xl-jit-test--toy-src (dir name)
  (let ((src (expand-file-name (concat name ".el") dir)))
    (with-temp-file src
      (insert (format ";;; %s.el -*- lexical-binding: t; -*-\n(defun %s-fn () 1)\n"
                      name name)))
    src))

(ert-deftest xl-jit-ledger-failure-policy ()
  "A failure waits a day; two failures end it until the key changes."
  (xl-jit-test--with-state
    (let* ((key '("/src/a.el" "sha" (1)))
           (now (xl-jit--now)))
      (should (xl-jit--may-attempt-p key now))
      (xl-jit--ledger-set key (list :status 'failed :fails 1 :time now))
      (should-not (xl-jit--may-attempt-p key now))
      (should (xl-jit--may-attempt-p key (+ now xl-jit-retry-interval 1)))
      (xl-jit--ledger-set key (list :status 'failed :fails 2 :time now))
      (should-not (xl-jit--may-attempt-p key (+ now (* 10 xl-jit-retry-interval))))
      (xl-jit--ledger-set key (list :status 'quarantined))
      (should-not (xl-jit--may-attempt-p key (+ now (* 10 xl-jit-retry-interval)))))))

(ert-deftest xl-jit-source-key-changes-with-compile-settings ()
  "Changing a setting that does not show in the eln name still changes the key."
  (xl-jit-test--with-state
    (let* ((dir (make-temp-file "xl-jit-key-" t))
           (src (xl-jit-test--toy-src dir "keyed"))
           (k1 (xl-jit-source-key src))
           (k2 (let ((native-comp-speed 1)) (xl-jit-source-key src))))
      (should-not (equal k1 k2))
      (should (equal k1 (xl-jit-source-key src))))))

(ert-deftest xl-jit-lock-is-exclusive-and-takes-over-stale ()
  "A live owner keeps the lock; a dead owner's lock is taken over once."
  (xl-jit-test--with-state
    (should (xl-jit--lock-acquire))
    (should-not (xl-jit--lock-acquire))
    (xl-jit--lock-release)
    (should-not (file-exists-p (xl-jit--lock-dir)))
    ;; Simulate a crashed run: its lock names a process that does not exist.
    (make-directory (xl-jit--lock-dir) t)
    (xl-jit--write-eld (xl-jit--owner-file (xl-jit--lock-dir))
                       (list :pid (xl-jit-test--dead-pid) :time (xl-jit--now)))
    (should (xl-jit--lock-acquire))
    (should (= (plist-get (xl-jit--read-eld (xl-jit--owner-file (xl-jit--lock-dir))) :pid)
               (emacs-pid)))
    (xl-jit--lock-release)))

(ert-deftest xl-jit-lock-old-owner-counts-as-stale ()
  "A lock older than `xl-jit-lock-max-age' is stale even if its pid lives."
  (xl-jit-test--with-state
    (make-directory (xl-jit--lock-dir) t)
    (xl-jit--write-eld (xl-jit--owner-file (xl-jit--lock-dir))
                       (list :pid (emacs-pid)
                             :time (- (xl-jit--now) xl-jit-lock-max-age 10)))
    (should (xl-jit--lock-stale-p (xl-jit--lock-dir)))
    (should (xl-jit--lock-acquire))
    (xl-jit--lock-release)))

(ert-deftest xl-jit-run-busy-when-locked ()
  "A run that cannot get the lock does nothing."
  (xl-jit-test--with-state
    (let ((work 0))
      (cl-letf (((symbol-function 'xl-jit-platform-available-p) (lambda () t))
                ((symbol-function 'xl-jit--work) (lambda () (cl-incf work) (list))))
        (should (xl-jit--lock-acquire))
        (should (eq (xl-jit-run) 'busy))
        (xl-jit--lock-release)
        (should (= work 0))))))

(ert-deftest xl-jit-second-run-does-no-work ()
  "With nothing changed, the second run skips the work entirely."
  (xl-jit-test--with-state
    (let ((work 0))
      (cl-letf (((symbol-function 'xl-jit-platform-available-p) (lambda () t))
                ((symbol-function 'xl-jit--work) (lambda () (cl-incf work) (list :compiled 0))))
        (should (eq (car (xl-jit-run)) 'ran))
        (should (= work 1))
        (should (eq (xl-jit-run) 'unchanged))
        (should (= work 1))
        (should-not (file-exists-p (xl-jit--lock-dir)))))))

(ert-deftest xl-jit-audit-drops-entries-whose-eln-is-missing ()
  "A done entry whose eln is gone is removed, so the source can be built again."
  (xl-jit-test--with-state
    (let ((key '("/src/gone.el" "sha" (1))))
      (xl-jit--ledger-set key (list :status 'done :eln "/no/such.eln" :sha "x" :size 1))
      (xl-jit--audit)
      (should-not (xl-jit--ledger-get key)))))

(ert-deftest xl-jit-audit-discards-tampered-and-broken-eln ()
  "A changed or unloadable eln is deleted and counted as a failure."
  (xl-jit-test--with-state
    (let* ((eln (expand-file-name "bad.eln" xl-jit-state-dir))
           (key '("/src/bad.el" "sha" (1))))
      (make-directory xl-jit-state-dir t)
      ;; Recorded hash does not match the file: deleted at once.
      (with-temp-file eln (insert "not an eln"))
      (xl-jit--ledger-set key (list :status 'done :eln eln :sha "wrong" :size 1))
      (xl-jit--audit)
      (should-not (file-exists-p eln))
      (should (= (plist-get (xl-jit--ledger-get key) :fails) 1))
      (should (eq (plist-get (xl-jit--ledger-get key) :status) 'failed))
      ;; Recorded hash matches but the file cannot be loaded: deleted too.
      (with-temp-file eln (insert "not an eln either"))
      (xl-jit--ledger-set key (list :status 'done :eln eln
                                    :sha (xl-jit--file-sha eln)
                                    :size (xl-jit--file-size eln)))
      (xl-jit--audit)
      (should-not (file-exists-p eln))
      (should (eq (plist-get (xl-jit--ledger-get key) :status) 'failed)))))

(ert-deftest xl-jit-child-sees-our-load-path ()
  "A fresh child can require a file that is only on our load-path."
  (let* ((dir (make-temp-file "xl-jit-dep-" t))
         (load-path (cons dir load-path)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "xl-jit-dep-probe.el" dir)
            (insert "(provide 'xl-jit-dep-probe)\n"))
          (let ((r (xl-jit--child '(require 'xl-jit-dep-probe))))
            (should (= (car r) 0))))
      (delete-directory dir t))))

(ert-deftest xl-jit-verify-accepts-native-and-rejects-garbage ()
  "Verification loads the eln and checks that the defun is native."
  (skip-unless (native-comp-available-p))
  (xl-jit-test--with-state
    (let* ((dir (make-temp-file "xl-jit-verify-" t))
           (src (xl-jit-test--toy-src dir "verified"))
           (eln (expand-file-name "verified.eln" dir)))
      (native-compile src eln)
      (should (null (xl-jit-verify src eln)))
      (with-temp-file eln (insert "garbage"))
      (should (stringp (xl-jit-verify src eln))))))

;; Step 4: reset removes only what the ledger recorded

(ert-deftest xl-jit-reset-removes-only-recorded-elns ()
  "Reset deletes recorded elns and the ledger, and leaves other files alone."
  (xl-jit-test--with-state
    (let* ((recorded (expand-file-name "recorded.eln" xl-jit-state-dir))
           (other (expand-file-name "not-ours.eln" xl-jit-state-dir)))
      (make-directory xl-jit-state-dir t)
      (with-temp-file recorded (insert "x"))
      (with-temp-file other (insert "y"))
      (xl-jit--ledger-set '("/src/r.el" "sha" (1))
                          (list :status 'done :eln recorded :sha "s" :size 1))
      (should (= (xl-jit-reset) 1))
      (should-not (file-exists-p recorded))
      (should-not (file-exists-p (xl-jit--ledger-file)))
      (should (file-exists-p other))
      (should (= (xl-jit-reset) 0)))))

;;; Step 5: the ranking report (no compiling)

(ert-deftest xl-jit-rank-file-key-drops-autoloads ()
  "Autoload stubs are not a place to compile; other names lose .el/.elc/.gz."
  (should (null (xl-jit-rank--file-key "/x/cape-autoloads.el")))
  (should (null (xl-jit-rank--file-key "/x/evil-autoloads.elc")))
  (should (equal (xl-jit-rank--file-key "/x/winum.elc") "winum"))
  (should (equal (xl-jit-rank--file-key "/x/org/org.el.gz") "org")))

(ert-deftest xl-jit-rank-respects-no-native-cookie ()
  "A file that opts out with a local variable is recognised."
  (let ((dir (make-temp-file "xl-jit-cookie-" t)))
    (unwind-protect
        (let ((opt (expand-file-name "opt.el" dir))
              (plain (expand-file-name "plain.el" dir)))
          (with-temp-file opt
            (insert ";;; opt.el -*- lexical-binding: t; -*-\n;; Local Variables:\n;; no-native-compile: t\n;; End:\n"))
          (with-temp-file plain (insert ";;; plain.el -*- lexical-binding: t; -*-\n"))
          (should (xl-jit-rank--no-native-p opt))
          (should-not (xl-jit-rank--no-native-p plain)))
      (delete-directory dir t))))

(ert-deftest xl-jit-rank-heat-spreads-with-decay-and-stops-at-the-level-limit ()
  "Heat falls along a call chain and stops after `xl-jit-rank-heat-levels' steps."
  (let* ((edges (make-hash-table :test #'eq))
         (chain '(c0 c1 c2 c3 c4 c5 c6)))
    (cl-loop for (a b) on chain while b do (puthash a (list b) edges))
    (let ((heat (xl-jit-rank--propagate '((c0 . 1.0)) edges)))
      (should (> (gethash 'c1 heat) (gethash 'c2 heat)))
      (should (> (gethash 'c2 heat) 0))
      (should (= (gethash 'c6 heat 0.0) 0.0))
      (should (= (gethash 'c5 heat 0.0) 0.0)))))

(ert-deftest xl-jit-rank-savehist-is-capped-and-kill-ring-is-not-read ()
  "Savehist commands weigh at most the cap; the kill ring is never parsed."
  (let ((dir (make-temp-file "xl-jit-profile-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "history" dir)
            (insert "(setq extended-command-history '(\"forward-char\" \"backward-char\"))\n"
                    "(setq kill-ring '(\"text\" \"forward-word\"))\n"))
          (with-temp-file (expand-file-name "recentf.eld" dir)
            (insert "(setq recentf-list '(\"/a/b.el\"))\n"))
          (let* ((info (xl-jit-rank--roots dir))
                 (saved (plist-get info :savehist)))
            (should (= (length saved) 2))
            (dolist (r saved)
              (should (<= (cdr r) xl-jit-rank-savehist-cap)))
            (should-not (assq 'forward-word (plist-get info :roots)))))
      (delete-directory dir t))))

(ert-deftest xl-jit-trusted-file-refuses-symlink-out-of-the-roots ()
  "A symlink inside our lisp/ that points outside is not trusted."
  (let* ((root (make-temp-file "xl-jit-dot-" t))
         (outside (make-temp-file "xl-jit-out-" t))
         (lisp (expand-file-name "emacs/lisp" root))
         (xl-dotfiles (file-name-as-directory root)))
    (unwind-protect
        (progn
          (make-directory lisp t)
          (with-temp-file (expand-file-name "inside.el" lisp) (insert ";;; inside\n"))
          (with-temp-file (expand-file-name "outside.el" outside) (insert ";;; outside\n"))
          (make-symbolic-link (expand-file-name "outside.el" outside)
                              (expand-file-name "link.el" lisp))
          (should (xl-jit-trusted-file-p (expand-file-name "inside.el" lisp)))
          (should-not (xl-jit-trusted-file-p (expand-file-name "link.el" lisp)))
          (should-not (xl-jit-trusted-file-p (expand-file-name "outside.el" outside))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest xl-jit-rank-graph-never-compiles-interpreted-code ()
  "The report must have no side effects: an interpreted function stays interpreted."
  (let* ((name (intern "xl-jit-rank-test-interpreted"))
         (src (format "(defun %s (x) (+ x 1))" name)))
    (unwind-protect
        (progn
          ;; Put a defun in load-history, interpreted, under a fake file name.
          (let ((load-file-name "/tmp/xl-jit-rank-test-fake.el"))
            (eval (read src) t))
          (push (cons "/tmp/xl-jit-rank-test-fake.el"
                      (list (cons 'defun name)))
                load-history)
          (xl-jit-rank--graph user-emacs-directory)
          (should (interpreted-function-p (symbol-function name))))
      (setq load-history (seq-remove (lambda (e) (equal (car e) "/tmp/xl-jit-rank-test-fake.el"))
                                     load-history))
      (fmakunbound name))))

;;; Step 6: background compile, under stubs for the child process

(defmacro xl-jit-test--with-tier2 (&rest body)
  "Temporary state folder, and a temporary dotfiles tree with our lisp/."
  (declare (indent 0))
  `(xl-jit-test--with-state
     (let* ((root (make-temp-file "xl-jit-tier2-" t))
            (xl-dotfiles (file-name-as-directory root))
            (xl-jit-crash-dir (expand-file-name "crashes/" root))
            (lisp (expand-file-name "emacs/lisp" root)))
       (make-directory lisp t)
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-directory root t))))))

(defun xl-jit-test--row (path ratio)
  (list :file (file-name-sans-extension (file-name-nondirectory path))
        :path path :ratio ratio :score 1.0 :size 1000 :top nil :trusted t))

(defun xl-jit-test--src (dir name)
  (let ((src (expand-file-name (concat name ".el") dir)))
    (with-temp-file src (insert (format ";;; %s.el\n(defun %s-fn () 1)\n" name name)))
    src))

(ert-deftest xl-jit-timeout-kills-a-child-that-runs-too-long ()
  "A child that outlives its time is killed and reported; a quick one is ok."
  (skip-unless (file-executable-p (expand-file-name invocation-name invocation-directory)))
  (should (eq (xl-jit--run-with-timeout '("-Q" "--batch" "--eval" "(princ 1)") 60) 'ok))
  (let ((r (xl-jit--run-with-timeout '("-Q" "--batch" "--eval" "(sleep-for 30)") 1)))
    (should (stringp r))
    (should (string-match-p "timed out" r))))

(ert-deftest xl-jit-candidate-refuses-untrusted-optout-big-and-low-ranked ()
  "Each rule that keeps a file out of the compile list is enforced here."
  (xl-jit-test--with-tier2
    (let* ((good (xl-jit-test--src lisp "good"))
           (outside (make-temp-file "xl-jit-out-" t))
           (outside-src (xl-jit-test--src outside "escaped"))
           (link (expand-file-name "link.el" lisp))
           (optout (xl-jit-test--src lisp "optout"))
           (big (expand-file-name "big.el" lisp)))
      (make-symbolic-link outside-src link)
      (with-temp-file optout (insert ";;; optout.el\n;; Local Variables:\n;; no-native-compile: t\n;; End:\n"))
      (with-temp-file big (insert (make-string (+ 10 xl-jit-max-source-bytes) ?x)))
      (should (xl-jit--candidate-p (xl-jit-test--row good 100.0)))
      (should-not (xl-jit--candidate-p (xl-jit-test--row link 100.0)))
      (should-not (xl-jit--candidate-p (xl-jit-test--row optout 100.0)))
      (should-not (xl-jit--candidate-p (xl-jit-test--row big 100.0)))
      (should-not (xl-jit--candidate-p (xl-jit-test--row good 1.0)))
      (delete-directory outside t))))

(ert-deftest xl-jit-work-compiles_budget_then_nothing_on_the_second_run ()
  "Within the budget, verified elns are recorded; a second run compiles none."
  (xl-jit-test--with-tier2
    (let* ((srcs (mapcar (lambda (n) (xl-jit-test--src lisp n)) '("a1" "a2" "a3" "a4")))
           (rows (mapcar (lambda (f) (xl-jit-test--row f 100.0)) srcs))
           (children 0))
      (cl-letf (((symbol-function 'xl-jit-rank--compute)
                 (lambda (_) (list :rows rows)))
                ((symbol-function 'xl-jit--child-compile)
                 (lambda (src eln)
                   (cl-incf children)
                   (make-directory (file-name-directory eln) t)
                   (with-temp-file eln (insert "fake eln"))
                   'ok))
                ((symbol-function 'xl-jit-verify) (lambda (_src _eln &optional _names) nil)))
        (let ((first (xl-jit--work)))
          (should (= (plist-get first :compiled) 3))
          (should (= children 3)))
        (let ((second (xl-jit--work)))
          (should (= (plist-get second :compiled) 0))
          (should (= children 3)))))))

(ert-deftest xl-jit-timeout-is-not-retried-within-the-day ()
  "A compile that times out is counted failed, and is not attempted again soon."
  (xl-jit-test--with-tier2
    (let* ((src (xl-jit-test--src lisp "slow"))
           (rows (list (xl-jit-test--row src 100.0)))
           (children 0))
      (cl-letf (((symbol-function 'xl-jit-rank--compute) (lambda (_) (list :rows rows)))
                ((symbol-function 'xl-jit--child-compile)
                 (lambda (_ _) (cl-incf children) "timed out after 180 s")))
        (should (= (plist-get (xl-jit--work) :failed) 1))
        (should (= (plist-get (xl-jit--work) :compiled) 0))
        (should (= children 1))))))

(ert-deftest xl-jit-crash-report-quarantines-the-named-eln ()
  "An eln named in a fresh crash report is deleted and its key quarantined."
  (xl-jit-test--with-tier2
    (let* ((eln (expand-file-name "crashy-abc-def.eln" xl-jit-state-dir))
           (key '("/src/crashy.el" "sha" (1)))
           (report-dir xl-jit-crash-dir))
      (make-directory xl-jit-state-dir t)
      (with-temp-file eln (insert "eln"))
      (xl-jit--ledger-set key (list :status 'done :eln eln :sha "s" :size 3))
      (make-directory report-dir t)
      (with-temp-file (expand-file-name "Emacs-2026-11-24.ips" report-dir)
        (insert "{\"threads\": [{\"frames\": [\"crashy-abc-def.eln\"]}]}"))
      (should (= (xl-jit--crash-scan 0) 1))
      (should-not (file-exists-p eln))
      (should (eq (plist-get (xl-jit--ledger-get key) :status) 'quarantined))
      (should-not (xl-jit--may-attempt-p key (+ (xl-jit--now) (* 10 xl-jit-retry-interval)))))))

(ert-deftest xl-jit-total-size-cap-stops_compiling ()
  "Past the total eln size cap, a run compiles nothing and says so."
  (xl-jit-test--with-tier2
    (let* ((src (xl-jit-test--src lisp "capped"))
           (children 0))
      (xl-jit--ledger-set '("/src/old.el" "sha" (1))
                          (list :status 'done :eln "/x" :sha "s" :size 100))
      (cl-letf (((symbol-function 'xl-jit-rank--compute)
                 (lambda (_) (list :rows (list (xl-jit-test--row src 100.0)))))
                ((symbol-function 'xl-jit--child-compile)
                 (lambda (_ _) (cl-incf children) 'ok)))
        (let ((xl-jit-max-total-bytes 50))
          (let ((r (xl-jit--work)))
            (should (plist-get r :capped))
            (should (= children 0))))))))

;;; The in-Emacs trigger (xl-jit-auto.el): timers only, and never in batch

(defmacro xl-jit-test--with-auto (&rest body)
  "A temporary profile folder, an Emacs that has been open for an hour, on AC
power, native compilation available, and timers recorded instead of armed."
  (declare (indent 0))
  `(let* ((user-emacs-directory (file-name-as-directory (make-temp-file "xl-jit-auto-" t)))
          (before-init-time (time-subtract nil 3600))
          (xl-jit-auto-enable t)
          (xl-jit-auto--process nil)
          (xl-jit-auto--timer nil)
          (timers nil))
     (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
               ((symbol-function 'xl-jit-auto--on-battery-p) (lambda () nil))
               ((symbol-function 'run-with-timer)
                (lambda (secs _repeat fn) (push (list 'timer secs fn) timers) 'fake-timer))
               ((symbol-function 'run-with-idle-timer)
                (lambda (secs _repeat fn) (push (list 'idle secs fn) timers) 'fake-idle))
               ((symbol-function 'xl-jit-auto--cancel)
                (lambda () (setq xl-jit-auto--timer nil))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-directory user-emacs-directory t))))))

(defun xl-jit-test--touch-last-run (age)
  "Write a last-run record that is AGE seconds old."
  (let ((f (xl-jit-auto--last-run-file)))
    (make-directory (file-name-directory f) t)
    (with-temp-file f (insert "(:time 0)\n"))
    (set-file-times f (time-subtract nil age))))

(ert-deftest xl-jit-auto-decides-when-to-run ()
  "Each reason not to run is reported; with none left, it may run."
  (xl-jit-test--with-auto
    (should (null (xl-jit-auto--decision)))
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (should (eq (xl-jit-auto--decision) 'unavailable)))
    (let ((xl-jit-auto-enable nil))
      (should (eq (xl-jit-auto--decision) 'disabled)))
    (let ((process-environment (cons "XL_JIT=0" process-environment)))
      (should (eq (xl-jit-auto--decision) 'disabled)))
    (let ((before-init-time (time-subtract nil 30)))
      (should (eq (xl-jit-auto--decision) 'too-early)))
    (cl-letf (((symbol-function 'xl-jit-auto--on-battery-p) (lambda () t)))
      (should (eq (xl-jit-auto--decision) 'battery)))
    (xl-jit-test--touch-last-run 3600)
    (should (eq (xl-jit-auto--decision) 'recent))
    (xl-jit-test--touch-last-run (+ xl-jit-auto-interval 60))
    (should (null (xl-jit-auto--decision)))))

(ert-deftest xl-jit-auto-tick-waits-for-idle-or-checks-again-later ()
  "Allowed: an idle timer.  Too early: a timer for the rest.  Unavailable: none."
  (xl-jit-test--with-auto
    (xl-jit-auto--tick)
    (should (equal (car timers) (list 'idle xl-jit-auto-idle #'xl-jit-auto--start)))
    (setq timers nil)
    (let ((before-init-time (time-subtract nil 60)))
      (xl-jit-auto--tick)
      (should (eq (car (car timers)) 'timer))
      (should (<= (abs (- (nth 1 (car timers)) (- xl-jit-auto-uptime 60))) 2)))
    (setq timers nil)
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (xl-jit-auto--tick)
      (should (null timers)))))

(ert-deftest xl-jit-auto-never-arms-in-batch ()
  "Setup, tests, and the background run itself load init in batch: no timer."
  (xl-jit-test--with-auto
    (let ((noninteractive t))
      (xl-jit-auto-arm)
      (should (null timers)))
    (let ((noninteractive nil))
      (xl-jit-auto-arm)
      (should (eq (car (car timers)) 'timer)))))

(ert-deftest xl-jit-auto-start-rechecks-and-spawns-nothing-when-not-allowed ()
  "If the user went on battery while idle, no process starts."
  (xl-jit-test--with-auto
    (let ((spawned 0))
      (cl-letf (((symbol-function 'xl-jit-auto--on-battery-p) (lambda () t))
                ((symbol-function 'make-process) (lambda (&rest _) (cl-incf spawned) nil)))
        (xl-jit-auto--start)
        (should (= spawned 0))
        (should (equal (car timers) (list 'timer xl-jit-auto-retry #'xl-jit-auto--tick)))))))

(ert-deftest xl-jit-auto-command-runs-at-background-priority ()
  "The background run uses taskpolicy -b when present, nice -n 19 otherwise."
  (xl-jit-test--with-auto
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (and (equal p "taskpolicy") "/usr/sbin/taskpolicy"))))
      (should (equal (seq-take (xl-jit-auto--command) 2) '("taskpolicy" "-b"))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (and (equal p "nice") "/usr/bin/nice"))))
      (should (equal (seq-take (xl-jit-auto--command) 3) '("nice" "-n" "19"))))
    (should (member "--batch" (xl-jit-auto--command)))))

;;; Query: which functions are native

(ert-deftest xl-jit-native-list-shows-a-native-toy ()
  "A function that is native in this Emacs shows up in the native list."
  (skip-unless (native-comp-available-p))
  (xl-jit-test--with-state
    (let* ((dir (make-temp-file "xl-jit-native-toy-" t))
           (src (xl-jit-test--toy-src dir "native-toy"))
           (eln (expand-file-name "native-toy.eln" dir)))
      (unwind-protect
          (progn
            (native-compile src eln)
            (load eln nil t)
            (let* ((info (xl-jit-native-list))
                   (names (apply #'append (mapcar #'cdr (plist-get info :files)))))
              (should (memq 'native-toy-fn names))
              (should (> (plist-get info :total) 0))
              (should (stringp (with-output-to-string
                                 (let ((noninteractive t))
                                   (xl-jit-native-report)))))))
        (ignore-errors (fmakunbound 'native-toy-fn))
        (ignore-errors (delete-directory dir t))))))

(provide 'xl-jit-tests)
;;; xl-jit-tests.el ends here
