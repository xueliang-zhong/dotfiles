;;; eln-opt-tests.el --- ERT tests for eln-opt -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/eln-opt-tests.el \
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

(defun eln-opt-test--worker-result ()
  "Run the worker quietly and return its result."
  (let (result)
    (with-output-to-string (setq result (eln-opt-worker)))
    result))

(defmacro eln-opt-test--count-canary (counter &rest body)
  "Run BODY with `eln-opt--canary' counting its calls into COUNTER."
  (declare (indent 1))
  `(let ((orig (symbol-function 'eln-opt--canary)))
     (cl-letf (((symbol-function 'eln-opt--canary)
                (lambda () (cl-incf ,counter) (funcall orig))))
       ,@body)))

(ert-deftest eln-opt-unavailable-only-byte-compiles ()
  "Without native compilation the worker plans no native compile and never
runs the canary; it still byte-compiles."
  (eln-opt-test--with-state
    (let ((canary-calls 0) (native-asked nil))
      (eln-opt-test--count-canary canary-calls
        (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
                  ((symbol-function 'eln-opt--byte-candidates) (lambda () nil))
                  ((symbol-function 'eln-opt-rank--compute)
                   (lambda (&rest _) (setq native-asked t) nil)))
          (should (eql (eln-opt-test--worker-result) 0))
          (should (eql (eln-opt-test--worker-result) 0))))
      (should (= canary-calls 0))
      (should-not native-asked)
      (should (file-exists-p (eln-opt--platform-file))))))

(ert-deftest eln-opt-canary-failure-trips-breaker ()
  "A canary that fails is cached as unavailable and not retried."
  (eln-opt-test--with-state
    (let ((compile-calls 0))
      (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
                ((symbol-function 'native-compile)
                 (lambda (&rest _) (cl-incf compile-calls) (error "libgccjit missing"))))
        (should-not (eln-opt-platform-available-p))
        (cl-letf (((symbol-function 'eln-opt--byte-candidates) (lambda () nil)))
          (should (eql (eln-opt-test--worker-result) 0)))
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

;;; Step 5: the ranking report (no compiling)

(ert-deftest eln-opt-rank-file-key-drops-autoloads ()
  "Autoload stubs are not a place to compile; other names lose .el/.elc/.gz."
  (should (null (eln-opt-rank--file-key "/x/cape-autoloads.el")))
  (should (null (eln-opt-rank--file-key "/x/evil-autoloads.elc")))
  (should (equal (eln-opt-rank--file-key "/x/winum.elc") "winum"))
  (should (equal (eln-opt-rank--file-key "/x/org/org.el.gz") "org"))
  (should (null (eln-opt-rank--file-key "/x/org/org-loaddefs.elc")))
  (should (null (eln-opt-rank--file-key "/x/org/org-loaddefs.el.gz")))
  (should (null (eln-opt-rank--file-key "/x/loaddefs.elc")))
  (should (null (eln-opt-rank--file-key "/x/loaddefs.el.gz"))))

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
         (lisp (expand-file-name "tuned-emacs/lisp" root))
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
            (lisp (expand-file-name "tuned-emacs/lisp" root)))
       (make-directory lisp t)
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-directory root t))))))

(defun eln-opt-test--row (path ratio)
  (list :file (file-name-sans-extension (file-name-nondirectory path))
        :path path :score ratio :gain 1.0 :size 1000 :top nil :trusted t))

(defun eln-opt-test--src (dir name)
  (let ((src (expand-file-name (concat name ".el") dir)))
    (with-temp-file src (insert (format ";;; %s.el\n(defun %s-fn () 1)\n" name name)))
    src))

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
                                   (eln-opt-report)))))))
        (ignore-errors (fmakunbound 'native-toy-fn))
        (ignore-errors (delete-directory dir t))))))

(ert-deftest eln-opt-report-says-when-this-emacs-cannot-compile ()
  "Without native compilation the report is the same report, with the reason in
its header, the Status line, the byte-compile list, and no native list."
  (eln-opt-test--with-state
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
              ((symbol-function 'eln-opt--byte-candidates) (lambda () '("/x/a.el"))))
      (let ((text (with-output-to-string
                    (let ((noninteractive t))
                      (eln-opt-report)))))
        (should (string-match-p "Native compilation is not available in this Emacs" text))
        (should (string-match-p "byte-compiles first .*:\n  1 file(s): a.el" text))
        (should (string-match-p "Status:\n  0 file(s) at Tier 2" text))
        (should-not (string-match-p "native-compiles" text))
        (should (string-match-p "Not listed: without native compilation" text))))))

;;; Byte-compiling what runs interpreted

(ert-deftest eln-opt-byte-candidates-are-trusted-interpreted-files ()
  "Candidates: loaded from .el with functions, under the roots, no current
.elc; not autoloads, init files, opted-out files or past failures.  Most
functions first."
  (eln-opt-test--with-state
    (let* ((dir (file-name-as-directory (make-temp-file "eln-opt-byte-" t)))
           (out (file-name-as-directory (make-temp-file "eln-opt-out-" t)))
           (f (lambda (d name &optional text)
                (let ((file (expand-file-name name d)))
                  (write-region (or text ";; -*- lexical-binding: t; -*-\n") nil file)
                  file)))
           (a (funcall f dir "a.el"))
           (stale (funcall f dir "stale.el"))
           (current (funcall f dir "current.el"))
           (opted (funcall f dir "opted.el" ";; -*- no-byte-compile: t; -*-\n"))
           (autoloads (funcall f dir "x-autoloads.el"))
           (init (funcall f dir "init.el"))
           (data (funcall f dir "data.el"))
           (outside (funcall f out "outside.el"))
           (failed (funcall f dir "failed.el"))
           (defuns (lambda (n) (cl-loop for i below n collect (cons 'defun (intern (format "f%d" i))))))
           (load-history
            (list (cons a (funcall defuns 1)) (cons stale (funcall defuns 3))
                  (cons current (funcall defuns 1)) (cons opted (funcall defuns 1))
                  (cons autoloads (funcall defuns 1)) (cons init (funcall defuns 1))
                  (cons data '((defvar . v))) (cons outside (funcall defuns 1))
                  (cons failed (funcall defuns 1)))))
      (unwind-protect
          (progn
            (write-region "" nil (concat stale "c"))
            (set-file-times (concat stale "c") (time-subtract nil 100))
            (write-region "" nil (concat current "c"))
            (set-file-times (concat current "c") (time-add nil 100))
            (eln-opt--record-failure failed "boom")
            (cl-letf (((symbol-function 'eln-opt-trusted-roots) (lambda () (list dir))))
              (should (equal (eln-opt--byte-candidates) (list stale a)))))
        (delete-directory dir t)
        (delete-directory out t)))))

(ert-deftest eln-opt-worker-byte-compiles-and-stops-at-a-broken-file ()
  "Byte items compile to a current .elc; a file that does not compile goes on
the failed list, and the worker goes on with the next file."
  (eln-opt-test--with-state
    (let* ((dir (make-temp-file "eln-opt-bw-" t))
           (good (eln-opt-test--toy-src dir "bw-good"))
           (bad (expand-file-name "bw-bad.el" dir))
           (after (eln-opt-test--toy-src dir "bw-after")))
      (write-region ";; -*- lexical-binding: t; -*-\n(defun bw-bad-fn (\n" nil bad)
      (unwind-protect
          (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () nil))
                    ((symbol-function 'eln-opt-worker-plan)
                     (lambda (&rest _) (list (cons 'byte good) (cons 'byte bad) (cons 'byte after))))
                    ((symbol-function 'sleep-for) #'ignore))
            (let ((byte-compile-log-warning-function #'ignore))
              (should (eql (eln-opt-test--worker-result) 2)))
            (should (eln-opt--elc-current-p good))
            (should-not (file-exists-p (concat bad "c")))
            (should (eln-opt--elc-current-p after))
            (should (eln-opt--failed-p bad)))
        (delete-directory dir t)))))

;; The report and the worker must see the same byte candidates: the lazy
;; roots are loaded first, or files behind a lazy package are missed.
(ert-deftest eln-opt-byte-plan-loads-lazy-roots-before-candidates ()
  "Both the worker and the report take byte candidates after the lazy roots load."
  (let ((order nil))
    (cl-letf (((symbol-function 'eln-opt-rank--roots)
               (lambda (&rest _) (list :roots '((lazy-pkg . 1.0)))))
              ((symbol-function 'eln-opt-rank--load-lazy-roots)
               (lambda (roots) (push (cons 'load roots) order) nil))
              ((symbol-function 'eln-opt--byte-candidates)
               (lambda () (push 'candidates order) nil)))
      (should (null (eln-opt--byte-plan)))
      (should (equal (reverse order) '((load (lazy-pkg . 1.0)) candidates))))))

;; The table has one row per file at Tier 1 or 2, with an elc and an eln column.
(ert-deftest eln-opt-report-tier-table-shows-elc-and-eln ()
  "Native files show eln yes; byte-compiled-only files show eln no."
  (let* ((rows (list (list :file "zeta" :elc t :eln nil :size 2048 :count 3 :top '(z-a))
                     (list :file "alpha" :elc t :eln t :size 1024 :count 5 :top '(a-b))))
         (text (eln-opt-rank--format (list :rows nil :built-rows rows :next nil :failed nil
                                      :byte nil :excluded nil :native-skipped 0
                                      :interpreted-skipped 0 :functions 0
                                      :savehist-share 0.0 :roots 0))))
    (should (string-match-p "file +KB +elc +eln +functions +examples" text))
    (should (string-match-p "alpha +1.0 +yes +yes +5" text))
    (should (string-match-p "zeta +2.0 +yes +no +3" text))))

;; The report buffer gets faces; the batch text does not.
(ert-deftest eln-opt-report-buffer-is-colored-batch-is-not ()
  "yes is green and no dimmed in the table; the title is bold; the text itself
is unchanged."
  (let* ((plain "eln-opt report (report only)\n\nStatus: 1 file(s) at Tier 2 (native, 5 function(s)); 0 file(s) at Tier 1 (byte-compiled)\n\nfile                 KB  elc   eln   functions  examples\nzeta                2.0  yes   no            3  (z-a)\nalpha               1.0  yes   yes           5  (a-b)\n\nLeft out before ranking:\n")
         (out (eln-opt-rank--colorize plain)))
    (should (equal (substring-no-properties out) plain))
    (let ((yes (string-match "yes   yes" out)))
      (should (memq 'success (ensure-list (get-text-property yes 'face out)))))
    (should (memq 'shadow (ensure-list (get-text-property (string-match "no  " out) 'face out))))
    (should (memq 'bold (ensure-list (get-text-property 0 'face out))))
    (should (memq 'bold (ensure-list (get-text-property (string-match "Left out" out) 'face out))))
    ;; The left-out counts and file lists in their own section.
    (let* ((left "Left out before ranking:\n  opted out with no-native-compile: 2 file(s): package, cus-load\n")
           (lout (eln-opt-rank--colorize left)))
      (should (memq 'font-lock-type-face
                    (ensure-list (get-text-property (string-match "cus-load" lout) 'face lout))))
      (should (memq 'font-lock-constant-face
                    (ensure-list (get-text-property (string-match "2 file" lout) 'face lout)))))
    ;; File names and function names get their faces too.
    (should (memq 'font-lock-type-face
                  (ensure-list (get-text-property (string-match "zeta" out) 'face out))))
    (should (memq 'font-lock-function-name-face
                  (ensure-list (get-text-property (string-match "z-a" out) 'face out))))))

;; Reset asks first when run by hand; the batch command does not ask.
(ert-deftest eln-opt-reset-asks-before-deleting-when-interactive ()
  "Interactive reset deletes nothing unless the user types yes."
  (eln-opt-test--with-state
    (let ((deleted nil) (asked nil) (noninteractive nil))
      (cl-letf (((symbol-function 'eln-opt--user-elns) (lambda () '("/x/a.eln" "/x/b.eln")))
                ((symbol-function 'comp-delete-or-replace-file)
                 (lambda (f) (push f deleted)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (setq asked prompt) nil)))
        (eln-opt--write-eld (eln-opt--failed-file) '(:x 1))
        (should (null (eln-opt-reset)))
        (should (null deleted))
        (should (string-match-p "Delete 2 eln" asked))
        (should (file-exists-p (eln-opt--failed-file)))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (should (= 2 (eln-opt-reset))))
        (should (equal (sort deleted #'string<) '("/x/a.eln" "/x/b.eln")))
        (should-not (file-exists-p (eln-opt--failed-file)))))))

(ert-deftest eln-opt-reset-batch-does-not-ask ()
  "The batch command resets without a prompt."
  (eln-opt-test--with-state
    (let ((asked nil) (deleted nil))
      (cl-letf (((symbol-function 'eln-opt--user-elns) (lambda () '("/x/c.eln")))
                ((symbol-function 'comp-delete-or-replace-file)
                 (lambda (f) (push f deleted)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_) (setq asked t) nil)))
        (let ((noninteractive t))
          (should (= 1 (eln-opt-reset))))
        (should-not asked)
        (should (equal deleted '("/x/c.eln")))))))

;;; In-progress marker: an interrupted compile is retried, then given up

(ert-deftest eln-opt-marker-is-written-during-a-compile-and-removed-after ()
  "The marker names the file while it compiles, and is gone when it finishes."
  (eln-opt-test--with-worker srcs '("m1")
    (let ((seen nil))
      (cl-letf (((symbol-function 'native-compile)
                 (lambda (&rest _)
                   (setq seen (plist-get (eln-opt--read-eld (eln-opt--marker-file)) :src))))
                ((symbol-function 'eln-opt--built-p) (lambda (_) t)))
        (should (= 1 (eln-opt-test--worker-result))))
      (should (equal seen (car srcs)))
      (should-not (file-exists-p (eln-opt--marker-file))))))

(ert-deftest eln-opt-interrupted-once-is-retried-then-interrupted-twice-fails ()
  "A marker with one attempt is retried; with the limit reached it goes on the
failed list, and the file is not compiled."
  (eln-opt-test--with-worker srcs '("m2")
    (let ((src (car srcs)) (compiled 0))
      (eln-opt--write-eld (eln-opt--marker-file) (list :src src :attempts 1 :time 0))
      (cl-letf (((symbol-function 'native-compile) (lambda (&rest _) (cl-incf compiled)))
                ((symbol-function 'eln-opt--built-p) (lambda (_) t)))
        (should (equal (eln-opt--settle-interrupted) (cons src 1))))
      (eln-opt--write-eld (eln-opt--marker-file) (list :src src :attempts 2 :time 0))
      (cl-letf (((symbol-function 'native-compile) (lambda (&rest _) (cl-incf compiled)))
                ((symbol-function 'eln-opt--built-p) (lambda (_) t)))
        (should-not (eln-opt--settle-interrupted))
        (should (eln-opt--failed-p src))
        (should-not (file-exists-p (eln-opt--marker-file)))
        (should (= 0 compiled))))))

(ert-deftest eln-opt-marker-with-an-untrusted-path-is-dropped-not-used ()
  "A marker that names a file outside the trusted roots is removed, and nothing
is recorded for that path."
  (eln-opt-test--with-state
    (let* ((dir (make-temp-file "eln-opt-outside-" t))
           (outside (eln-opt-test--src dir "outside")))
      (unwind-protect
          (progn
            (eln-opt--write-eld (eln-opt--marker-file) (list :src outside :attempts 2 :time 0))
            (cl-letf (((symbol-function 'eln-opt-trusted-roots) (lambda () nil)))
              (should-not (eln-opt--settle-interrupted)))
            (should-not (file-exists-p (eln-opt--marker-file)))
            (should-not (eln-opt--failed-p outside)))
        (delete-directory dir t)))))

(ert-deftest eln-opt-worker-does-not-compile-when-the-marker-cannot-be-written ()
  "If the marker cannot be written, the file is not compiled and the worker stops."
  (eln-opt-test--with-worker srcs '("m3" "m4")
    (let ((compiled 0))
      (cl-letf (((symbol-function 'native-compile) (lambda (&rest _) (cl-incf compiled)))
                ((symbol-function 'eln-opt--write-eld)
                 (let ((real (symbol-function 'eln-opt--write-eld)))
                   (lambda (file form)
                     (if (equal file (eln-opt--marker-file))
                         (error "disk full")
                       (funcall real file form))))))
        (should (= 0 (eln-opt-test--worker-result))))
      (should (= 0 compiled))
      (should-not (eln-opt--failed-p (car srcs))))))

(ert-deftest eln-opt-marker-behind-a-symlink-is-refused-and-its-target-untouched ()
  "A symlinked marker is never read or written through; the worker does not compile."
  (eln-opt-test--with-worker srcs '("m5")
    (let* ((target (expand-file-name "target.txt" eln-opt-state-dir))
           (compiled 0))
      (with-temp-file target (insert "keep me"))
      (make-directory (eln-opt--version-dir) t)
      (make-symbolic-link target (eln-opt--marker-file))
      (cl-letf (((symbol-function 'native-compile) (lambda (&rest _) (cl-incf compiled)))
                ((symbol-function 'eln-opt--built-p) (lambda (_) t)))
        (should (= 0 (eln-opt-test--worker-result))))
      (should (= 0 compiled))
      (should (equal (with-temp-buffer (insert-file-contents target) (buffer-string))
                     "keep me")))))

(ert-deftest eln-opt-reset-forgets-the-marker ()
  "Reset deletes the marker with the failed list."
  (eln-opt-test--with-state
    (eln-opt--write-eld (eln-opt--marker-file) (list :src "/x/a.el" :attempts 1 :time 0))
    (let ((noninteractive t) (eln-opt--user-elns-stub nil))
      (cl-letf (((symbol-function 'eln-opt--user-elns) (lambda () nil)))
        (eln-opt-reset)))
    (should-not (file-exists-p (eln-opt--marker-file)))))

;; The one-pass file map must give what `symbol-file' gives for the native
;; functions, or the native list names the wrong files.
(ert-deftest eln-opt-defun-files-agrees-with-symbol-file ()
  "For every native function, the map and `symbol-file' name the same file."
  (eln-opt-test--with-tier2
    (let ((src (eln-opt-test--src lisp "mapped1")))
      (load (file-name-sans-extension src) nil t)
      (let ((map (eln-opt--defun-files)) (bad 0) (checked 0))
        (should (equal (gethash 'mapped1-fn map) (symbol-file 'mapped1-fn 'defun)))
        (mapatoms (lambda (s)
                    (when (and (fboundp s) (not (macrop s))
                               (ignore-errors (native-comp-function-p (indirect-function s))))
                      (cl-incf checked)
                      (unless (equal (gethash s map) (symbol-file s 'defun))
                        (cl-incf bad)))))
        (should (> checked 100))
        (should (= bad 0))))))

;; Taking over a stale lock must not leave lock.stale-* folders behind, and
;; folders left by earlier takeovers are swept once the lock is ours.
(ert-deftest eln-opt-lock-takeover-leaves-no-stale-folders ()
  "A stale lock is taken over, and no lock.stale-* folder remains afterwards."
  (eln-opt-test--with-state
    (let ((dir (eln-opt--lock-dir)))
      (make-directory (eln-opt--version-dir) t)
      (dolist (old '("lock.stale-old1" "lock.stale-old2"))
        (make-directory (expand-file-name old (eln-opt--version-dir)) t))
      (make-directory dir)
      (eln-opt--write-eld (eln-opt--owner-file dir) (list :pid 999999999 :time 0))
      (should (eln-opt--lock-acquire))
      (should (equal (directory-files (eln-opt--version-dir) nil "\\`lock\\.stale-")
                     nil))
      (eln-opt--lock-release))))

;; A zombie owner (exited, not yet reaped) must not keep the lock alive.
(ert-deftest eln-opt-zombie-owner-does-not-hold-the-lock ()
  "A lock whose owner is a zombie is stale, and can be taken over."
  (eln-opt-test--with-state
    (let ((dir (eln-opt--lock-dir)))
      (make-directory dir t)
      (eln-opt--write-eld (eln-opt--owner-file dir) (list :pid 4242 :time (eln-opt--now)))
      (cl-letf (((symbol-function 'process-attributes)
                 (lambda (pid) (and (= pid 4242) '((state . "Z"))))))
        (should-not (eln-opt--process-alive-p 4242))
        (should (eln-opt--lock-stale-p dir))
        (should (eln-opt--lock-acquire)))
      (eln-opt--lock-release))))

;; Stopping the worker ends its process and its watchdog, and arms nothing.
(ert-deftest eln-opt-stop-worker-ends-the-run-and-arms-nothing ()
  "A running worker is stopped, its timers are cancelled, and the stop says so."
  (eln-opt-test--with-auto
    (let* ((proc (xl-async-start (list "sleep" "100") :name "eln-opt-test-stop")))
      (setq eln-opt-auto--process proc
            eln-opt-auto--watchdog (run-with-timer 60 60 #'ignore)
            eln-opt-auto--timer (run-with-idle-timer 5 nil #'ignore))
      (eln-opt-stop-worker)
      (should-not (process-live-p proc))
      (should-not eln-opt-auto--process)
      (should-not eln-opt-auto--watchdog)
      (should-not eln-opt-auto--timer)
      (let (msg)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (eln-opt-stop-worker))
        (should (string-match-p "no worker is running" msg))))))

;; Repeated report and compile calls must not start one analysis after another.
(ert-deftest eln-opt-report-reuses-a-fresh-cache-and-a-running-child ()
  "A recent report that still matches is shown without a child; a running
child is not restarted."
  (eln-opt-test--with-state
    (let ((spawned 0) (noninteractive nil))
      (cl-letf (((symbol-function 'xl-async-start)
                 (lambda (&rest _) (cl-incf spawned) (start-process "eln-opt-test-report" nil "sleep" "0")))
                ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (setq eln-opt-rank--cache
              (list (float-time) (eln-opt--quiet-fingerprint) "CACHED REPORT"))
        (eln-opt-report)
        (should (= spawned 0))
        (with-current-buffer "*eln-opt report*"
          (should (string-match-p "CACHED REPORT" (buffer-string))))
        ;; A stale cache is not used.
        (setq eln-opt-rank--cache (list 0 (eln-opt--quiet-fingerprint) "OLD"))
        (cl-letf (((symbol-function 'xl-async-busy-p) (lambda (&rest _) nil)))
          (eln-opt-report))
        (should (= spawned 1))
        ;; A child that is already running is shown, not restarted.
        (cl-letf (((symbol-function 'xl-async-busy-p) (lambda (&rest _) t)))
          (eln-opt-report))
        (should (= spawned 1))
        (kill-buffer "*eln-opt report*")))))

(ert-deftest eln-opt-compile-now-waits-after-a-worker-ends ()
  "A manual start is refused for `eln-opt-manual-gap' seconds after a worker ended."
  (eln-opt-test--with-auto
    (let ((started 0) (msg nil) (noninteractive nil))
      (cl-letf (((symbol-function 'eln-opt-auto--start)
                 (lambda (&rest _) (cl-incf started) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (setq eln-opt-auto--last-end (float-time))
        (eln-opt-compile-now)
        (should (= started 0))
        (should (string-match-p "try again in" msg))
        (setq eln-opt-auto--last-end (- (float-time) (* 2 eln-opt-manual-gap)))
        (eln-opt-compile-now)
        (should (= started 1))))))

;; macOS checks each new native file on its first load; the worker pays that.
(ert-deftest eln-opt-warm-loads-new-files-in-a-throwaway-emacs ()
  "On macOS, existing files go to one `emacs -Q' child with no load path;
elsewhere, or with no existing file, no child starts."
  (let* ((f (make-temp-file "eln-opt-warm" nil ".eln"))
         (real (symbol-function 'make-process))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (push (plist-get args :command) calls)
                     (funcall real :name "eln-opt-test-true" :command '("true")))))
          (let ((system-type 'gnu/linux))
            (should (= (eln-opt--warm (list f)) 0)))
          (should-not calls)
          (let ((system-type 'darwin))
            (should (= (eln-opt--warm (list "/no/such/file.eln" nil)) 0))
            (should-not calls)
            (should (= (eln-opt--warm (list f "/no/such/file.eln")) 1)))
          (should (= (length calls) 1))
          (let ((cmd (car calls)))
            (should (member "-Q" cmd))
            (should (member "--batch" cmd))
            (should (string-match-p "load-path nil" (car (last cmd))))
            (should (string-match-p (regexp-quote f) (car (last cmd))))
            (should-not (string-match-p "no/such" (car (last cmd))))))
      (delete-file f))))

(ert-deftest eln-opt-worker-warms-what-it-builds-then-the-whole-folder ()
  "Each native build is warmed right away; byte builds are not; the run ends
with one warm-up of every eln in the folder."
  (eln-opt-test--with-worker srcs '("v1" "v2")
    (let ((warmed nil))
      (cl-letf (((symbol-function 'eln-opt-worker-plan)
                 (lambda (&rest _) (list (cons 'byte (nth 0 srcs)) (cons 'native (nth 1 srcs)))))
                ((symbol-function 'byte-compile-file) (lambda (&rest _) t))
                ((symbol-function 'eln-opt--elc-current-p) (lambda (_) t))
                ((symbol-function 'native-compile) #'ignore)
                ((symbol-function 'eln-opt--built-p) (lambda (_) t))
                ((symbol-function 'eln-opt--eln-for) (lambda (src) (concat src ".eln")))
                ((symbol-function 'eln-opt--eln-files) (lambda () '("all-1.eln" "all-2.eln")))
                ((symbol-function 'eln-opt--warm)
                 (lambda (files) (push files warmed) (length files))))
        (let ((out (with-output-to-string (eln-opt-worker))))
          (should (equal (reverse warmed)
                         (list (list (concat (nth 1 srcs) ".eln"))
                               '("all-1.eln" "all-2.eln"))))
          (should (string-match-p "warmed 2 eln file" out)))))))

(ert-deftest eln-opt-worker-pauses-between-files-not-between-the-steps-of-one ()
  "Two steps of the same file are not separated by the pause; a new file is."
  (eln-opt-test--with-worker srcs '("p1" "p2")
    (let ((pauses 0))
      (cl-letf (((symbol-function 'eln-opt-worker-plan)
                 (lambda (&rest _)
                   (list (cons 'byte (nth 0 srcs)) (cons 'native (nth 0 srcs))
                         (cons 'native (nth 1 srcs)))))
                ((symbol-function 'sleep-for) (lambda (&rest _) (cl-incf pauses)))
                ((symbol-function 'byte-compile-file) (lambda (&rest _) t))
                ((symbol-function 'eln-opt--elc-current-p) (lambda (_) t))
                ((symbol-function 'native-compile) #'ignore)
                ((symbol-function 'eln-opt--built-p) (lambda (_) t))
                ((symbol-function 'eln-opt--warm) (lambda (files) (length files))))
        (with-output-to-string (eln-opt-worker))
        (should (= pauses 1))))))

;; The worker's trace: a span per top-level form, named for its defun.
(ert-deftest eln-opt-trace-top-level-names-the-defun-and-costs-nothing-idle ()
  "Outside a recording the advice only calls the original; during one, each
top-level form is a span named for the function it defines."
  (let ((calls nil))
    (should (= 42 (eln-opt--trace-top-level (lambda (_form) 42) '(defun foo () 1))))
    (let ((dir (file-name-as-directory (make-temp-file "eln-opt-trace-test-" t)))
          (xl-perfetto-marker-file (expand-file-name "no-marker.eld" temporary-file-directory)))
      (unwind-protect
          (let ((xl-perfetto-trace-directory dir)
                (xl-perfetto-trace-max-seconds 30))
            (xl-perfetto-trace-start)
            (should (= 7 (eln-opt--trace-top-level
                          (lambda (_form) (push 'called calls) 7)
                          '(defun foo () 1))))
            (xl-perfetto-trace-stop)
            (let ((text (with-temp-buffer
                          (dolist (f (directory-files dir t "\\.json\\'"))
                            (insert-file-contents f))
                          (buffer-string))))
              (should (string-match-p "byte foo" text))
              (should calls)))
        (delete-directory dir t)))))

;;; Explicit compile: one file, now, whatever the failed list and the quiet state say

(defun eln-opt-test--forced (sources)
  "Run the worker on SOURCES quietly and return its result."
  (let (result)
    (with-output-to-string (setq result (eln-opt-worker sources)))
    result))

(ert-deftest eln-opt-forced-compile-ignores-the-failed-list ()
  "A file that failed before is still compiled when the user asks for it."
  (eln-opt-test--with-tier2
    (let ((src (eln-opt-test--src lisp "forced1")))
      (eln-opt--record-failure src "an old failure")
      (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () nil))
                ((symbol-function 'sleep-for) #'ignore))
        (should (= 1 (eln-opt-test--forced (list src)))))
      (should (eln-opt--elc-current-p src)))))

(ert-deftest eln-opt-forced-compile-refuses-an-untrusted-file ()
  "A file outside the trusted folders is not compiled, and the reason is printed."
  (eln-opt-test--with-tier2
    (let* ((outside (file-name-as-directory (make-temp-file "eln-opt-outside-" t)))
           (src (eln-opt-test--src outside "forced2"))
           (text nil))
      (unwind-protect
          (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () nil)))
            (setq text (with-output-to-string
                         (should (= 0 (eln-opt-worker (list src))))))
            (should (string-match-p "refused .*not in a trusted folder" text))
            (should-not (file-exists-p (concat src "c"))))
        (delete-directory outside t)))))

;;; Processors: automatic workers need a second core; a manual run does not

(ert-deftest eln-opt-auto-needs-two-processors-but-manual-runs-do-not ()
  "On one processor the automatic start says so; a forced (manual) run does not."
  (eln-opt-test--with-state
    (eln-opt-test--with-auto
      (cl-letf (((symbol-function 'num-processors) (lambda (&rest _) 1)))
        (should (eq (eln-opt-auto--decision) 'single-core))
        (should (null (eln-opt-auto--decision t))))
      (cl-letf (((symbol-function 'num-processors) (lambda (&rest _) 2)))
        (should (null (eln-opt-auto--decision)))))))

;;; Quiet: a worker that finds nothing stops automatic starts from looking

(ert-deftest eln-opt-quiet-holds-automatic-workers-until-something-changes ()
  "A quiet state holds automatic starts back; a manual run is not held back;
a changed load path lets them start again."
  (eln-opt-test--with-state
    (eln-opt-test--with-auto
      (should-not (eln-opt-auto--decision))
      (eln-opt--mark-quiet)
      (should (eq (eln-opt-auto--decision) 'quiet))
      (should (null (eln-opt-auto--decision t)))
      ;; A package update changes the load path: the quiet state no longer holds.
      (let ((load-path (cons "/tmp/eln-opt-new-package" load-path)))
        (should-not (eln-opt-auto--decision)))
      ;; Nothing changed: still quiet, on every start.
      (should (eq (eln-opt-auto--decision) 'quiet)))))

(ert-deftest eln-opt-worker-marks-quiet-when-nothing_to_do-and-clears-when-busy ()
  "An empty plan records the quiet state; a non-empty plan clears it."
  (eln-opt-test--with-worker srcs '()
    (eln-opt-test--worker-result)
    (should (file-exists-p (eln-opt--quiet-file))))
  (eln-opt-test--with-worker srcs '("q1")
    (eln-opt--mark-quiet)
    (cl-letf (((symbol-function 'native-compile) (lambda (&rest _) (error "boom"))))
      (eln-opt-test--worker-result))
    (should-not (file-exists-p (eln-opt--quiet-file)))))

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

;; ---- eln-opt-compile-all: the files it chooses, and the request it makes ----

(defvar xl-dotfiles)

(ert-deftest eln-opt-compile-all-picks-our-files-and-asks-first ()
  "Our lisp/ and personal.el are chosen; autoloads are not; a yes starts one
worker with exactly those files; a no starts nothing."
  (let* ((root (file-name-as-directory (make-temp-file "eln-opt-all-" t)))
         (xl-dotfiles root)
         (lisp (expand-file-name "tuned-emacs/lisp/" root))
         (personal (expand-file-name "tuned-emacs/personal.el" root))
         (started nil))
    (unwind-protect
        (progn
          (make-directory lisp t)
          (with-temp-file (expand-file-name "a.el" lisp) (insert "(defun a1 () 1)\n"))
          (with-temp-file (expand-file-name "foo-autoloads.el" lisp) (insert ";;; x\n"))
          (with-temp-file personal (insert "(defun p1 () 2)\n"))
          (let ((files (eln-opt--all-sources)))
            (should (member (expand-file-name "a.el" lisp) files))
            (should (member personal files))
            (should-not (cl-some (lambda (f) (string-match-p "autoloads" f)) files)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                    ((symbol-function 'eln-opt-auto--start)
                     (lambda (force sources) (setq started (list force sources)) 'proc)))
            (eln-opt-compile-all))
          (should (eq (car started) t))
          (should (member personal (cadr started)))
          (setq started nil)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                    ((symbol-function 'eln-opt-auto--start)
                     (lambda (&rest _) (setq started 'wrongly))))
            (eln-opt-compile-all))
          (should-not started))
      (delete-directory root t))))

;; ---- A file that opts out of native compilation is never planned, whatever it is called ----

(ert-deftest eln-opt-opted-out-file-is-never-planned-whatever-its-name ()
  "The cookie decides: the same file with and without `no-native-compile' is
refused and planned respectively, even as a mandatory row with a high score."
  (let* ((root (file-name-as-directory (make-temp-file "eln-optout-" t)))
         (xl-dotfiles root)
         (lisp (expand-file-name "tuned-emacs/lisp/" root))
         (optout (expand-file-name "zz-quiet.el" lisp))
         (plain (expand-file-name "zz-plain.el" lisp)))
    (unwind-protect
        (progn
          (make-directory lisp t)
          (with-temp-file optout
            (insert ";;; zz-quiet.el\n;; Local Variables:\n;; no-native-compile: t\n;; End:\n(defun zz-quiet-fn () 1)\n"))
          (with-temp-file plain (insert ";;; zz-plain.el\n(defun zz-plain-fn () 1)\n"))
          (let* ((row (lambda (path)
                        (list :file (file-name-sans-extension (file-name-nondirectory path))
                              :path path :score (+ eln-opt-min-score 10) :size 100
                              :mandatory t :top nil :defuns nil)))
                 (quiet-row (funcall row optout))
                 (plain-row (funcall row plain)))
            (should (string-match-p "opts out" (eln-opt--candidate-reason quiet-row)))
            (should-not (eln-opt--candidate-p quiet-row))
            (should (eln-opt--candidate-p plain-row))
            (let ((plan (eln-opt--plan-from-rows (list quiet-row plain-row) 10)))
              (should-not (seq-find (lambda (x) (string-match-p "zz-quiet" (format "%S" x))) plan))
              (should (seq-find (lambda (x) (string-match-p "zz-plain" (format "%S" x))) plan)))))
      (delete-directory root t))))

(provide 'eln-opt-tests)
;;; The worker: built and failed, the plan, the compile loop

(ert-deftest eln-opt-built-means-an-eln-emacs-will-use ()
  "Built: an eln exists for the source and the .elc is not newer than it."
  (skip-unless (native-comp-available-p))
  (let* ((dir (make-temp-file "eln-opt-built-" t))
         (cache (file-name-as-directory (expand-file-name "cache" dir)))
         (native-comp-eln-load-path (list cache (car (last native-comp-eln-load-path))))
         (src (eln-opt-test--toy-src dir "built-toy"))
         (byte-compile-verbose nil))
    (unwind-protect
        (progn
          (byte-compile-file src)
          (should-not (eln-opt--built-p src))
          (set-file-times (concat src "c") (time-subtract nil 100))
          (native-compile src)
          (should (eln-opt--built-p src))
          ;; A newer .elc, as after a package upgrade: Emacs ignores the eln.
          (set-file-times (concat src "c") (time-add nil 100))
          (should-not (eln-opt--built-p src)))
      (ignore-errors (fmakunbound 'built-toy-fn))
      (delete-directory dir t))))

;; Emacs ships its own libraries as .elc plus .el.gz, with no .el.
(ert-deftest eln-opt-gz-source-compiles-to-the-eln-emacs-loads ()
  "With only an .el.gz next to the .elc, the .el.gz is the source: its size is
counted uncompressed, compiling it gives the eln Emacs looks for, and loading
the .elc then runs native code."
  (skip-unless (and (native-comp-available-p) (executable-find "gzip")))
  (let* ((dir (make-temp-file "eln-opt-gz-" t))
         (cache (file-name-as-directory (expand-file-name "cache" dir)))
         (native-comp-eln-load-path (list cache (car (last native-comp-eln-load-path))))
         (el (eln-opt-test--toy-src dir "gz-toy"))
         (elc (concat el "c"))
         (gz (concat el ".gz"))
         (bytes (eln-opt--file-size el))
         (byte-compile-verbose nil))
    (unwind-protect
        (progn
          (byte-compile-file el)
          (set-file-times elc (time-subtract nil 100))
          (should (zerop (call-process "gzip" nil nil nil el)))
          (should-not (file-exists-p el))
          ;; Older than the .elc, as in an Emacs install, so `load-prefer-newer'
          ;; still picks the .elc.
          (set-file-times gz (time-subtract nil 200))
          (should (equal (eln-opt--source-for elc) gz))
          (should (equal (eln-opt--source-for gz) gz))
          (should (equal (eln-opt--elc-for gz) elc))
          (should (= (eln-opt--source-bytes gz) bytes))
          (should-not (eln-opt--built-p gz))
          (native-compile gz)
          (should (eln-opt--built-p gz))
          ;; Without a suffix, as `require' loads: only then does Emacs look
          ;; for an eln.  Loading the .elc by its full name never swaps.
          (load (file-name-sans-extension el) nil t)
          (should (native-comp-function-p (symbol-function 'gz-toy-fn))))
      (ignore-errors (fmakunbound 'gz-toy-fn))
      (delete-directory dir t))))

(ert-deftest eln-opt-source-for-prefers-the-el ()
  "An .el next to the .elc wins over an .el.gz; with neither, the .el path."
  (let ((dir (make-temp-file "eln-opt-src-" t)))
    (unwind-protect
        (let ((el (expand-file-name "a.el" dir)))
          (should (equal (eln-opt--source-for (concat el "c")) el))
          (write-region "" nil (concat el ".gz"))
          (should (equal (eln-opt--source-for (concat el "c")) (concat el ".gz")))
          (write-region "" nil el)
          (should (equal (eln-opt--source-for (concat el "c")) el)))
      (delete-directory dir t))))

(ert-deftest eln-opt-failed-list-holds-until-the-source-changes ()
  "A failed source is skipped until its content changes; one entry per source."
  (eln-opt-test--with-state
    (let* ((dir (make-temp-file "eln-opt-failed-" t))
           (src (eln-opt-test--toy-src dir "bad")))
      (unwind-protect
          (progn
            (should-not (eln-opt--failed-p src))
            (eln-opt--record-failure src "boom")
            (should (eln-opt--failed-p src))
            (eln-opt--record-failure src "boom again")
            (should (= (length (eln-opt--failed-load)) 1))
            (with-temp-file src (insert ";;; bad.el\n(defun bad-fn () 2)\n"))
            (should-not (eln-opt--failed-p src)))
        (delete-directory dir t)))))

(ert-deftest eln-opt-plan-skips-built-and-failed-and-keeps-the-order ()
  "The plan is the candidates in score order, without built or failed ones."
  (eln-opt-test--with-tier2
    (let* ((srcs (mapcar (lambda (n) (eln-opt-test--src lisp n)) '("p1" "p2" "p3" "p4")))
           (rows (mapcar (lambda (f) (eln-opt-test--row f 100.0)) srcs)))
      (cl-letf (((symbol-function 'eln-opt--built-p)
                 (lambda (src) (string-suffix-p "p2.el" src))))
        (eln-opt--record-failure (nth 2 srcs) "boom")
        (should (equal (mapcar #'file-name-nondirectory (eln-opt--plan-from-rows rows))
                       '("p1.el" "p4.el")))))))

(defmacro eln-opt-test--with-worker (srcs-var names &rest body)
  "Temporary state and sources named NAMES bound to SRCS-VAR; the platform is
available, the plan is those sources, and the worker does not sleep."
  (declare (indent 2))
  `(eln-opt-test--with-tier2
     (let ((,srcs-var (mapcar (lambda (n) (eln-opt-test--src lisp n)) ,names))
           (xl-perfetto-marker-file (expand-file-name "no-such-marker.eld" temporary-file-directory)))
       (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () t))
                 ((symbol-function 'eln-opt-worker-plan)
                  (lambda (&rest _) (mapcar (lambda (s) (cons 'native s)) ,srcs-var)))
                 ((symbol-function 'sleep-for) #'ignore))
         ,@body))))

(ert-deftest eln-opt-worker-compiles-in-turn-and-goes-on-past-a-failure ()
  "The worker compiles each file in turn.  A failure goes on the failed list,
and the worker goes on with the next file, so one bad file does not block the
rest."
  (eln-opt-test--with-worker srcs '("w1" "w2" "w3")
    (let ((compiled nil))
      (cl-letf (((symbol-function 'native-compile)
                 (lambda (src &rest _)
                   (push (file-name-nondirectory src) compiled)
                   (when (string-suffix-p "w2.el" src) (error "Cannot compile"))))
                ((symbol-function 'eln-opt--built-p) (lambda (_) t)))
        (let ((out (with-output-to-string (eln-opt-worker))))
          (should (string-match-p "ELN-OPT done .*w1\\.el" out))
          (should (string-match-p "ELN-OPT failed .*w2\\.el" out))
          (should (string-match-p "ELN-OPT done .*w3\\.el" out))
          (should (string-match-p "finished 2, failed 1" out))
          (should (equal (reverse compiled) '("w1.el" "w2.el" "w3.el")))
          (should (eln-opt--failed-p (nth 1 srcs)))
          (should-not (eln-opt--failed-p (nth 0 srcs)))
          (should-not (eln-opt--failed-p (nth 2 srcs)))
          (should-not (file-exists-p (eln-opt--lock-dir))))))))

(ert-deftest eln-opt-worker-counts-a-compile-without-a-usable-eln-as-failed ()
  "A compile that leaves no eln Emacs would use is a failure, not a success."
  (eln-opt-test--with-worker srcs '("n1")
    (cl-letf (((symbol-function 'native-compile) #'ignore)
              ((symbol-function 'eln-opt--built-p) (lambda (_) nil)))
      (let ((out (with-output-to-string (eln-opt-worker))))
        (should (string-match-p "ELN-OPT failed .*no eln" out))
        (should (eln-opt--failed-p (car srcs)))))))

(ert-deftest eln-opt-worker-is-busy-when-another-holds-the-lock ()
  "A second worker exits at once when the lock is held."
  (eln-opt-test--with-worker srcs '("b1")
    (should (eln-opt--lock-acquire))
    (let ((compiled 0))
      (cl-letf (((symbol-function 'native-compile) (lambda (&rest _) (cl-incf compiled))))
        (should (eq (eln-opt-worker) 'busy))
        (should (= compiled 0))))
    (eln-opt--lock-release)))

;;; The main Emacs: one idle start, a summary, a watchdog

(defmacro eln-opt-test--with-auto (&rest body)
  "No worker, no timers armed; timers are recorded instead."
  (declare (indent 0))
  `(let ((eln-opt-auto-enable t)
         (eln-opt-auto--process nil)
         (eln-opt-auto--timer nil)
         (eln-opt-auto--current nil)
         (timers nil))
     (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
               ((symbol-function 'run-with-idle-timer)
                (lambda (secs repeat fn) (push (list secs repeat fn) timers) 'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore))
       ,@body)))

(ert-deftest eln-opt-auto-arms-one-idle-start-per-session ()
  "Arming schedules one idle timer that does not repeat; batch arms nothing."
  (eln-opt-test--with-auto
    (let ((noninteractive t)) (eln-opt-auto-arm))
    (should (null timers))
    (let ((noninteractive nil)) (eln-opt-auto-arm))
    (should (equal (car timers) (list eln-opt-auto-idle nil #'eln-opt-auto--start)))))

(ert-deftest eln-opt-auto-decision-says-why-not ()
  "Each reason the worker may not start is reported; with none, it may."
  (eln-opt-test--with-auto
    (should (null (eln-opt-auto--decision)))
    (let ((eln-opt-auto-enable nil))
      (should (eq (eln-opt-auto--decision) 'disabled)))
    (let ((process-environment (cons "ELN_OPT=0" process-environment)))
      (should (eq (eln-opt-auto--decision) 'disabled)))
    ;; No native compilation is no reason: the worker still byte-compiles.
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (should (null (eln-opt-auto--decision))))
    (let ((eln-opt-auto--process (xl-async-start (list "sleep" "5"))))
      (unwind-protect
          (should (eq (eln-opt-auto--decision) 'running))
        (delete-process eln-opt-auto--process)))))

(ert-deftest eln-opt-auto-summary-says-what-the-worker-did ()
  "The summary counts the files done and names the failure, if any."
  (should (string-match-p "compiled 2 file"
                          (eln-opt-auto--summary
                           "ELN-OPT plan 2 a b\nELN-OPT done /x/a.el\nELN-OPT done /x/b.el\nELN-OPT finished 2\n")))
  (should (string-match-p "1 failed"
                          (eln-opt-auto--summary
                           "ELN-OPT done /x/a.el\nELN-OPT failed /x/b.el boom\nELN-OPT finished 1\n")))
  (should (string-match-p "nothing to compile" (eln-opt-auto--summary "ELN-OPT plan 0 \nELN-OPT finished 0\n")))
  (should (string-match-p "lock" (eln-opt-auto--summary "ELN-OPT busy\n")))
  (should (string-match-p "stopped early" (eln-opt-auto--summary "Loading init...\n"))))

(ert-deftest eln-opt-auto-notes-the-file-the-worker-is-on ()
  "The file being compiled is tracked from the worker's start and done lines."
  (eln-opt-test--with-auto
    (eln-opt-auto--note "Loading x\nELN-OPT start /x/a.el\n")
    (should (equal eln-opt-auto--current "/x/a.el"))
    (eln-opt-auto--note "ELN-OPT done /x/a.el\n")
    (should (null eln-opt-auto--current))))

(ert-deftest eln-opt-auto-watchdog-kills-a-stalled-worker-and-skips-its-file ()
  "A worker silent for longer than the stall limit is killed, and the file it
was on goes on the failed list."
  (eln-opt-test--with-state
    (eln-opt-test--with-auto
      (let* ((dir (make-temp-file "eln-opt-stall-" t))
             (src (eln-opt-test--toy-src dir "stuck"))
             (eln-opt-auto--process (xl-async-start (list "sleep" "30")))
             (eln-opt-auto--current src)
             (eln-opt-auto--last-output (- (float-time) eln-opt-worker-stall 10)))
        (unwind-protect
            (progn
              (eln-opt-auto--watch)
              (should-not (process-live-p eln-opt-auto--process))
              (should (eln-opt--failed-p src)))
          (when (process-live-p eln-opt-auto--process)
            (delete-process eln-opt-auto--process))
          (delete-directory dir t))))))

(ert-deftest eln-opt-auto-command-runs-the-worker-at-background-priority ()
  "The worker runs under taskpolicy -b when present, nice -n 19 otherwise."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (p) (and (equal p "taskpolicy") "/usr/sbin/taskpolicy"))))
    (should (equal (seq-take (eln-opt-auto--command) 2) '("taskpolicy" "-b"))))
  (cl-letf (((symbol-function 'executable-find)
             (lambda (p) (and (equal p "nice") "/usr/bin/nice"))))
    (should (equal (seq-take (eln-opt-auto--command) 3) '("nice" "-n" "19"))))
  (should (cl-some (lambda (a) (and (stringp a) (string-match-p "(eln-opt-worker)" a)))
                   (eln-opt-auto--command))))

;;; eln-opt-tests.el ends here
