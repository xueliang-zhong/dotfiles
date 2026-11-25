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

(ert-deftest eln-opt-unavailable-writes-only-platform-file ()
  "Without native compilation, a run stops at the gate and writes one file."
  (eln-opt-test--with-state
    (let ((canary-calls 0))
      (eln-opt-test--count-canary canary-calls
        (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
          (should (eq (eln-opt-test--worker-result) 'unavailable))
          (should (eq (eln-opt-test--worker-result) 'unavailable))))
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
        (should (eq (eln-opt-test--worker-result) 'unavailable))
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
  "On a platform without native compilation the report says so and ranks nothing."
  (eln-opt-test--with-state
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (let ((text (with-output-to-string
                    (let ((noninteractive t))
                      (eln-opt-report)))))
        (should (string-match-p "Native compilation is not available" text))
        (should-not (string-match-p "Next worker compiles" text))
        (should-not (string-match-p "ranking" text))))))

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
     (let ((,srcs-var (mapcar (lambda (n) (eln-opt-test--src lisp n)) ,names)))
       (cl-letf (((symbol-function 'eln-opt-platform-available-p) (lambda () t))
                 ((symbol-function 'eln-opt-worker-plan) (lambda () ,srcs-var))
                 ((symbol-function 'sleep-for) #'ignore))
         ,@body))))

(ert-deftest eln-opt-worker-compiles-in-turn-and-stops-at-the-first-error ()
  "The worker compiles each file in turn, and stops at the first failure,
which goes on the failed list.  The files after it are not started."
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
          (should-not (string-match-p "w3\\.el\n" (substring out (string-match "ELN-OPT failed" out))))
          (should (equal (reverse compiled) '("w1.el" "w2.el")))
          (should (eln-opt--failed-p (nth 1 srcs)))
          (should-not (eln-opt--failed-p (nth 0 srcs)))
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
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
      (should (eq (eln-opt-auto--decision) 'unavailable)))
    (let ((eln-opt-auto--process (start-process "eln-opt-test" nil "sleep" "5")))
      (unwind-protect
          (should (eq (eln-opt-auto--decision) 'running))
        (delete-process eln-opt-auto--process)))))

(ert-deftest eln-opt-auto-summary-says-what-the-worker-did ()
  "The summary counts the files done and names the failure, if any."
  (should (string-match-p "compiled 2 file"
                          (eln-opt-auto--summary
                           "ELN-OPT plan 2 a b\nELN-OPT done /x/a.el\nELN-OPT done /x/b.el\nELN-OPT finished 2\n")))
  (should (string-match-p "then failed: /x/b.el"
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
             (eln-opt-auto--process (start-process "eln-opt-test" nil "sleep" "30"))
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
