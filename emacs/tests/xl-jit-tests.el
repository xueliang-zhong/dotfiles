;;; xl-jit-tests.el --- ERT tests for xl-jit -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L emacs/lisp -l emacs/tests/xl-jit-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Every test works in its own temporary state folder.  Each one checks the
;; final state and that a second run does no compilation at all.

(require 'ert)
(require 'cl-lib)
(require 'xl-jit)

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

(ert-deftest xl-jit-state-file-is-read-with-read-only ()
  "An oversized or non-Lisp platform file is ignored, not evaluated."
  (xl-jit-test--with-state
    (make-directory xl-jit-state-dir t)
    (with-temp-file (xl-jit--platform-file)
      (insert "#.(error \"must not run\")"))
    (should (null (xl-jit--read-eld (xl-jit--platform-file))))))

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

(provide 'xl-jit-tests)
;;; xl-jit-tests.el ends here
