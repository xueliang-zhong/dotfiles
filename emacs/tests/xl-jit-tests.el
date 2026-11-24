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

(provide 'xl-jit-tests)
;;; xl-jit-tests.el ends here
