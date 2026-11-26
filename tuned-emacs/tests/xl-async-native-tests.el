;;; xl-async-native-tests.el --- tests for the background native compile -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L tuned-emacs/lisp -L tuned-emacs/tests -l tuned-emacs/tests/xl-async-native-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'xl-async-native)

(defun xl-async-native-test--wait (box &optional seconds)
  "Wait until BOX holds a value, at most SECONDS (default 120)."
  (let ((deadline (+ (float-time) (or seconds 120))))
    (while (and (null (car box)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (car box)))

(defun xl-async-native-test--source (name body)
  "Write a temporary Emacs Lisp file NAME containing BODY; return its path."
  (let* ((dir (make-temp-file "xl-async-native-" t))
         (file (expand-file-name (concat name ".el") dir)))
    (with-temp-file file (insert body))
    file))

(ert-deftest xl-async-native-compile-produces-an-eln-in-the-background ()
  (skip-unless (and (fboundp 'native-comp-available-p) (native-comp-available-p)))
  (let* ((file (xl-async-native-test--source
                "xl-anc-produce" "(defun xl-anc-produce-demo (x) (* x x))\n"))
         (box (list nil)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (xl-async-native--compile file (lambda (eln) (setcar box (list eln))))
      (should (xl-async-native-test--wait box)))
    (let ((eln (car (car box))))
      (should (stringp eln))
      (should (equal (file-name-extension eln) "eln"))
      (should (file-exists-p eln)))))

(ert-deftest xl-async-native-compile-skips-a-file-already-up-to-date ()
  (skip-unless (and (fboundp 'native-comp-available-p) (native-comp-available-p)))
  (let* ((file (xl-async-native-test--source
                "xl-anc-current" "(defun xl-anc-current-demo () 1)\n"))
         (box (list nil)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (xl-async-native--compile file (lambda (eln) (setcar box (list eln))))
      (xl-async-native-test--wait box))
    ;; The .eln is now at least as new as the source: nothing starts.
    (should-not (xl-async-native--compile file #'ignore))))

(ert-deftest xl-async-native-compile-and-load-defines-native-code ()
  (skip-unless (and (fboundp 'native-comp-available-p) (native-comp-available-p)))
  (let* ((file (xl-async-native-test--source
                "xl-anc-load" "(defun xl-anc-load-demo (x) (+ x 1))\n"))
         (box (list nil)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (xl-async-native--compile
       file
       (lambda (eln)
         (native-elisp-load eln)
         (setcar box (list eln))))
      (xl-async-native-test--wait box))
    (should (fboundp 'xl-anc-load-demo))
    (should (= (xl-anc-load-demo 41) 42))
    (should (native-comp-function-p (symbol-function (quote xl-anc-load-demo))))))

(ert-deftest xl-async-native-compile-without-support-signals-a-user-error ()
  (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil)))
    (should-error (xl-async-native--check) :type 'user-error)))

(ert-deftest xl-async-native-compile-starts-from-the-profile-when-asked ()
  "With :profile the child is started with that profile's init directory."
  (let ((captured nil))
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
              ((symbol-function 'xl-async-evaluate)
               (lambda (_form _callback &rest args) (setq captured args) nil)))
      (xl-async-native-compile "/x/file.el" nil :profile "/p/profile/"))
    (should (equal (plist-get captured :init-directory) "/p/profile/"))))

(provide 'xl-async-native-tests)
;;; xl-async-native-tests.el ends here
