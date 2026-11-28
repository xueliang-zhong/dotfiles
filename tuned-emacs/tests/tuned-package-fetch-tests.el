;;; tuned-package-fetch-tests.el --- ERT tests for tuned-package-fetch -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-package-fetch-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; No network: the archive is a fake `package-archive-contents', and the gpg
;; test uses a throwaway key in a temporary GNUPGHOME.

(require 'ert)
(require 'cl-lib)
(require 'package)
(require 'tuned-package-fetch)

(defun tuned-fetch-test--desc (name version reqs &optional archive)
  "A fake available package NAME at VERSION (a string) needing REQS."
  (package-desc-create :name (intern name) :version (version-to-list version)
                       :summary name :reqs reqs :kind 'single
                       :archive (or archive "gnu")))

(ert-deftest tuned-fetch-run-returns-results-in-command-order ()
  "Each command's exit code and output come back in the order given."
  (let ((results (tuned-fetch--run
                  (list (list "sh" "-c" "echo one; exit 0")
                        (list "sh" "-c" "echo two; exit 3")
                        (list "sh" "-c" "sleep 0.2; echo three; exit 0")))))
    (should (= (length results) 3))
    (should (= (car (nth 0 results)) 0))
    (should (string-match-p "one" (cdr (nth 0 results))))
    (should (= (car (nth 1 results)) 3))
    (should (string-match-p "two" (cdr (nth 1 results))))
    (should (string-match-p "three" (cdr (nth 2 results))))))

(ert-deftest tuned-fetch-run-with-no-commands-returns-nil ()
  "An empty list of commands returns at once, with no results."
  (should-not (tuned-fetch--run nil)))

(ert-deftest tuned-fetch-run-never-exceeds-the-job-limit ()
  "With a limit of 2, four sleeping jobs take at least two rounds."
  (let* ((tuned-fetch-max-jobs 2)
         (start (float-time))
         (_ (tuned-fetch--run (make-list 4 (list "sleep" "0.4"))))
         (elapsed (- (float-time) start)))
    ;; All four at once would take about 0.4 s; two rounds take about 0.8 s.
    (should (>= elapsed 0.7))))

(ert-deftest tuned-fetch-wanted-lists-dependencies-first-once ()
  "A package's requirements come before it, and each appears only once."
  (let* ((base (tuned-fetch-test--desc "base" "1.0" nil))
         (mid (tuned-fetch-test--desc "mid" "1.0" '((base (1 0)))))
         (top (tuned-fetch-test--desc "top" "2.0" '((mid (1 0)) (base (1 0)))))
         (package-alist nil)
         (package-archive-contents
          (list (list 'top top) (list 'mid mid) (list 'base base))))
    (let ((names (mapcar #'package-desc-name
                         (tuned-fetch--wanted '(top mid)))))
      (should (equal (seq-uniq names) names))
      (should (equal (car (last names)) 'top))
      (should (< (seq-position names 'base) (seq-position names 'mid))))))

(ert-deftest tuned-fetch-wanted-skips-installed-packages ()
  "A package already installed at a good version is not in the plan."
  (let* ((base (tuned-fetch-test--desc "base" "1.0" nil))
         (package-alist (list (list 'base base)))
         (package-archive-contents (list (list 'base base))))
    (should-not (tuned-fetch--wanted '(base)))))

(ert-deftest tuned-fetch-allow-unsigned-follows-package-check-signature ()
  "Unsigned archives are accepted only when package.el would accept them."
  (let ((package-unsigned-archives nil))
    (let ((package-check-signature t))
      (should-not (tuned-fetch--allow-unsigned-p "gnu")))
    (let ((package-check-signature 'allow-unsigned))
      (should (tuned-fetch--allow-unsigned-p "melpa")))
    (let ((package-check-signature nil))
      (should (tuned-fetch--allow-unsigned-p "gnu")))
    (let ((package-check-signature t)
          (package-unsigned-archives '("melpa")))
      (should (tuned-fetch--allow-unsigned-p "melpa")))))

(ert-deftest tuned-fetch-gpg-accepts-good-and-rejects-tampered-archives ()
  "The gpg check the parallel path runs: a good signature passes, and any
change to the archive file makes it fail."
  (skip-unless (executable-find "gpg"))
  (let* ((dir (make-temp-file "tuned-fetch-gpg-" t))
         (home (expand-file-name "gnupg" dir))
         (file (expand-file-name "pkg-1.0.el" dir))
         (sig (concat file ".sig")))
    (unwind-protect
        (progn
          (make-directory home t)
          (set-file-modes home #o700)
          (with-temp-file file (insert ";;; pkg.el --- test -*- lexical-binding: t; -*-\n"))
          (should (zerop (call-process "gpg" nil nil nil
                                       "--batch" "--quiet" "--homedir" home
                                       "--passphrase" ""
                                       "--quick-gen-key" "tuned-fetch-test <t@example.invalid>"
                                       "ed25519" "sign" "never")))
          (should (zerop (call-process "gpg" nil nil nil
                                       "--batch" "--quiet" "--homedir" home
                                       "--pinentry-mode" "loopback" "--passphrase" ""
                                       "--detach-sign" "--output" sig file)))
          (let ((verify (lambda ()
                          (call-process "gpg" nil nil nil
                                        "--batch" "--quiet" "--homedir" home
                                        "--verify" sig file))))
            (should (zerop (funcall verify)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (goto-char (point-max))
              (insert "X")
              (let ((coding-system-for-write 'binary))
                (write-region (point-min) (point-max) file nil 'silent)))
            (should-not (zerop (funcall verify)))))
      (ignore-errors (delete-directory dir t)))))

(provide 'tuned-package-fetch-tests)
;;; tuned-package-fetch-tests.el ends here
