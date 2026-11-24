;;; xl-jit-probe-bench.el --- interpreted vs byte code vs native -*- lexical-binding: t; -*-
;; Probe, not part of the config.  Run with: emacs --batch -Q -l THIS-FILE
;; Copies emacs/lisp to a temporary folder first, so no .elc or .eln is
;; written into the repository, then times two xl-git-status functions in
;; each of the three states.
(setq byte-compile-warnings nil native-comp-async-report-warnings-errors 'silent)

(defvar probe-src (expand-file-name "../../lisp" (file-name-directory load-file-name)))
(defvar probe-tmp (make-temp-file "xl-jit-bench-" t))
(copy-directory probe-src probe-tmp t t t)
(add-to-list 'load-path probe-tmp)
(require 'xl-spinner)
(require 'xl-async)

(defvar probe-status
  (concat "## master...origin/master\0"
          (mapconcat (lambda (i) (format " M src/dir%d/file-%d.el" (% i 40) i))
                     (number-sequence 1 20000) "\0")))
(defvar probe-diff
  (concat "diff --git a/f.el b/f.el\nindex 1..2 100644\n--- a/f.el\n+++ b/f.el\n"
          (mapconcat (lambda (i) (format "@@ -%d,3 +%d,3 @@\n context\n-old %d\n+new %d" i i i i))
                     (number-sequence 1 1500) "\n")))

(defun probe-bench (label)
  (garbage-collect)
  (let ((e (car (benchmark-run 5 (xl-git-status--entries probe-status))))
        (d (car (benchmark-run 5 (with-temp-buffer
                                   (xl-git-status--insert-diff "unstaged" "f.el" probe-diff))))))
    (princ (format "%-12s entries %.3fs   insert-diff %.3fs\n" label e d))
    (list e d)))

(let* ((src (expand-file-name "xl-git-status.el" probe-tmp))
       (_ (load src nil t))
       (interp (probe-bench "interpreted"))
       (_ (byte-compile-file src))
       (_ (load (concat src "c") nil t))
       (byte (probe-bench "byte-code"))
       (eln (native-compile src (expand-file-name "xl-git-status.eln" probe-tmp)))
       (_ (load eln nil t))
       (nat (probe-bench "native")))
  (princ (format "native vs byte-code: entries x%.1f, insert-diff x%.1f\n"
                 (/ (nth 0 byte) (nth 0 nat)) (/ (nth 1 byte) (nth 1 nat))))
  (princ (format "byte-code vs interpreted: entries x%.1f, insert-diff x%.1f\n"
                 (/ (nth 0 interp) (nth 0 byte)) (/ (nth 1 interp) (nth 1 byte)))))

(delete-directory probe-tmp t)
