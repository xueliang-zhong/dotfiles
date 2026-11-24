;;; xl-packages.el --- Find packages in consult, fed by a child Emacs  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (consult "1.0"))
;; Keywords: tools, convenience

;;; Commentary:
;; `consult-list-packages' lists every installed and available package in a
;; consult minibuffer.  The list comes from a child Emacs that loads this
;; profile and prints one line per package, so the main Emacs never blocks on
;; the archives.  The lines arrive as they are read, and consult's own
;; indicator (animated by `xl-consult-spinner') shows that the list is still
;; coming in.
;;
;; The child reads the archive caches only; it never goes to the network.
;; Choosing a package shows its description with `describe-package', which
;; offers the install and delete buttons itself.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'package)
(require 'consult)
(require 'xl-consult-spinner)

(defconst xl-packages--marker "XLPKG"
  "First field of every line the child prints.  Other output from the child
\(its loading messages\) does not carry it and is dropped.")

;;; The child: runs in a batch Emacs with the profile loaded

(defun xl-packages--status (desc avail)
  "The status of installed package DESC, given its newer archive version AVAIL."
  (if (and avail
           (version-list-< (package-desc-version desc) (package-desc-version avail)))
      "upgradable"
    "installed"))

(defun xl-packages--newest (descs)
  "The package description with the highest version in DESCS, or nil."
  (car (sort (copy-sequence descs)
             (lambda (a b) (version-list-< (package-desc-version b)
                                           (package-desc-version a))))))

(defun xl-packages--field (text)
  "TEXT made safe for one tab-separated field."
  (replace-regexp-in-string "[\t\n\r]+" " " (or text "")))

(defun xl-packages--line (name status version archive summary)
  (string-join (list xl-packages--marker
                     (xl-packages--field name) status (xl-packages--field version)
                     (xl-packages--field archive) (xl-packages--field summary))
               "\t"))

(defun xl-packages-print-rows ()
  "Print one marked line per package: installed, upgradable, and available.
Runs in the child.  Reads the archive caches only; no network."
  (package-read-all-archive-contents)
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (entry package-alist)
      (let* ((name (car entry))
             (desc (cadr entry))
             (avail (xl-packages--newest (cdr (assq name package-archive-contents))))
             (status (xl-packages--status desc avail))
             (shown (if (equal status "upgradable") avail desc)))
        (puthash name t seen)
        (princ (format "%s\n"
                       (xl-packages--line
                        (symbol-name name) status
                        (package-version-join (package-desc-version shown))
                        (package-desc-archive shown)
                        (package-desc-summary desc))))))
    (dolist (entry package-archive-contents)
      (unless (gethash (car entry) seen)
        (let ((desc (xl-packages--newest (cdr entry))))
          (when desc
            (princ (format "%s\n"
                           (xl-packages--line
                            (symbol-name (car entry)) "available"
                            (package-version-join (package-desc-version desc))
                            (package-desc-archive desc)
                            (package-desc-summary desc))))))))))

;;; The main Emacs: consult candidates

(defun xl-packages--command ()
  "The child command: this binary, this profile, the same init, then the printer."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory)))
    (list exe "--batch" "--init-directory" profile
          "-l" (expand-file-name "early-init.el" profile)
          "--eval" "(run-hooks (quote before-init-hook))"
          "-l" (expand-file-name "init.el" profile)
          "--eval" "(progn (require (quote xl-packages)) (xl-packages-print-rows))")))

(defun xl-packages--face (status)
  (pcase status
    ("installed" 'success)
    ("upgradable" 'warning)
    (_ 'shadow)))

(defun xl-packages--candidate (line)
  "A consult candidate for one marked LINE from the child.
The name rides on the string as a property, so the lookup can return it."
  (let* ((fields (split-string line "\t"))
         (name (nth 1 fields))
         (status (nth 2 fields))
         (version (nth 3 fields))
         (archive (nth 4 fields))
         (summary (nth 5 fields))
         (text (format "%-34s %-11s %-12s %-12s %s"
                       name status version archive (or summary ""))))
    (propertize (propertize text 'face (xl-packages--face status))
                'xl-package name)))

(defun xl-packages--marked-p (line)
  (string-prefix-p (concat xl-packages--marker "\t") line))

;;;###autoload
(defun consult-list-packages ()
  "Choose a package from every installed and available one, then describe it.
The list is fed asynchronously from a child Emacs, with consult's indicator
shown while it comes in."
  (interactive)
  (let* ((command (xl-packages--command))
         (name (consult--read
                (consult--async-pipeline
                 (consult--async-indicator)
                 (consult--async-process (lambda (_input) command))
                 (consult--async-filter #'xl-packages--marked-p)
                 (consult--async-map #'xl-packages--candidate))
                :prompt "Package: "
                :require-match t
                :category 'package
                :sort nil
                :lookup (lambda (cand &rest _)
                          (get-text-property 0 'xl-package cand)))))
    (when (and name (not (string-empty-p name)))
      (describe-package (intern name)))))

(provide 'xl-packages)
;;; xl-packages.el ends here
