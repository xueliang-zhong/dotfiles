;;; xl-bootstrap.el --- install the packages the profile needs -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, packages

;;; Commentary:
;; The install path of the profile: read the archives, fetch and check the
;; packages in parallel, install whatever is left, and fetch the few packages
;; that live only in Git.  It runs before the Crafted modules load, and only
;; does network work when a selected package is missing.
;;
;; `init.el' keeps the manifest (which Crafted modules and which packages)
;; and calls `xl-bootstrap'; this file keeps the machinery.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar xl-progress (equal (getenv "XL_PROGRESS") "1")
  "Non-nil when emacs-setup.sh asked for step-by-step progress output.")

(defun xl-report (format-string &rest args)
  "Print a progress line when `xl-progress' is set.
Written to stderr one character at a time: batch stdout is buffered, so
progress written to it appears only at exit.  stderr is unbuffered, so
every character appears as soon as it is written."
  (when xl-progress
    (mapc #'external-debugging-output
          (concat (apply #'format format-string args) "\n"))))

(defun xl-package-present-p (pkg)
  "Non-nil when PKG is activated or built-in.
Checks `package-alist' and `locate-library' first so a normal start
where every package is already installed never needs to load
`package.el' and its 37 dependencies."
  (if (or (featurep 'package) (null package-alist))
      (package-installed-p pkg)
    (or (assq pkg package-alist)
        (locate-library (symbol-name pkg)))))

(defun xl-refresh-package-keyring ()
  "Rebuild package.el's GPG home from the keyring shipped with Emacs.
A stale or corrupt `package-gnupghome-dir' makes GNU ELPA fail signature
verification, which hides GNU packages (denote, for example) from
`package-archive-contents' even after a refresh."
  (require 'package)
  (when (and package-gnupghome-dir (file-directory-p package-gnupghome-dir))
    (ignore-errors (delete-directory package-gnupghome-dir t)))
  (let ((keyring (expand-file-name "package-keyring.gpg" data-directory)))
    (when (and (package-check-signature) (file-exists-p keyring))
      (ignore-errors (package-import-keyring keyring)))))

(defun xl-install-packages (packages)
  "Install PACKAGES one at a time, reporting each when `xl-progress' is set.
Each package is installed the normal way, byte-compiled by `package.el'."
  (require 'package)
  (let ((total (length packages))
        (index 0))
    (when packages
      (xl-report "installing %d package(s)" total))
    (dolist (pkg packages)
      (setq index (1+ index))
      (xl-report "package %d/%d: %s" index total pkg)
      (condition-case err
          (unless (package-installed-p pkg)
            (package-install pkg))
        (error
         (xl-report "package %d/%d failed: %s (%s)"
                    index total pkg (error-message-string err)))))))

(defun xl-refresh-archives ()
  "Refresh the package archives, downloading them in parallel.
`package-refresh-contents' with ASYNC starts every archive at once.  This
waits for them and prints one dot per archive as it finishes, so progress is
visible.  An archive has finished when it is no longer in
`package--downloads-in-progress', which package.el keeps."
  (let ((total (length package-archives))
        (printed 0)
        (start (float-time)))
    (cl-flet ((print-finished ()
                (let ((finished (- total (length package--downloads-in-progress))))
                  (when (and xl-progress (> finished printed))
                    (dotimes (_ (- finished printed))
                      (external-debugging-output ?.)))
                  (setq printed (max printed finished)))))
      (package-refresh-contents t)
      (print-finished)
      (while package--downloads-in-progress
        (accept-process-output nil 0.05)
        (print-finished)))
    (when xl-progress (external-debugging-output ?\n))
    ;; A failed download is reported by package.el in the echo area; say
    ;; which archive it was, and that the install goes on with what is cached.
    (dolist (archive package-archives)
      (let ((file (expand-file-name
                   "archive-contents"
                   (expand-file-name (concat "archives/" (car archive))
                                     package-user-dir))))
        (unless (and (file-exists-p file)
                     (time-less-p (float-time start)
                                  (float-time (file-attribute-modification-time
                                               (file-attributes file)))))
          (message "Could not refresh archive %s (%s); using its cached index, if any"
                   (car archive) (cdr archive)))))))

(defun xl-install-missing-packages ()
  "Refresh the archives, then install what `package-selected-packages' lists.
`package-install-selected-packages' only refreshes when the archive cache is
empty.  An old cache makes package.el report a dependency as unavailable
even though the archive offers it: transient asked for compat 31.1 on a
machine whose cached index predated it.  So refresh whenever this path runs;
that is only when a package is actually missing.

When a refresh leaves packages missing, rebuild package.el's keyring and
try once more: a stale keyring makes the GNU ELPA signature check fail, so
GNU packages such as denote never enter `package-archive-contents'.  If
gpg is unusable even then, retry once without signature checks for this
session."
  (when-let* ((missing (seq-remove #'xl-package-present-p package-selected-packages)))
    (require 'package)
    ;; Installing byte-compiles; load bytecomp now, not from inside some lexical
    ;; binding of its variables (that fails on Windows and some Linux builds).
    (require 'bytecomp)
    (xl-report "refreshing package archives ...")
    (xl-refresh-archives)
    ;; Parallel download and gpg check; whatever it cannot finish is left to
    ;; `xl-install-packages' below.
    (require 'xl-package-fetch)
    (xl-fetch-packages (seq-remove #'package-installed-p package-selected-packages))
    (xl-install-packages (seq-remove #'package-installed-p package-selected-packages))
    (when-let* ((still (seq-remove #'package-installed-p package-selected-packages)))
      (message "Package install incomplete (%S); rebuilding the ELPA keyring"
               still)
      (xl-report "rebuilding the ELPA keyring")
      (xl-refresh-package-keyring)
      (xl-report "refreshing package archives again")
      (xl-refresh-archives)
      (xl-install-packages (seq-remove #'package-installed-p package-selected-packages))
      (when-let* ((still (seq-remove #'package-installed-p package-selected-packages)))
        ;; Last resort for machines without a usable gpg: download over
        ;; HTTPS without checking the archive signatures for this session.
        (message "Still missing %S; retrying without ELPA signature verification"
                 still)
        (xl-report "retrying without signature verification")
        (let ((package-check-signature nil))
          (xl-refresh-archives)
          (xl-install-packages
           (seq-remove #'package-installed-p package-selected-packages)))
        (when-let* ((still (seq-remove #'package-installed-p package-selected-packages)))
          (let ((unavailable (seq-remove
                              (lambda (pkg) (assoc pkg package-archive-contents))
                              still)))
            (error "Still missing after refreshing (and rebuilding the ELPA keyring): %S%s.  Check `package-archives' and gpg (`package-check-signature'); a broken GNU ELPA keyring hides its packages."
                   still
                   (if unavailable
                       (format " (not offered by any archive: %S)" unavailable)
                     ""))))))))

(defconst xl-vc-packages
  '((pyim-tsinghua-dict
     :url "https://github.com/redguardtoo/pyim-tsinghua-dict"
     :rev "9f4a665db629b20a04d4ee6b93582b0abe73e977"))
  "Packages that live only in Git, pinned by URL and commit.
pyim's bigger pinyin dictionary is not in any ELPA.  Fetch it from its Git
repository, pinned like Crafted.")

(defun xl-install-vc-packages ()
  "Install the packages in `xl-vc-packages' that are missing.
When that fails (no network or no git on the first run), pyim still works
with its own small dictionary, and the next start retries."
  (when (seq-remove #'package-installed-p (mapcar #'car xl-vc-packages))
    (require 'package-vc)
    (dolist (spec xl-vc-packages)
      (unless (package-installed-p (car spec))
        (condition-case err
            (progn
              (package-vc-install (plist-get (cdr spec) :url)
                                  (plist-get (cdr spec) :rev)
                                  nil (car spec))
              ;; This session activated packages before the install, so put the
              ;; new one on `load-path' now.
              (package-activate-all))
          (error (message "Could not install %s: %s"
                          (car spec) (error-message-string err))))))))

(defun xl-bootstrap ()
  "Install the packages the profile selects, before any module needs them."
  (xl-install-missing-packages)
  (xl-install-vc-packages))

(provide 'xl-bootstrap)
;;; xl-bootstrap.el ends here
