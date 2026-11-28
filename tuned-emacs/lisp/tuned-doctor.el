;;; tuned-doctor.el --- health checks behind `tuned doctor' -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools

;;; Commentary:
;;
;; The checks behind `tuned doctor' and `M-x tuned-doctor'.  A check returns
;; a plist with a status (`ok', `warn' or `fail'), a title, a detail and an
;; optional fix.  Only `fail' makes the command exit non-zero.

;;; Code:

(require 'package)
(require 'seq)
(require 'subr-x)

(defgroup tuned-doctor nil
  "Health checks for the tuned-emacs distribution."
  :group 'tools)

(defcustom tuned-doctor-programs '("git" "curl" "gpg")
  "Programs the installation needs."
  :type '(repeat string)
  :group 'tuned-doctor)

(defun tuned-doctor--result (status title &optional detail fix)
  "Build one doctor result with STATUS, TITLE, DETAIL and FIX."
  (list :status status :title title :detail detail :fix fix))

(defun tuned-doctor--failure-p (result)
  "Non-nil when RESULT is a failure."
  (eq (plist-get result :status) 'fail))

(defun tuned-doctor--distro-root ()
  "Return the tuned-emacs directory, or nil when the profile is missing."
  (let ((dotfiles (and (boundp 'tuned-dotfiles) tuned-dotfiles)))
    (and dotfiles (expand-file-name "tuned-emacs/" dotfiles))))

(defun tuned-doctor--version-string ()
  "Return the distribution version, or `unknown'."
  (or (and (boundp 'tuned-version) tuned-version)
      (let* ((root (tuned-doctor--distro-root))
             (file (and root (expand-file-name "VERSION" root))))
        (when (and file (file-readable-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (string-trim (buffer-string)))))
      "unknown"))

(defun tuned-doctor--check-version ()
  "Check that VERSION is readable."
  (let ((version (tuned-doctor--version-string)))
    (if (equal version "unknown")
        (tuned-doctor--result 'fail "version" "VERSION is missing"
                              "restore tuned-emacs/VERSION")
      (tuned-doctor--result 'ok "version" version))))

(defun tuned-doctor--check-emacs ()
  "Check the Emacs version and native compilation."
  (let ((old (< emacs-major-version 30)))
    (list
     (tuned-doctor--result
      (if old 'fail 'ok) "Emacs" emacs-version
      (when old "install Emacs 30 or newer"))
     (if (native-comp-available-p)
         (tuned-doctor--result 'ok "native compilation" "available")
       (tuned-doctor--result
        'warn "native compilation" "not available"
        "install an Emacs built with libgccjit")))))

(defun tuned-doctor--check-programs ()
  "Check the external programs package installation needs."
  (let ((missing (seq-remove #'executable-find tuned-doctor-programs)))
    (if missing
        (tuned-doctor--result
         'fail "programs" (format "missing %s" (string-join missing ", "))
         "install git, curl and gpg")
      (tuned-doctor--result
       'ok "programs" (string-join tuned-doctor-programs ", ")))))

(defun tuned-doctor--check-keyring ()
  "Check package.el's GPG home."
  (let ((dir (cond ((boundp 'package-gnupghome-dir) package-gnupghome-dir)
                   ((boundp 'package-user-dir)
                    (expand-file-name "gnupg" package-user-dir)))))
    (if (and dir (file-directory-p dir))
        (tuned-doctor--result 'ok "ELPA keyring" dir)
      (tuned-doctor--result
       'warn "ELPA keyring" "not found"
       "run tuned sync once to install packages"))))

(defun tuned-doctor--check-stubs ()
  "Check that the profile stubs load this checkout."
  (let ((dotfiles (and (boundp 'tuned-dotfiles) tuned-dotfiles))
        (crafted (and (boundp 'tuned-crafted-home) tuned-crafted-home))
        (init (expand-file-name "init.el" user-emacs-directory)))
    (cond
     ((not dotfiles)
      (tuned-doctor--result 'fail "profile stubs" "tuned-dotfiles is not set"
                            "run tuned install"))
     ((not (file-directory-p dotfiles))
      (tuned-doctor--result 'fail "profile stubs" dotfiles
                            "run tuned install"))
     ((not (file-exists-p (expand-file-name "tuned-emacs/init.el" dotfiles)))
      (tuned-doctor--result 'fail "profile stubs" "tuned-emacs/init.el is missing"
                            "run tuned install"))
     ((not (file-exists-p init))
      (tuned-doctor--result 'fail "profile stubs" init
                            "run tuned install"))
     ((not crafted)
      (tuned-doctor--result 'fail "profile stubs" "tuned-crafted-home is not set"
                            "run tuned install"))
     ((not (file-directory-p crafted))
      (tuned-doctor--result 'fail "profile stubs" crafted
                            "run tuned install"))
     (t (tuned-doctor--result
         'ok "profile stubs" (format "%s -> %s" user-emacs-directory dotfiles))))))

(defun tuned-doctor--user-folder ()
  "Return the first user folder that exists, or nil."
  (let* ((xdg (getenv "XDG_CONFIG_HOME"))
         (dirs (delq nil
                     (list (getenv "TUNED_EMACS_USER_DIR")
                           (and xdg (expand-file-name "tuned-emacs/" xdg))
                           (expand-file-name "~/.tuned-emacs.d/")))))
    (seq-find #'file-directory-p dirs)))

(defun tuned-doctor--check-user-folder ()
  "Check the user folder, which phase 4 introduces."
  (if-let* ((dir (tuned-doctor--user-folder)))
      (tuned-doctor--result 'ok "user folder" dir)
    (tuned-doctor--result
     'ok "user folder" "not used yet; personal.el comes from the distro")))

(defun tuned-doctor--check-elc ()
  "Check for stale or orphaned .elc files in lisp/."
  (let* ((root (tuned-doctor--distro-root))
         (lisp (and root (expand-file-name "lisp/" root))))
    (cond
     ((not (and lisp (file-directory-p lisp)))
      (tuned-doctor--result 'fail "byte code" "lisp/ is missing"
                            "run tuned install"))
     (t
      (let* ((files (directory-files lisp t "\\.elc\\'"))
             (bad (seq-filter
                   (lambda (elc)
                     (let ((src (string-remove-suffix "c" elc)))
                       (or (not (file-exists-p src))
                           (file-newer-than-file-p src elc))))
                   files)))
        (if bad
            (tuned-doctor--result
             'fail "byte code" (format "%d stale or orphan .elc" (length bad))
             "run tuned sync")
          (tuned-doctor--result 'ok "byte code" "no stale .elc")))))))

(defun tuned-doctor--check-autoloads ()
  "Check that the generated autoloads are current."
  (let* ((root (tuned-doctor--distro-root))
         (lisp (and root (expand-file-name "lisp/" root)))
         (autoloads (and lisp (expand-file-name "tuned-autoloads.el" lisp))))
    (cond
     ((not (and autoloads (file-exists-p autoloads)))
      (tuned-doctor--result 'fail "autoloads" "tuned-autoloads.el is missing"
                            "run tuned sync"))
     (t
      (let* ((sources (directory-files lisp t "\\.el\\'"))
             (newer (seq-find (lambda (src) (file-newer-than-file-p src autoloads))
                              sources)))
        (if newer
            (tuned-doctor--result
             'fail "autoloads"
             (format "older than %s" (file-name-nondirectory newer))
             "run tuned sync")
          (tuned-doctor--result 'ok "autoloads" "current")))))))

(defun tuned-doctor--check-trampolines ()
  "Check the cached subr trampolines."
  (if (not (native-comp-available-p))
      (tuned-doctor--result 'ok "trampolines" "not needed without native compilation")
    (let* ((load-path (bound-and-true-p native-comp-eln-load-path))
           (version-dir (bound-and-true-p comp-native-version-dir))
           (dir (and load-path version-dir
                     (expand-file-name version-dir (car load-path))))
           (count (and dir (file-directory-p dir)
                       (length (directory-files
                                dir nil "\\`subr--trampoline-")))))
      (if (and count (> count 0))
          (tuned-doctor--result 'ok "trampolines" (format "%d cached" count))
        (tuned-doctor--result 'warn "trampolines" "none cached"
                              "run tuned sync")))))

(defun tuned-doctor--check-packages-lock ()
  "Compare installed package versions with the profile's packages.lock."
  (let ((lock (expand-file-name "packages.lock" user-emacs-directory)))
    (if (not (file-exists-p lock))
        (tuned-doctor--result 'warn "packages.lock" "not written yet"
                              "run tuned sync")
      (let (drift)
        (with-temp-buffer
          (insert-file-contents lock)
          (goto-char (point-min))
          (while (re-search-forward "^\\([^ \t]+\\)[ \t]+\\([^ \t\n]+\\)$" nil t)
            (let* ((name (intern (match-string 1)))
                   (want (match-string 2))
                   (desc (cadr (assq name package-alist)))
                   (have (and desc (package-version-join
                                    (package-desc-version desc)))))
              (unless (equal want have)
                (push (format "%s %s (have %s)"
                              name want (or have "missing"))
                      drift)))))
        (if drift
            (tuned-doctor--result
             'fail "packages" (string-join (nreverse drift) ", ")
             "run tuned sync")
          (tuned-doctor--result 'ok "packages" "locked versions match"))))))

(defun tuned-doctor--check-startup-log ()
  "Report the last slow start, if one was recorded."
  (let ((log (expand-file-name "startup-slow.log" user-emacs-directory)))
    (if (not (file-exists-p log))
        (tuned-doctor--result 'ok "slow start" "none logged")
      (with-temp-buffer
        (insert-file-contents log nil 0 200)
        (goto-char (point-min))
        (tuned-doctor--result
         'warn "slow start" (string-trim (buffer-substring
                                          (point-min) (line-end-position)))
         (format "inspect %s" log))))))

(defun tuned-doctor--run ()
  "Run every check and return the results."
  (append
   (list (tuned-doctor--check-version))
   (tuned-doctor--check-emacs)
   (list (tuned-doctor--check-programs)
         (tuned-doctor--check-keyring)
         (tuned-doctor--check-stubs)
         (tuned-doctor--check-user-folder)
         (tuned-doctor--check-elc)
         (tuned-doctor--check-autoloads)
         (tuned-doctor--check-trampolines)
         (tuned-doctor--check-packages-lock)
         (tuned-doctor--check-startup-log))))

(defun tuned-doctor--render (results)
  "Render RESULTS as one line per check."
  (mapconcat
   (lambda (result)
     (let* ((status (plist-get result :status))
            (title (plist-get result :title))
            (detail (plist-get result :detail))
            (fix (plist-get result :fix))
            (label (pcase status ('ok "ok") ('warn "warn") (_ "FAIL"))))
       (concat (format "%-5s %s" label title)
               (if detail (format ": %s" detail) "")
               (if (and fix (not (eq status 'ok))) (format "  -> %s" fix) ""))))
   results "\n"))

(defun tuned-doctor-report ()
  "Return the full doctor report as a string."
  (concat (format "tuned-emacs %s\n\n" (tuned-doctor--version-string))
          (tuned-doctor--render (tuned-doctor--run))
          "\n"))

;;;###autoload
(defun tuned-doctor ()
  "Show a health report for this tuned-emacs installation."
  (interactive)
  (let ((buffer (get-buffer-create "*tuned-doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (tuned-doctor-report))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun tuned-doctor-batch ()
  "Print the doctor report; exit non-zero when a check fails."
  (let* ((results (tuned-doctor--run))
         (report (concat (format "tuned-emacs %s\n\n"
                                 (tuned-doctor--version-string))
                         (tuned-doctor--render results)
                         "\n")))
    (princ report)
    (kill-emacs (if (seq-some #'tuned-doctor--failure-p results) 1 0))))

(provide 'tuned-doctor)
;;; tuned-doctor.el ends here
