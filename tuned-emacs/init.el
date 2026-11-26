;;; init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-

;; ============================================================
;; Emacs Version and Basics
;; ============================================================
;; Target Emacs 31; 30 is the floor (built-in which-key, `restart-emacs').
;; Stop early with a clear message instead of failing halfway through.
(when (< emacs-major-version 30)
  (error "This profile needs Emacs 30+, this is %s" emacs-version))

(require 'cl-lib)

;; Customize writes into its own file, never into this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil :nomessage))

;; Crafted's base: puts its modules/ folder on the load path.
;; `crafted-emacs-home' is set first so Crafted skips its `vc-find-root' lookup.
(defvar crafted-emacs-home xl-crafted-home)
(load (expand-file-name "modules/crafted-init-config" xl-crafted-home))

;; ============================================================
;; Packages - install everything before any module needs it
;; ============================================================
;; Crafted's package sets
(require 'crafted-completion-packages) ; vertico, consult, orderless, corfu, cape
(require 'crafted-evil-packages)
(require 'crafted-org-packages)

;; My extra packages
(defconst xl-packages
  '(evil-escape evil-surround winum avy yasnippet yasnippet-snippets
    magit evil-org org-superstar golden-ratio spacemacs-theme doom-themes
    just-mode lua-mode graphviz-dot-mode markdown-mode diff-hl
    persistent-scratch))

(dolist (pkg xl-packages) (add-to-list 'package-selected-packages pkg))

;; Activate installed packages: load-path only, no archive reads
(package-activate-all)

;; Read archives and install only when something on the list is missing.
;; This downloads over the network and checks signatures with gpg, all before
;; the first frame is painted, but it only runs when a package is missing.
(require 'seq)

(defvar xl-progress (equal (getenv "XL_PROGRESS") "1")
  "Non-nil when emacs-setup.sh asked for step-by-step progress output.")

(defun xl-report (format-string &rest args)
  "Print a progress line to stdout when `xl-progress' is set."
  (when xl-progress
    (princ (apply #'format format-string args))
    (princ "\n")))

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
Each package is copied as source only: `package.el' would byte-compile it
here, which makes the install slow, and eln-opt compiles it later in the
background (.elc, then .eln where native compilation is available)."
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
            ;; `package--compile' is private; the Emacs 30/31 install path
            ;; calls it only from `package-install-1'.  Check it on upgrade.
            (cl-letf (((symbol-function 'package--compile) #'ignore))
              (package-install pkg)))
        (error
         (xl-report "package %d/%d failed: %s (%s)"
                    index total pkg (error-message-string err)))))))

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
    (xl-report "refreshing package archives ... (this may take a few seconds)")
    (package-refresh-contents)
    ;; Parallel download and gpg check; whatever it cannot finish is left to
    ;; `xl-install-packages' below.
    (add-to-list 'load-path (expand-file-name "tuned-emacs/lisp" xl-dotfiles))
    (require 'xl-package-fetch)
    (xl-fetch-packages (seq-remove #'package-installed-p package-selected-packages))
    (xl-install-packages (seq-remove #'package-installed-p package-selected-packages))
    (when-let* ((still (seq-remove #'package-installed-p package-selected-packages)))
      (message "Package install incomplete (%S); rebuilding the ELPA keyring"
               still)
      (xl-report "rebuilding the ELPA keyring")
      (xl-refresh-package-keyring)
      (xl-report "refreshing package archives again")
      (package-refresh-contents)
      (xl-install-packages (seq-remove #'package-installed-p package-selected-packages))
      (when-let* ((still (seq-remove #'package-installed-p package-selected-packages)))
        ;; Last resort for machines without a usable gpg: download over
        ;; HTTPS without checking the archive signatures for this session.
        (message "Still missing %S; retrying without ELPA signature verification"
                 still)
        (xl-report "retrying without signature verification")
        (let ((package-check-signature nil))
          (package-refresh-contents)
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

(xl-install-missing-packages)

;; ============================================================
;; Crafted Modules - the parts of Crafted I use
;; ============================================================
;; evil-collection binds keys in many modes; keep SPC and `,' out of them
;; so both leaders work everywhere.  Must be set before the evil module.
(setq evil-collection-key-blacklist '("SPC" ","))

(require 'crafted-defaults-config)   ; sane built-in defaults
(require 'crafted-completion-config) ; vertico, consult, corfu, cape
(require 'crafted-evil-config)       ; evil + evil-collection
(require 'crafted-org-config)        ; Org basics
(require 'crafted-startup-config)    ; the home buffer

;; ============================================================
;; Crafted Defaults Overrides
;; ============================================================
(customize-set-variable 'evil-want-C-h-delete nil) ; C-h stays the help prefix
(with-eval-after-load 'ispell                      ; no spell checking everywhere
  (remove-hook 'text-mode-hook #'flyspell-mode)
  (remove-hook 'prog-mode-hook #'flyspell-prog-mode))

;; Consult 3.4 removed `consult-preview-at-point-mode', but Crafted still
;; puts it on `embark-collect-mode-hook'.  That hook then breaks every
;; Embark Collect buffer, including the collect fallback `embark-export'
;; uses for types without a dedicated exporter (e.g. imenu).  Embark-Consult
;; already adds the replacement, `consult--default-completion-list-preview-setup'.
(remove-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;; ============================================================
;; My Layers - Spacemacs keys first, then my own config
;; ============================================================
;; lisp/: modules loaded with `require'.  The ones with package headers
;; also work when installed on their own.
(add-to-list 'load-path (expand-file-name "tuned-emacs/lisp" xl-dotfiles))
;; xl-key-bindings: SPC tree, M-m, `,' major-mode leader, fd, surround.
;; xueliang.el loads last, so its bindings win over the Spacemacs defaults.
(require 'xl-key-bindings)
;; `xl-dired': load on first use so startup does not pull in `dired',
;; `dired-x' and `evil-collection-dired'.
(dolist (cmd '(xl-dired xl-dired-jump xl-dired-open xl-dired-up-directory
               xl-dired-preview xl-dired-preview-quit xl-dired-mode))
  (autoload cmd "xl-dired" nil t))
(autoload 'xl-dired-noselect "xl-dired")
(require 'xl-git-status)
(require 'xl-git-commands)
;; `xl-org-babel-async' only advises `ob-core' and binds a key in `org-mode-map';
;; deferring it until `org' loads keeps `ansi-color' out of startup.
(with-eval-after-load 'org
  (require 'xl-org-babel-async))
(require 'xl-consult-spinner) ; consult's async indicator, animated while a list runs
(dolist (cmd '(xl-perfetto-trace-start xl-perfetto-trace-stop
               xl-perfetto-trace-mark xl-perfetto-trace-open))
  (autoload cmd "xl-perfetto-trace" nil t))
(autoload 'xl-perfetto-trace-function "xl-perfetto-trace")
(autoload 'xl-perfetto-untrace-function "xl-perfetto-trace")
(autoload 'xl-perfetto-with-span "xl-perfetto-trace" nil nil 'macro)
;; eln-opt: once this Emacs has stayed open a while and goes idle, compile in a
;; background batch Emacs (timers only, no hooks): byte-compile what runs
;; interpreted, then the hot files to native code (byte only without
;; libgccjit).  See tuned-emacs/ideas/eln-opt.md.
(require 'eln-opt)
(eln-opt-auto-arm)
;; No extension: `load' then prefers tuned-emacs/xueliang.elc when it is current
;; (see `compile-lisp' in emacs-setup.sh), and the .el otherwise.
(load (expand-file-name "tuned-emacs/xueliang" xl-dotfiles))

;; ============================================================
;; Keep the Package List Complete
;; ============================================================
;; Loading a theme also restores the package list saved in custom.el, which
;; can be older and miss packages added above.  Put mine back now and after
;; every theme switch, so `package-autoremove' never deletes a package I use.
(defun xl-refresh-selected-packages (&rest _)
  "Add every package in `xl-packages' to `package-selected-packages'."
  (dolist (pkg xl-packages)
    (add-to-list 'package-selected-packages pkg))
  ;; Also record it as the saved value, which the next theme load restores
  (customize-set-variable 'package-selected-packages package-selected-packages))

(xl-refresh-selected-packages)
(add-hook 'enable-theme-functions #'xl-refresh-selected-packages)
