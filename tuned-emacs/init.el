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
;; personal.el loads last, so its bindings win over the Spacemacs defaults.
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
;; Helpers for asynchronous compiles and package installs; loaded on first use.
(autoload 'xl-async-native-compile "xl-async-evaluate" nil t)
(autoload 'xl-async-evaluate "xl-async-evaluate" nil t)
(autoload 'xl-package-install "xl-package-install" nil t)
;; xl-eln-opt: once per session, after five seconds of idle time, a child Emacs
;; native-compiles what your history uses (it exits at once when nothing is left).
;; M-x xl-eln-opt-start runs it now.  See doc/xl-eln-opt.md.
(require 'xl-eln-opt)
(xl-eln-opt-arm)
;; No extension: `load' finds personal.el.  It is never byte-compiled, so there
;; is no .elc that can go stale (its header has a no-byte-compile cookie).
;; The profile stub may set `xl-load-example' (crafted-install with XL_EXAMPLE=1)
;; to load the author's example/xueliang.el here instead of personal.el.
(defvar xl-load-example nil
  "Non-nil loads example/xueliang.el instead of personal.el.")
(load (expand-file-name (if xl-load-example
                            "tuned-emacs/example/xueliang"
                          "tuned-emacs/personal")
                        xl-dotfiles))

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
