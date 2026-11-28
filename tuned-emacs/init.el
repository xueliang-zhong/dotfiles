;;; init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-

;; ============================================================
;; Emacs Version and Basics
;; ============================================================
;; Target Emacs 31; 30 is the floor (built-in which-key, `restart-emacs').
;; Stop early with a clear message instead of failing halfway through.
(when (< emacs-major-version 30)
  (error "This profile needs Emacs 30+, this is %s" emacs-version))

(require 'cl-lib)

;; The distribution version lives in VERSION; phase 3 moves this to
;; lisp/tuned-core.el.
(defconst tuned-version
  (string-trim
   (with-temp-buffer
     (insert-file-contents (expand-file-name "tuned-emacs/VERSION" tuned-dotfiles))
     (buffer-string)))
  "Version of this tuned-emacs distribution.")

(defun tuned-version ()
  "Show the version of this tuned-emacs distribution."
  (interactive)
  (message "tuned-emacs %s" tuned-version))

;; Customize writes into its own file, never into this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil :nomessage))

;; Crafted's base: puts its modules/ folder on the load path.
;; `crafted-emacs-home' is set first so Crafted skips its `vc-find-root' lookup.
(defvar crafted-emacs-home tuned-crafted-home)
(load (expand-file-name "modules/crafted-init-config" tuned-crafted-home))

;; ============================================================
;; Packages - install everything before any module needs it
;; ============================================================
;; Crafted's package sets
(require 'crafted-completion-packages) ; vertico, consult, orderless, corfu, cape
(require 'crafted-evil-packages)
(require 'crafted-org-packages)

;; My extra packages
(defconst tuned-packages
  '(evil-escape evil-surround winum avy yasnippet yasnippet-snippets
    magit evil-org org-superstar golden-ratio spacemacs-theme doom-themes
    just-mode lua-mode graphviz-dot-mode markdown-mode diff-hl
    persistent-scratch
    pyim pyim-basedict posframe))       ; Chinese input inside Emacs

(dolist (pkg tuned-packages) (add-to-list 'package-selected-packages pkg))

;; Activate installed packages: load-path only, no archive reads
(package-activate-all)

;; Our modules, and the example's helpers, go on the load path before the
;; install path, which lives in lisp/tuned-bootstrap.el.
(add-to-list 'load-path (expand-file-name "tuned-emacs/lisp" tuned-dotfiles))
(add-to-list 'load-path (expand-file-name "tuned-emacs/example" tuned-dotfiles))

;; The install path: refresh the archives when a selected package is missing,
;; fetch and check packages in parallel, retry without signatures if gpg is
;; unusable, and install the packages that live only in Git.  Details in
;; lisp/tuned-bootstrap.el.
(require 'tuned-bootstrap)
(tuned-bootstrap)

;; Autoloads are generated from the `;;;###autoload' cookies in lisp/ by
;; `bin/tuned sync' (install and sync run it).  Loading the one generated
;; file keeps the commands' definitions and their autoloads in step.
(load (expand-file-name "tuned-emacs/lisp/tuned-autoloads" tuned-dotfiles) nil t)

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
;; also work when installed on their own.  The paths were added above, before
;; the install path.
;; tuned-key-bindings: SPC tree, M-m, `,' major-mode leader, fd, surround.
;; personal.el loads last, so its bindings win over the Spacemacs defaults.
(require 'tuned-key-bindings)
;; `tuned-dired' loads on first use, so startup does not pull in `dired',
;; `dired-x' and `evil-collection-dired'; its autoloads come from
;; tuned-autoloads.
(require 'tuned-git-status)
(require 'tuned-git-commands)
;; `tuned-org-babel-async' only advises `ob-core' and binds a key in `org-mode-map';
;; deferring it until `org' loads keeps `ansi-color' out of startup.
(with-eval-after-load 'org
  (require 'tuned-org-babel-async))
(require 'tuned-consult-spinner) ; consult's async indicator, animated while a list runs
;; perfetto, the asynchronous compile and package-install helpers load on first
;; use; their autoloads come from tuned-autoloads.
;; tuned-eln-opt: once per session, after five seconds of idle time, a
;; child Emacs native-compiles what your history uses (it exits at once
;; when nothing is left).
;; M-x tuned-eln-opt-start runs it now.  See doc/tuned-eln-opt.md.
(require 'tuned-eln-opt)
(tuned-eln-opt-arm)
;; No extension: `load' finds personal.el.  It is never byte-compiled, so there
;; is no .elc that can go stale (its header has a no-byte-compile cookie).
;; The profile stub may set `tuned-load-example' (crafted-install with TUNED_EXAMPLE=1)
;; to load the author's example/xueliang.el here instead of personal.el.
(defvar tuned-load-example nil
  "Non-nil loads example/xueliang.el instead of personal.el.")
(load (expand-file-name (if tuned-load-example
                            "tuned-emacs/example/xueliang"
                          "tuned-emacs/personal")
                        tuned-dotfiles))

;; ============================================================
;; Keep the Package List Complete
;; ============================================================
;; Loading a theme also restores the package list saved in custom.el, which
;; can be older and miss packages added above.  Put mine back now and after
;; every theme switch, so `package-autoremove' never deletes a package I use.
(defun tuned-refresh-selected-packages (&rest _)
  "Add every package in `tuned-packages' to `package-selected-packages'."
  (dolist (pkg tuned-packages)
    (add-to-list 'package-selected-packages pkg))
  ;; Also record it as the saved value, which the next theme load restores
  (customize-set-variable 'package-selected-packages package-selected-packages))

(tuned-refresh-selected-packages)
(add-hook 'enable-theme-functions #'tuned-refresh-selected-packages)
