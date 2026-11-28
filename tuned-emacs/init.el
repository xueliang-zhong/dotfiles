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
    persistent-scratch
    pyim pyim-basedict posframe))       ; Chinese input inside Emacs

(dolist (pkg xl-packages) (add-to-list 'package-selected-packages pkg))

;; Activate installed packages: load-path only, no archive reads
(package-activate-all)

;; Our modules, and the example's helpers, go on the load path before the
;; install path, which lives in lisp/xl-bootstrap.el.
(add-to-list 'load-path (expand-file-name "tuned-emacs/lisp" xl-dotfiles))
(add-to-list 'load-path (expand-file-name "tuned-emacs/example" xl-dotfiles))

;; The install path: refresh the archives when a selected package is missing,
;; fetch and check packages in parallel, retry without signatures if gpg is
;; unusable, and install the packages that live only in Git.  Details in
;; lisp/xl-bootstrap.el.
(require 'xl-bootstrap)
(xl-bootstrap)

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
(autoload 'xl-async-native-compile "xl-native-compile" nil t)
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
