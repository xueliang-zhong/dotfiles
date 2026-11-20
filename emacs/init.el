;;; init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-

;; ============================================================
;; Show Maximized Screen Early
;; ============================================================
(set-frame-parameter nil 'fullscreen 'maximized)

;; ============================================================
;; Emacs Version and Basics
;; ============================================================
;; Target Emacs 31; 30 is the floor (built-in which-key, `restart-emacs').
;; Stop early with a clear message instead of failing halfway through.
(when (< emacs-major-version 30)
  (error "This profile needs Emacs 30+, this is %s" emacs-version))

;; Customize writes into its own file, never into this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil :nomessage))

;; Crafted's base: puts its modules/ folder on the load path
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
    just-mode lua-mode graphviz-dot-mode markdown-mode diff-hl))

(dolist (pkg xl-packages) (add-to-list 'package-selected-packages pkg))

;; Activate installed packages: load-path only, no archive reads
(package-activate-all)

;; Read archives and install only when something on the list is missing
(require 'seq)
(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-install-selected-packages :noconfirm))

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
;; spacemacs-keys.el: SPC tree, M-m, `,' major-mode leader, fd, surround.
;; xueliang.el loads last, so its bindings win over the Spacemacs defaults.
(load (expand-file-name "emacs/spacemacs-keys.el" xl-dotfiles))
(load (expand-file-name "emacs/xueliang.el" xl-dotfiles))

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
