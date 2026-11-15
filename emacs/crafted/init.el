;;; init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-
;; Selects Crafted modules and packages, then loads the personal config.
;; Personal habits belong in emacs/xueliang.el, not here.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

(load (expand-file-name "modules/crafted-init-config" xl-crafted-home))

;; => Packages: Crafted's sets plus `xl-packages' (first form of xueliang.el)
(defvar xl-personal-file (expand-file-name "emacs/xueliang.el" xl-dotfiles))
(require 'crafted-evil-packages)
(require 'crafted-org-packages)
(with-temp-buffer
  (insert-file-contents xl-personal-file)
  (eval (read (current-buffer)) t))     ; defines `xl-packages'
(dolist (pkg xl-packages)
  (add-to-list 'package-selected-packages pkg))
(when (< emacs-major-version 30)        ; which-key is built in from Emacs 30
  (add-to-list 'package-selected-packages 'which-key))
(package-install-selected-packages :noconfirm)

;; => Modules
(setq evil-collection-key-blacklist '("SPC")) ; SPC stays the leader everywhere
(require 'crafted-defaults-config)
(require 'crafted-evil-config)
(require 'crafted-org-config)
(require 'crafted-startup-config)
(when (< emacs-major-version 30)
  (which-key-mode 1))

;; => Crafted defaults that clash with my habits
(customize-set-variable 'evil-want-C-h-delete nil) ; C-h stays the help prefix
(with-eval-after-load 'ispell                      ; no spell checking everywhere
  (remove-hook 'text-mode-hook #'flyspell-mode)
  (remove-hook 'prog-mode-hook #'flyspell-prog-mode))

;; => Leader key
(defvar-keymap xl-leader-map :doc "Keys under SPC.")
(evil-define-key '(normal visual motion) 'global (kbd "SPC") xl-leader-map)

;; => Personal config
(load xl-personal-file)
