;;; init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-
;; Target: Emacs 31 (30 is the floor: built-in which-key, `restart-emacs').
(when (< emacs-major-version 30)
  (error "This profile needs Emacs 30+, this is %s" emacs-version))
;; Selects Crafted modules and packages, then loads the personal config.
;; Personal habits belong in emacs/xueliang.el, not here.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

(load (expand-file-name "modules/crafted-init-config" xl-crafted-home))

;; => Packages: Crafted's sets plus `xl-packages' (first form of xueliang.el)
(defvar xl-personal-file (expand-file-name "emacs/xueliang.el" xl-dotfiles))
(require 'crafted-completion-packages) ; vertico, consult, orderless, corfu, cape...
(require 'crafted-evil-packages)
(require 'crafted-org-packages)
(with-temp-buffer
  (let ((coding-system-for-read 'utf-8)) ; never guess: xueliang.el is UTF-8
    (insert-file-contents xl-personal-file))
  (eval (read (current-buffer)) t))     ; defines `xl-packages'
(dolist (pkg xl-packages)
  (add-to-list 'package-selected-packages pkg))
;; Spacemacs habits that Crafted does not ship (see spacemacs-keys.el)
(dolist (pkg '(evil-escape evil-surround winum avy))
  (add-to-list 'package-selected-packages pkg))
(package-install-selected-packages :noconfirm)

;; => Modules
(setq evil-collection-key-blacklist '("SPC" ",")) ; leaders stay leaders everywhere
(require 'crafted-defaults-config)
(require 'crafted-completion-config)
(require 'crafted-evil-config)
(require 'crafted-org-config)
(require 'crafted-startup-config)

;; => Crafted defaults that clash with my habits
(customize-set-variable 'evil-want-C-h-delete nil) ; C-h stays the help prefix
(with-eval-after-load 'ispell                      ; no spell checking everywhere
  (remove-hook 'text-mode-hook #'flyspell-mode)
  (remove-hook 'prog-mode-hook #'flyspell-prog-mode))

;; => Spacemacs key habits: SPC tree, M-m, `,' major-mode leader, fd, surround
(load (expand-file-name "emacs/spacemacs-keys.el" xl-dotfiles))

;; => Personal config
(load xl-personal-file)

;; => Keep `package-selected-packages' in sync with `xl-packages'
;; Loading a theme replays Customize's saved value for the variable (the
;; `user' theme), which would drop packages added above.  Re-apply the list,
;; and do it again after each theme switch, so `package-autoremove' is safe.
(defun xl-refresh-selected-packages (&rest _)
  "Add every package in `xl-packages' to `package-selected-packages'."
  (dolist (pkg xl-packages)
    (add-to-list 'package-selected-packages pkg))
  ;; Update the user-theme value as well: a later `load-theme' restores it.
  (customize-set-variable 'package-selected-packages package-selected-packages))

(xl-refresh-selected-packages)
(add-hook 'enable-theme-functions #'xl-refresh-selected-packages)
