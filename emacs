; 2017 the emacs.

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'evil)
(evil-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; line/column related
(column-number-mode)
(global-linum-mode)
(global-hl-line-mode)

; Nice M-x
(global-set-key (kbd "M-x") 'helm-M-x)

; good way to learn all completion functions.
(global-set-key (kbd "M-/") 'hippie-expand)

; something like tagbar/tlist
(global-set-key (kbd "<f7>") 'helm-semantic-or-imenu)

; buffer
(global-set-key (kbd "<f10>") 'helm-buffers-list)

; buffer
(global-set-key (kbd "<f12>") 'helm-do-grep-ag)


; company mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-SPC") 'company-complete)

; disable menu-bar-mode
(menu-bar-mode -1)
(tool-bar-mode -1)

; show match parenthes
(show-paren-mode)

; wrap long lines
(toggle-truncate-lines 1)

; mark long lines in column
(require 'column-marker)
(add-hook 'evil-mode-hook (lambda () (interactive) (column-marker-1 100)))
(require 'whitespace)
(global-whitespace-mode +1)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

; default theme: tango-dark or tango are both OK.
(load-theme 'tango-dark)

;;
;; C-u ARG M-x func RET
;; package install RET command-log-mode, toggle-command-log/buffer
