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

(column-number-mode)

; Nice M-x
(global-set-key (kbd "M-x") 'helm-M-x)

; good way to learn all completion functions.
(global-set-key (kbd "M-/") 'hippie-expand)

; buffer
(global-set-key (kbd "<f10>") 'buffer-menu)

; (global-set-key (kbd "C-u") 'evil-scroll-page-up)

; line related
(global-linum-mode)
; (global-hl-line-mode)

; disable menu-bar-mode
(menu-bar-mode -1)
(tool-bar-mode -1)

; show match parenthes
(show-paren-mode)

; wrap long lines
(toggle-truncate-lines 1)

; 
(require 'column-marker)
(add-hook 'evil-mode-hook (lambda () (interactive) (column-marker-1 50)))                

(require 'whitespace)
(global-whitespace-mode +1)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;;
;; C-u ARG M-x func RET
;; package install RET command-log-mode, toggle-command-log/buffer

