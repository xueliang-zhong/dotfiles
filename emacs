; 2017 the emacs.

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'evil)
(evil-mode 1)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(rainbow-delimiters-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; line/column related
(column-number-mode)
(global-linum-mode)
; (global-hl-line-mode)

; tabs
(setq tab-width 2)
(setq indent-tabs-mode nil)
(defun tab-as-two-spaces() (interactive) (insert "  "))
(global-set-key (kbd "TAB") 'tab-as-two-spaces)

; disable menu-bar-mode
(menu-bar-mode -1)
(tool-bar-mode -1)

; show match parenthes
(show-paren-mode)

; wrap long lines
(toggle-truncate-lines 1)

; diable bold font
(set-face-bold 'bold nil)

; mark long lines in column
(require 'column-marker)
; (add-hook 'evil-mode-hook (lambda () (interactive) (column-marker-1 100)))

(require 'whitespace)
(global-whitespace-mode +1)
(setq whitespace-line-column 101) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

; '-' as part of word
(add-hook 'c-mode-hook    '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'java-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))

; default theme: tango-dark or tango are both OK.
(load-theme 'tango-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq android-art-root "~/workspace/aosp/art/")

(setq tags-table-list '("~/workspace/aosp/art/TAGS"
                        "~/workspace/aosp/external/vixl/src/TAGS"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Help to make code reviews easier; requires cpplint.py in $PATH
(defun art-cpplint ()
  "invokes AOSP/art/tools/cpplint.py on current buffer" (interactive)
  (shell-command (concat
                  "cpplint.py --filter=-whitespace/line_length,-build/include " (buffer-file-name))))

(defun xueliang-helm-etags-select() (interactive) (cd android-art-root) (helm-etags-select t))

(defun xueliang-gdiff-current-buffer ()
  "run git diff on current buffer; use C-M-i to browse diff hunks." (interactive)
  (shell-command (concat "git diff " (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*"))

(defun xueliang-gwrite-current-buffer ()
  "run git add on current buffer" (interactive)
  (shell-command (concat "git add " (buffer-file-name))))

(defun xueliang-gread-current-buffer ()
  "run git checkout on current buffer" (interactive)
  (shell-command (concat "git checkout " (buffer-file-name))))

(defun xueliang-gblame-current-buffer ()
  "run git blame on current buffer, esp. current line" (interactive)
  (setq gblame-window-line (line-number-at-pos))
  (shell-command (concat "git blame " (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (goto-line gblame-window-line))

(defun xueliang-gstatus ()
  "run git status" (interactive)
  (shell-command "git status"))

(defun xueliang-glog ()
  "run git log" (interactive)
  (shell-command "git log"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nice M-x
(global-set-key (kbd "M-x") 'helm-M-x)

; good way to learn all completion functions.
(global-set-key (kbd "M-/") 'hippie-expand)

; <f1> .. <f4> :
; <f5> .. <f8> : tag, coding, development related
; <f9> .. <f12>: buffer, folder, grep find related.

; find tag and jump to tag
(global-set-key (kbd "<f5>") 'xueliang-helm-etags-select)

; something like tagbar/tlist
(global-set-key (kbd "<f6>") 'helm-semantic-or-imenu)

; buffer
(global-set-key (kbd "<f9>") 'helm-buffers-list)

; find file
(global-set-key (kbd "<f10>") 'helm-find-files)

; increment grep so useful
(global-set-key (kbd "<f11>") 'helm-grep-do-git-grep)

; grep current word
(global-set-key (kbd "<f12>") 'helm-do-grep-ag)

; company mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-SPC") 'company-complete)

;;
;; * C-u ARG M-x func RET
;; * package install RET command-log-mode, toggle-command-log/buffer
;; * M-x ansi-term

;; https://github.com/emacs-tw/awesome-emacs
;; https://github.com/caiorss/Emacs-Elisp-Programming

