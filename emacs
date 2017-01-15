; 2017 the emacs year for me.

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

(set-face-attribute 'default nil :height 160)

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

; show match parentheses
(show-paren-mode)

; wrap long lines
(toggle-truncate-lines 1)

; diable bold font
(set-face-bold 'bold nil)

; don't put any backup files in my directory.
(setq backup-directory-alist "/tmp/")

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

(setq android-root "~/workspace/aosp")
(setq android-art  (concat android-root "/art"))
(setq android-vixl (concat android-root "/external/vixl/src"))
(setq android-art-tags  (concat android-art  "/TAGS"))
(setq android-vixl-tags (concat android-vixl "/TAGS"))
(setq tags-table-list (list android-art-tags android-vixl-tags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Help to make upate TAGS of the project I'm working on easier.
(defun xueliang-update-tags-art ()
  "update etags/TAGS of Android ART project" (interactive)
  (cd android-art) (shell-command "ctag"))

; Help to make code reviews easier; requires cpplint.py in $PATH
(defun xueliang-art-cpplint ()
  "invokes AOSP/art/tools/cpplint.py on current buffer" (interactive)
  (setq-local cpplint-cmd-and-options "cpplint.py --filter=-whitespace/line_length,-build/include ")
  (shell-command (concat cpplint-cmd-and-options (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (compilation-mode))

; Helper to cd to directory of current buffer/file.
(defun xueliang-cd-current-buffer-directory ()
  "cd to directory of current buffer/file." (interactive)
  (cd (file-name-directory buffer-file-name))
  (message "pwd: %s" (file-name-directory buffer-file-name)))

; Git helper functions/commands.
(defun xueliang-gcommit-current-file ()
  "run git commit on current file.
   *** HANDLE WITH CARE !!! only used after gwrite & gstatus ***" (interactive)
  (shell-command (message "git commit -m \"improve %s\"" (file-name-nondirectory buffer-file-name))))

(defun xueliang-gstatus ()
  "run git status" (interactive)
  (shell-command "git status")
  (switch-to-buffer-other-window "*Shell Command Output*"))

(defun xueliang-glog ()
  "run git log" (interactive)
  (shell-command "git log")
  (switch-to-buffer-other-window "*Shell Command Output*"))

(defun xueliang-gdiff-current-buffer ()
  "run git diff on current buffer;
   use C-M-i to browse diff hunks; C-c C-c to jump to source code." (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command (concat "git diff " (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (diff-mode) (toggle-truncate-lines 1))

(defun xueliang-gdiff-revision-at-point ()
  "run git diff using the revision number at point.
   workflow: glog to get revision in output, browse revisions, apply this function." (interactive)
   (if (null (buffer-file-name (current-buffer)))
       (funcall (lambda () ;; already in shell command output buffer.
                  (shell-command (message "git diff %s " (thing-at-point 'word)))
                  (diff-mode) (toggle-truncate-lines 1)))
       (funcall (lambda () ;; in some other file.
                  (xueliang-glog) (message "try apply this function in glog.")))))

(defun xueliang-gwrite-current-buffer ()
  "run git add on current buffer" (interactive)
  (shell-command (concat "git add " (buffer-file-name)))
  (message "git add: %s" (buffer-file-name)))

(defun xueliang-gread-current-buffer ()
  "run git checkout on current buffer" (interactive)
  (shell-command (concat "git checkout " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm)
  (message "git checkout: " (buffer-file-name)))

(defun xueliang-gblame-current-buffer ()
  "run git blame on current buffer, esp. current line" (interactive)
  (setq-local gblame-line (line-number-at-pos))
  (shell-command (concat "git blame " (buffer-file-name)))
  (goto-line gblame-line (switch-to-buffer-other-window "*Shell Command Output*"))
  (hl-line-mode) (toggle-truncate-lines 1))

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
(defun xueliang-helm-etags-select() (interactive) (cd android-art) (helm-etags-select t))
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
;; * Use compilation mode to parse command line outputs, e.g. cpplint's output.

;; https://github.com/emacs-tw/awesome-emacs
;; https://github.com/caiorss/Emacs-Elisp-Programming

