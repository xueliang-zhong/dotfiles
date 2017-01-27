(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'evil)
(evil-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)

(setq-default xueliang-leader-key "<SPC>")

(evil-leader/set-leader xueliang-leader-key)

(evil-leader/set-key
  "]" 'helm-etags-select-android-art
  "a" 'helm-do-grep-ag
  "b" 'helm-buffers-list
  "f" 'fiplr-find-file
  "g" 'helm-grep-do-git-grep
  "i" 'helm-semantic-or-imenu
  "n" 'neotree-toggle
  "r" 'helm-recentf
  "s" 'helm-swoop
  "u" 'universal-argument
  "x" 'helm-M-x  ;; for easier use in the dark
)

; highlight like vim, C-x SPC to remove all persistant search highlights.
(require 'highlight)
(require 'evil-search-highlight-persist)
;(global-evil-search-highlight-persist nil) ;; color is not so great.

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

; company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)  ;; instead of any key bindings for company-complete.
(setq company-minimum-prefix-length 1)

; default theme good themes: tango-dark, zenburn, monokai
(load-theme 'tango-dark t)

(require 'rainbow-delimiters)
(rainbow-delimiters-mode)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'powerline)
;(powerline-center-evil-theme)
;(powerline-evil-vim-color-theme)
(powerline-evil-center-color-theme)
(set-face-attribute 'mode-line nil
                    :foreground "White"
                    ;:background "RoyalBlue"
                    :background "BlueViolet"
                    :box nil)
(setq powerline-default-separator 'contour)

(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

(require 'guide-key)
(setq guide-key/guide-key-sequence (list xueliang-leader-key "C-h" "C-x"))
(setq guide-key/idle-delay 0.1)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)  ; Enable guide-key-mode

(require 'neotree)
(setq neo-smart-open 1)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q")   'neotree-hide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)

(setq helm-split-window-in-side-p t) ; open helm buffer inside current window, not occupy whole other window
(setq helm-echo-input-in-header-line t)
(setq helm-mode-fuzzy-match t)
(setq helm-ff-file-name-history-use-recentf t)

;; fuzzy matching settings in helm
(setq helm-M-x-fuzzy-match        nil
      helm-buffers-fuzzy-matching t
      helm-semantic-fuzzy-match   t
      helm-imenu-fuzzy-match      t)

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start for emacsclient
(server-start)

;; requires build emacs with: ./configure --with-x-toolkit=gtk
(set-default-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 140)

; line/column related
(column-number-mode)
(global-linum-mode)
(global-hl-line-mode)
(set-face-background hl-line-face "gray25")

; tabs
(setq tab-width 2)
(setq indent-tabs-mode nil)
(defun tab-as-two-spaces() (interactive) (insert "  "))
(global-set-key (kbd "TAB") 'tab-as-two-spaces)

; disable menu-bar-mode
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1) ;; powerline already shows a mini scroll bar

; show match parentheses
(show-paren-mode)

; wrap long lines
(set-default 'truncate-lines t)

; diable bold font
(set-face-bold 'bold t)

; mouse
(xterm-mouse-mode 1)

; scroll
(setq scroll-step 1)

; don't put any backup files in my directory.
(setq backup-directory-alist `(("." . "~/.saves")))

; mark long lines in column, for example: (column-marker-1 100)
(require 'column-marker)

(require 'whitespace)
(global-whitespace-mode +1)
(setq whitespace-line-column 101) ;; limit line length
(setq whitespace-style '(face trailing lines-tail tabs))
(add-hook 'prog-mode-hook 'whitespace-mode)

; '_' as part of word
(add-hook 'c-mode-hook    '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'java-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq android-root "~/workspace/aosp")
(setq android-art  (concat android-root "/art/"))
(setq android-vixl (concat android-root "/external/vixl/src/"))
(setq android-art-tags  (concat android-art  "TAGS"))
(setq android-vixl-tags (concat android-vixl "TAGS"))
(setq tags-table-list (list android-art-tags android-vixl-tags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default shell-output-buffer-name "*Shell Command Output*")

; Help to make upate TAGS of the project I'm working on easier.
(defun xueliang-update-tags-art ()
  "update etags/TAGS of Android ART project" (interactive)
  (cd android-art) (shell-command "ctag"))

; Help to make code reviews easier; requires cpplint.py in $PATH
(defun xueliang-art-cpplint ()
  "invokes AOSP/art/tools/cpplint.py on current buffer" (interactive)
  (setq-local cpplint-cmd-and-options "cpplint.py --filter=-whitespace/line_length,-build/include ")
  (shell-command (concat cpplint-cmd-and-options (buffer-file-name)))
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-very-bottom) (compilation-mode))

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
  "run git status" (interactive) (shell-command "git status"))

(defun xueliang-glog ()
  "run git log" (interactive)
  (shell-command "git log -n 100")
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-far-right) (evil-end-of-line))

(defun xueliang-gdiff-current-buffer ()
  "run git diff on current buffer;
   use C-M-i to browse diff hunks; C-c C-c to jump to source code." (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command (concat "git diff " (buffer-file-name)))
  (if (= (buffer-size (switch-to-buffer-other-window shell-output-buffer-name)) 0)
    (kill-buffer-and-window) ;; kill the buffer that we just switched to, should be the shell output buffer window.
    (evil-window-move-far-right) (diff-mode) (toggle-truncate-lines 1)))

(defun xueliang-gdiff-revision-at-point ()
  "run 'git diff' using the revision number at point.
   workflow: get git revision in output, browse revisions, apply this function." (interactive)
  (if (null (buffer-file-name (current-buffer)))
      (funcall (lambda () ;; already in shell command output buffer.
                 (evil-end-of-line)
                 (shell-command (message "git diff %s " (thing-at-point 'word)))
                 (evil-window-move-far-right) (diff-mode) (toggle-truncate-lines 1)))
      (funcall (lambda () ;; in some other file.
                 (xueliang-glog) (message "try apply this function in glog.")))))

(defun xueliang-gshow-revision-at-point()
  "run 'git show' using the revision number at point.
   workflow: get git revision in output, browse revisions, apply this function." (interactive)
   (if (null (buffer-file-name (current-buffer)))
       (funcall (lambda () ;; already in shell command output buffer.
                  (shell-command (message "git show %s " (thing-at-point 'word)))
                  (evil-window-move-far-right) (diff-mode) (toggle-truncate-lines 1)))
       (funcall (lambda () ;; in some other file.
                  (xueliang-glog) (message "try apply this function in glog.")))))

(defun xueliang-gread-current-buffer ()
  "run git checkout on current buffer" (interactive)
  (shell-command (concat "git checkout " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm)
  (message "git checkout: " (buffer-file-name)))

(defun xueliang-gwrite-current-buffer ()
  "run git add on current buffer" (interactive)
  (shell-command (concat "git add " (buffer-file-name)))
  (xueliang-gread-current-buffer) ;; gread it again to refresh git-gutter.
  (message "git add: %s" (buffer-file-name)))

(defun xueliang-gblame-current-buffer ()
  "run git blame on current buffer, esp. current line" (interactive)
  (setq-local gblame-line (line-number-at-pos))
  (shell-command (concat "git blame " (buffer-file-name)))
  (goto-line gblame-line (switch-to-buffer-other-window shell-output-buffer-name))
  (evil-window-move-far-left) (hl-line-mode) (toggle-truncate-lines 1))

; for switching between .h/.cc files.
(defun xueliang-A-h-cc-files-switcher ()
  "similiar to A plugin for VIM, just type 'M-x A' in helm" (interactive)
  (helm-find-files-1 (car (split-string (buffer-file-name) "\\."))))

; for tag search in android-art project.
(defun helm-etags-select-android-art() (interactive)
   (cd android-art) (helm-etags-select t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nice M-x
(global-set-key (kbd "M-x") 'helm-M-x)

; vim way of page up, the original universal argument is <leader>-u.
(global-set-key (kbd "C-u") 'evil-scroll-page-up)

; Better than (describe-function) and (describe-variable)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h v") 'helm-apropos)

; describe key-bindings
(global-set-key (kbd "C-h b") 'helm-descbinds)

; good way to learn all completion functions.
(global-set-key (kbd "M-/") 'hippie-expand)

; <f1> .. <f4> :
; <f5> .. <f8> : with in buffer : coding, development, tags,
; <f9> .. <f12>: with in project: buffer, find in project, related.

(global-set-key (kbd "<f5>")  'helm-swoop)                    ; find in current buffer using helm-swoop.
(global-set-key (kbd "<f6>")  'helm-semantic-or-imenu)        ; imenu in current file.
(global-set-key (kbd "<f8>")  'helm-etags-select-android-art) ; find tag and jump to tag in android-art.

(global-set-key (kbd "<f9>")  'helm-buffers-list)     ; buffer emnu.
(global-set-key (kbd "<f10>") 'fiplr-find-file)       ; find file in project.
(global-set-key (kbd "<f11>") 'helm-grep-do-git-grep) ; increment grep using git-grep.
(global-set-key (kbd "<f12>") 'helm-do-grep-ag)       ; grep current word in project.

; in org mode, mark a line as +strike-through+
(defun xueliang-ctrl-s() "in org mode, mark a line as +strike-through+" (interactive)
       (evil-insert-line 0) (insert "+")
       (evil-append-line 0) (insert "+")
       (evil-force-normal-state))
(global-set-key (kbd "C-s") 'xueliang-ctrl-s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang good tips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * C-u ARG M-x func RET
;; * package install RET command-log-mode, toggle-command-log/buffer
;; * M-x ansi-term
;; * Use compilation mode to parse command line outputs, e.g. cpplint's output.
;; * emacs -nw
;; * M-x server-start
;; * emacsclient -nw
;; * helm-show-kill-ring
;; * evil mode,  helm, org mode, company mode.
;; * helm-swoop
;; * (ielm) inferior-emacs-lisp-mode
;; * company-mode: M-n/p to select, TAB to complete the common part.
;; * helm: C-n/p to move up/down in helm buffer-menu-delete-backwards
;; * M-n/p is usually going through history items.

;; https://github.com/emacs-tw/awesome-emacs
;; http://www.john2x.com/emacs.html
;; https://github.com/caiorss/Emacs-Elisp-Programming
;; https://github.com/emacs-helm/helm/wiki
;; http://cestlaz.github.io/stories/emacs/
;; https://tuhdo.github.io/helm-intro.html
;; https://github.com/jwiegley/use-package
;; https://github.com/kai2nenobu/guide-key
