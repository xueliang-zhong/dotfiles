(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/xueliang/")
(load "xueliang-git.elc");

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============evil-config=============())
(require 'evil)
(evil-mode 1)

;; Evil Mode everywhere.
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

(require 'evil-leader)
(global-evil-leader-mode)

(setq evil-shift-width 2)

(setq-default xueliang-leader-key "<SPC>")
(evil-leader/set-leader xueliang-leader-key)
(evil-leader/set-key
  "]" 'helm-etags-select-android-art
  "a" 'xueliang-ag-search-in-project
  "b" 'helm-for-files
  "e" 'xueliang-eshell
  "f" 'fiplr-find-file
  "g" 'helm-grep-do-git-grep
  "i" 'helm-semantic-or-imenu
  "n" 'helm-for-files
  "r" 'helm-recentf
  "s" 'helm-swoop
  "u" 'universal-argument
  "x" 'helm-M-x  ;; for easier use in the dark
)

(setq evil-mode-line-format 'before)

;; use helm-swoop instead of vim style */# find.
(define-key evil-normal-state-map (kbd "*") 'helm-swoop)
(define-key evil-normal-state-map (kbd "#") 'helm-swoop)

; highlight like vim, C-x SPC to remove all persistant search highlights.
;(require 'highlight)
;(require 'evil-search-highlight-persist)
;(global-evil-search-highlight-persist nil) ;; color is not so great.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============helm-config=============())
(require 'helm)
(require 'helm-config)

(setq helm-split-window-in-side-p t) ; open helm buffer inside current window
(setq helm-echo-input-in-header-line t)
(setq helm-mode-fuzzy-match nil)
(setq helm-ff-file-name-history-use-recentf nil)

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf
        helm-source-bookmarks
        helm-source-file-cache
        helm-source-files-in-current-dir))

;; fuzzy matching settings in helm
(setq helm-M-x-fuzzy-match        nil
      helm-buffers-fuzzy-matching t
      helm-semantic-fuzzy-match   t
      helm-imenu-fuzzy-match      t)

;; rebind tab to the next/previous line in helm window, same behavior as company.
(define-key helm-map (kbd "<tab>")     'helm-next-line)
(define-key helm-map (kbd "<backtab>") 'helm-previous-line)  ;; Shift-Tab is <backtab>
(define-key helm-map (kbd "M-x") 'helm-select-action) ;; list actions using M-x inside helm.

(setq helm-autoresize-max-height 20)
(setq helm-autoresize-min-height 15)
(helm-autoresize-mode 1)

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============company-config=============())

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)  ;; instead of any key bindings for company-complete.
(setq company-minimum-prefix-length 2)

; tab to select in company.
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB")   'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart mode-line config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============smart-mode-line-config=============())

;; smart mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(smart-mode-line-enable)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/dotfiles/" ":dotfiles:") t)

(add-to-list 'sml/replacer-regexp-list '("^~/workspace/aosp/" ":aosp:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/linaro/" ":linaro:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/Linaro_Android_Master/" ":linaro:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/" ":workspace:") t)

(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/aosp/" ":aosp:") t)
(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/linaro/" ":linaro:") t)
(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/Linaro_Android_Master/" ":linaro:") t)
(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/" ":workspace:") t)

(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/aosp/external/vixl" ":aosp-vixl:") t)
(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/linaro/external/vixl/" ":linaro-vixl:") t)
(add-to-list 'sml/replacer-regexp-list '("^/data/workspace/Linaro_Android_Master/external/vixl/" ":linaro-vixl:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/aosp/external/vixl" ":aosp-vixl:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/linaro/external/vixl/" ":linaro-vixl:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/workspace/Linaro_Android_Master/external/vixl/" ":linaro-vixl:") t)

;; show which function on mode-line.
(which-function-mode 1)

;; always show which git branch I'm in.
(add-hook 'prog-mode-hook '(lambda () (vc-mode-line (buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============misc-modes-config=============())
;; helm for M-x, ivy for evil/vi ex command line.
;; <tab> to trigger in ex command line.
(ivy-mode 1)

; default theme good themes: tango-dark, zenburn, monokai, wombat
(if window-system
  (load-theme 'tango-dark t)
  (load-theme 'wombat t))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'git-gutter-fringe+)
(global-git-gutter+-mode)

(require 'guide-key)
(setq guide-key/guide-key-sequence (list xueliang-leader-key "C-h" "C-x" "C-x c"))
(setq guide-key/idle-delay 0.1)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)  ; Enable guide-key-mode

;(require 'neotree)
(setq neo-smart-open 1)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q")   'neotree-hide)

; said to make emacs faster
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============global-settings=============())

;; start server for emacsclient
(unless (server-running-p)
  (server-start))

;; make things faster?
(setq echo-keystrokes 0.1)

;; hide welcome screen
(setq inhibit-splash-screen t)

;; requires build emacs with: ./configure --with-x-toolkit=gtk
(set-default-font "DejaVu Sans Mono")

;; set default font for emacs --daemon / emacsclient
;; but I found it has some problem in showing fonts. A workaround is to eval following code here.
;;(setq default-frame-alist '((font . "DejaVu Sans Mono")))

(set-face-attribute 'default nil :height 130)

; line/column related
(column-number-mode)
(require 'hl-line+)
(global-hl-line-mode)
(set-face-background hl-line-face "gray25")

;; nlinum/nlinum-relative for programming and org.
;; cost lots of CPU.
(require 'nlinum)
(add-hook 'prog-mode-hook '(lambda () (nlinum-mode 1)))
(add-hook 'org-mode-hook  '(lambda () (nlinum-mode 1)))
;; nlinum-relative-mode is CPU consuming.
;(require 'nlinum-relative)
;(nlinum-relative-on)
;(nlinum-relative-setup-evil)
;(setq nlinum-relative-redisplay-delay 0.1)

; tabs
(setq tab-width 2)
(setq indent-tabs-mode nil)
(global-set-key (kbd "TAB") '(lambda () (interactive) (insert "  ")))

; disable some bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; show match parentheses
(show-paren-mode)

; wrap long lines
(set-default 'truncate-lines t)

; bold font
(set-face-bold 'bold t)

; mouse
(xterm-mouse-mode 1)

; scroll
(setq scroll-step 1)

; backup files
(setq make-backup-files nil)
(setq backup-directory-alist `(("." . "~/.saves")))

; yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

; improve display performance
(setq redisplay-dont-pause t)

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

;; eshell
(evil-define-key 'normal eshell-mode-map (kbd "A") 'xueliang-append-in-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============vars=============())

(setq android-root "~/workspace/linaro/")
(setq android-art  (concat android-root "/art/"))
(setq android-vixl (concat android-root "/external/vixl/src/"))
(setq android-art-tags  (concat android-art  "TAGS"))
(setq android-vixl-tags (concat android-vixl "TAGS"))
(setq tags-table-list (list android-art-tags android-vixl-tags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun =============xueliang-functions=============())

; Help to make upate TAGS of the project I'm working on easier.
(defun xueliang-update-tags-art ()
  "update etags/TAGS of Android ART project" (interactive)
  (cd android-art) (shell-command "ctag"))

(defun xueliang-update-tags-vixl ()
  "update etags/TAGS of Android VIXL project" (interactive)
  (cd android-vixl) (shell-command "ctag"))

; Help to make code reviews easier; requires cpplint.py in $PATH.
(defun xueliang-art-cpplint ()
  "invokes AOSP/art/tools/cpplint.py on current buffer" (interactive)
  (setq-local cpplint-cmd-and-options "cpplint.py --filter=-whitespace/line_length,-build/include ")
  (shell-command (concat cpplint-cmd-and-options (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (evil-window-move-very-bottom) (compilation-mode))

; Help to make cross-project search easier.
(defun xueliang-ag-vixl (arg)
  "look for WORD in VIXL using ag searcher" (interactive "P")
  (cd android-vixl) (helm-do-grep-ag arg))

; Help to make cross-project search easier.
(defun xueliang-ag-art (arg)
  "look for WORD in Android ART using ag searcher" (interactive "P")
  (cd android-art) (helm-do-grep-ag arg))

; Helper to cd to directory of current buffer/file.
(defun xueliang-cd-current-buffer-directory ()
  "cd to directory of current buffer/file." (interactive)
  (cd (file-name-directory buffer-file-name))
  (message "pwd: %s" (file-name-directory buffer-file-name)))

; for switching between .h/.cc files.
; USE NEOTREE INSTEAD
(defun xueliang-A-h-cc-files-switcher ()
  "USE NEOTREE INSTEAD; similiar to A plugin for VIM, just type 'M-x A' in helm" (interactive)
  (neotree-toggle))

; for tag search in android-art project.
(defun helm-etags-select-android-art() (interactive)
   (cd android-art) (helm-etags-select t))

; invoke e-shell
(defun xueliang-eshell()
   "invokes eshell in a split window, send current line to eshell." (interactive)
   (setq my-cmd-in-eshell (thing-at-point 'line))
   (when (buffer-file-name)
     (xueliang-cd-current-buffer-directory)
     (if (get-buffer-window "*eshell*")
       (switch-to-buffer-other-window "*eshell*")
       ;; else
       (split-window-below) (evil-window-move-very-bottom) (eshell)))
   (evil-goto-line) (evil-append-line 1) (insert my-cmd-in-eshell) (left-char))

; search in project using ag
(defun xueliang-ag-search-in-project(argument)
  "search in project using ag; use fiplr to goto the root dir of the project"
  (interactive "P")
  (require 'fiplr)
  (cd (fiplr-root)) (helm-do-grep-ag argument))

(defun xueliang-append-in-shell()
  (interactive)
  (evil-goto-line) (evil-append-line 1))  ;; or in shell/term in normal mode, type in G A.

(defun xueliang-run-linaro-art-test()
  "shows command for linaro target test single test, which can be further sent to shell to execute."
  (interactive)
  (split-window-horizontally)
  (find-file "~/workspace/dotfiles/linaro-build-scripts")
  (helm-swoop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============xueliang-key-bindings=============())

; Nice M-x
(global-set-key (kbd "M-x") 'helm-M-x)

; vim way of page up, the original universal argument is <leader>-u.
(global-set-key (kbd "C-u") 'evil-scroll-page-up)

;; my vim way of moving screen
(global-set-key (kbd "C-j") 'evil-scroll-line-down)
(global-set-key (kbd "C-k") 'evil-scroll-line-up)

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

(global-set-key (kbd "<f9>")  'neotree-toggle)        ; neotree
(global-set-key (kbd "<f10>") 'helm-for-files)        ; find files in project.
(global-set-key (kbd "<f11>") 'helm-grep-do-git-grep) ; increment grep using git-grep.
(global-set-key (kbd "<f12>") 'helm-do-grep-ag)       ; grep current word in project.

; simply save current file
(defun xueliang-ctrl-s() "simply save current file like other modern editors do." (interactive)
       (write-file (buffer-file-name))
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
;; * emacsclient -c
;; * helm-show-kill-ring
;; * evil mode, helm, org mode, company mode.
;; * helm-swoop
;; * (ielm) inferior-emacs-lisp-mode
;; * company-mode: M-n/p to select, TAB to complete the common part.
;; * helm: C-n/p to move up/down in helm buffer-menu-delete-backwards
;; * M-n/p is usually going through history items.
;; * Useful functions for coding: beginning-of-defun (C-M-home), end-of-defun (C-M-end).
;; * compilation-minor-mode in eshell, after use eshell for compiling project.

;; https://github.com/emacs-tw/awesome-emacs
;; http://www.john2x.com/emacs.html
;; https://github.com/caiorss/Emacs-Elisp-Programming
;; https://github.com/emacs-helm/helm/wiki
;; http://cestlaz.github.io/stories/emacs/
;; https://tuhdo.github.io/helm-intro.html
;; https://github.com/jwiegley/use-package
;; https://github.com/kai2nenobu/guide-key
;; http://aaronbedra.com/emacs.d/#sec-1
;; https://github.com/noctuid/evil-guide/blob/master/README.org
