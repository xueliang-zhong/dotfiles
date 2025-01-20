;; Minimal Emacs Configuration for My Productivity

;; Set package repositories
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package for easier package management
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;
;; Global Settings
;;
;; Set default font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height (if (string-equal system-type "windows-nt") 120 180)
                    :weight 'regular
                    :width 'normal)
;; Avoid menu bar and tool bar
(menu-bar-mode -1) (tool-bar-mode -1)
;; Start Emacs window maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Disable JIT compilation
(setq comp-deferred-compilation nil)
;; General Settings
(setq inhibit-startup-message t          ;; Disable startup message
      ring-bell-function 'ignore         ;; Disable bell
      scroll-conservatively 101          ;; Smooth scrolling
      backup-directory-alist `(("." . "~/.emacs.d/backups"))  ;; Backup files
      auto-save-default nil)             ;; Disable auto-save
;; Improve performance for large files
(setq large-file-warning-threshold 10000000)
;; Enable line numbers
(global-display-line-numbers-mode 0)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Evil Mode (vim keybindings)
(use-package evil
  :config
  (evil-mode 1)
  ;; better evil behaviour
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-shift-width 4)
  ;; better search in evil mode
  (setq evil-symbol-word-search t)
  ;; modern style 'paste' in evil insert mode.
  (define-key evil-insert-state-map (kbd "C-v") #'yank)
  (define-key evil-insert-state-map (kbd "TAB") #'(lambda() (interactive) (insert "  ")))
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up))

;; Evil Leader (SPC as leader key)
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "SPC" 'counsel-switch-buffer
    "bs"  'xueliang-open-scratch-buffer-window
    "bw"  'read-only-mode
    "ff"  'find-file
    "ht"  'counsel-load-theme
    "gg"  'xueliang-git-status-window
    "si"  'counsel-imenu
    "ss"  'swiper-all    ;; search in all open buffers
    "tl"  'hl-line-mode
    "wc"  'delete-window ;; window close
    "wg"  'golden-ratio
    "/"   'counsel-grep-or-swiper
    "x"   'counsel-M-x
    "fp"  'xueliang-find-file-in-dotfiles
    "is"  'ivy-yasnippet ;; insert snippet
    "RET" 'counsel-recentf
    ))

;; Org Mode
(use-package org
  :config
  (setq org-hide-leading-stars t
        org-startup-indented t)
  (define-key org-mode-map (kbd "s-<right>")  'org-metaright)
  (define-key org-mode-map (kbd "s-<left>")   'org-metaleft)
  (define-key org-mode-map (kbd "s-<return>") 'org-meta-return)
  ;; avoid long line wrap
  (add-hook 'dired-mode-hook (lambda () (setq-default truncate-lines t)))
  (add-hook 'org-mode-hook (lambda () (setq-default truncate-lines t)))
  (add-hook 'prog-mode-hook (lambda () (setq-default truncate-lines t))))

(use-package org-superstar
  :after org
  :config
  (setq-default
   org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
   org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓)) ; changes +/- symbols in item lists
   ))

;; Evil Mode
(use-package evil-org
  :after org
  :config (evil-org-mode))

;; Company Mode (Auto-completion)
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq
   company-dabbrev-code-other-buffers 'all
   company-dabbrev-char-regexp "[\\0-9a-zA-Z-_'/]"
   company-idle-delay 0
   company-tooltip-minimum 9
   company-tooltip-limit 9
   company-tooltip-minimum-width 33
   company-minimum-prefix-length 1)
  (setq-default company-backends '(company-files
                                   company-capf
                                   company-dabbrev-code
                                   company-dabbrev
                                   company-gtags
                                   company-keywords))
  (global-company-mode))

;;
;; Ivy, Counsel, and Swiper
(use-package ivy
  :after org
  :config
  (ivy-mode 1)
  (define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)
  (define-key ivy-minibuffer-map (kbd "ESC") 'minibuffer-keyboard-quit))

(use-package ivy-rich :after ivy :config (ivy-rich-mode 1))
(use-package counsel :after ivy :config (counsel-mode 1))

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper)
  :config
  (define-key swiper-map (kbd "ESC") 'minibuffer-keyboard-quit)
  (define-key swiper-all-map (kbd "ESC") 'minibuffer-keyboard-quit))

;; Eshell Settings
(use-package eshell
  :ensure nil
  :config
  (setq eshell-history-size 1000000
        eshell-scroll-to-bottom-on-input t)
  ;; eshell settings
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local company-idle-delay 600)
              (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)
              (define-key evil-insert-state-local-map (kbd "C-k") #'kill-line)
              (define-key evil-insert-state-local-map (kbd "C-r") #'counsel-esh-history)
              (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)
              (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window))))

;; Dired (File management)
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh")
  (evil-define-key 'normal dired-mode-map
    "h" 'dired-up-directory
    "o" 'dired-find-file
    "l" 'dired-find-file-other-window ;; since RET jumps to the file, would be nice to have a convinient preview key
    )
  (define-key dired-mode-map (kbd "TAB") 'dired-display-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-file-other-window))

(use-package dirvish :ensure t :config (dirvish-override-dired-mode 1))

;; Which-key (Keybinding helper)
(use-package which-key :config (which-key-mode 1))

;; Snippets
(use-package yasnippet :ensure t :config (yas-global-mode 1))
(use-package ivy-yasnippet :ensure t)
(use-package yasnippet-snippets :ensure t)

;; Better undo/redo
(use-package undo-fu
  :ensure t
  :after evil
  :config
  (setq evil-undo-system 'undo-fu)
  ;; Bind undo and redo keys explicitly
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))

;; Theme Pack
;; NOTE: all the icons avoided, to simplify the UI (especially on Windows)
(use-package doom-themes :ensure t :config (load-theme 'doom-one t))
(use-package doom-modeline :ensure t
  :config (doom-modeline-mode 1)
  :custom (doom-modeline-icon nil))

;; NOTE: all the icons only enabled for macos, to simplify the UI (especially on Windows)
(when (string-equal system-type "darwin")
  (use-package all-the-icons
    :ensure t
    :config (setq-default all-the-icons-dired-monochrome nil)) ;; colourful icons

  (use-package all-the-icons-dired
    :after all-the-icons
    :config (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

  (use-package all-the-icons-ivy-rich
    :after all-the-icons
    :config (all-the-icons-ivy-rich-mode)))

;; Key bindings
(global-set-key (kbd "s-x") 'execute-extended-command)
(global-set-key (kbd "C-<f4>") #'kill-buffer-and-window)

;; Functioin Keys
(global-set-key (kbd "<f3>")  #'dirvish) ;; keep it simple
(global-set-key (kbd "<f4>")  #'evil-window-delete)
(global-set-key (kbd "<f5>")  #'xueliang-eshell-popup)
(global-set-key (kbd "<f6>")  #'counsel-yank-pop)
(global-set-key (kbd "<f7>")  #'xueliang-just-make)
(global-set-key (kbd "<f8>")  #'xueliang-imenu-or-org-today) ;; counsel-imenu
(global-set-key (kbd "<f9>")  #'dirvish)
(global-set-key (kbd "<f10>") #'xueliang-telescope-counsel)
(global-set-key (kbd "<f11>") #'xueliang-open-link-in-browser)
(global-set-key (kbd "<f12>") #'xueliang-open-knowledge-links)

;;
;; My own functions
;;
(defun xueliang-reload-org-config ()
  "reload my spacemacs config" (interactive)
  ;; ivy settings
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (setq counsel-grep-swiper-limit 30000000)
  (setq ivy-initial-inputs-alist (remove '(counsel-M-x . "^") ivy-initial-inputs-alist))

  ;; org mode settings
  ;; Make sure these settings are called only when org-mode are loaded
  ;; org-mode appearance settings
  (org-indent-mode 1) (org-superstar-mode 1)
  (setq
   org-ellipsis " ▶"
   org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
   '(("DDG" . "https://duckduckgo.com/?q=")
     ("SC" . "https://stockcharts.com/h-sc/ui?s=%s")
     ("wiki" . "https://en.wikipedia.org/wiki/"))
   org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
   '((sequence
      "FOCUS(f)"     ; A task that I am focusing
      "TODO(t)"      ; A task that is ready to be tackled
      "PROG(p)"      ; A task that is in progress but no need to focus right now
      "|"            ; The pipe necessary to separate "active" states and "inactive" states
      "DONE(d)"      ; Task has been completed
      )))
  (setq org-log-done nil)
  (setq org-confirm-babel-evaluate nil)

  ;; key bindings
  ;; better org-mode key behaviour on MacOS
  (define-key org-mode-map (kbd "s-<right>")  'org-metaright)
  (define-key org-mode-map (kbd "s-<left>")   'org-metaleft)
  (define-key org-mode-map (kbd "s-<return>") 'org-meta-return)

  ;; better keys for org mode
  (define-key evil-normal-state-local-map (kbd "O")
              (lambda () (interactive)
                (if (org-at-table-p) (org-table-insert-row) (evil-open-above 1)) (evil-insert 1)))

  (define-key evil-normal-state-local-map (kbd "o")
              (lambda () (interactive)
                (if (org-at-table-p) (org-table-insert-row 1) (evil-open-below 1)) (evil-insert 1)))

  (define-key evil-insert-state-local-map (kbd "TAB") #'(lambda() (interactive) (insert "  ")))
  (define-key evil-normal-state-local-map (kbd "TAB") 'org-cycle)
  (define-key evil-normal-state-local-map (kbd "<return>") 'org-open-at-point)
  (define-key evil-normal-state-local-map (kbd "RET")      'org-open-at-point)

  ;; Face settings
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight 'regular))
  )

(defun xueliang-telescope-counsel ()
  "Show all useful counsel commands"
  (interactive)
  (counsel-M-x "counsel "))

(defun xueliang-imenu-or-org-today ()
  "" (interactive)
  (if (derived-mode-p 'org-mode)
      (progn (xueliang-reload-org-config) (swiper (format-time-string "<%Y-%m-%d")) (evil-ex-nohighlight))
    (counsel-imenu)))

(defun xueliang-cd-current-dir ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

(defun xueliang-find-file () (interactive)
       (xueliang-cd-current-dir) (counsel-find-file))

(defun xueliang-eshell-popup ()
  "Invokes a new eshell in a popup window and ready for command" (interactive)
  (xueliang-cd-current-dir) (evil-window-split) (other-window 1)
  (eshell) (evil-append-line 1))

(defun xueliang-open-link-in-browser ()
  "F12 to select from a list of favourite links to open." (interactive)
  (setq weblink-list
        (with-temp-buffer (insert-file-contents "~/workspace/xzhong-links.txt")
                          (split-string (buffer-string) "\n" t)))
  (setq-local xueliang-weblink-str (nth 1 (split-string (ivy-read "Link: " weblink-list))))
  (org-link-open-from-string xueliang-weblink-str))

(defun xueliang-open-knowledge-links ()
  "My knowledge links quick open" (interactive)
  (setq-local current-buffer-string (split-string (buffer-string) "\n" t))
  (setq-local url-list (cl-remove-if-not (lambda (line) (or (string-match "http" line) (string-match "file:" line))) current-buffer-string))
  (setq-local selected-str (ivy-read "Knowledge Link: " url-list))
  ;; find the URL substr (Knowledge Link) within the selected line
  (when (string-match "\\(https?://[^\s]+\\)" selected-str)
    (setq-local xueliang-url-str (match-string 1 selected-str)))
  (when (string-match "\\(file:[^\s]+\\)" selected-str)
    (setq-local xueliang-url-str (match-string 1 selected-str))
    ;; for file notes, it's better to open in a different window
    (+evil/window-vsplit-and-follow))
  (org-link-open-from-string xueliang-url-str))

(defun xueliang-T-open-T-in-browser () (interactive)
       (when (string-equal system-type "darwin")
         (org-link-open-from-string "https://stockcharts.com/h-sc/ui?s=SPY")))

(defun xueliang-replace-tab-trailing-spaces()
  "Easily replace all TAB in current buffer with spaces." (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun xueliang-what-face (pos)
  "Show the face under current cursor" (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name) (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

(defun xueliang-find-file-in-project ()
  "just do find-file for best performance" (interactive)
  (xueliang-cd-current-dir) (counsel-find-file))

(defun xueliang-open-scratch-buffer-window ()
  "Open scratch buffer window" (interactive)
  (evil-window-vsplit) (other-window 1) (scratch-buffer))

(defun xueliang-git-status-window ()
  "" (interactive)
  (xueliang-eshell-popup)
  (insert "git status") (eshell-send-input))

(defun xueliang-refresh () (interactive)
       (xueliang-cd-current-dir) (evil-force-normal-state)
       (set-auto-mode) (hl-line-mode -1) (message "Refresh!"))

(defun xueliang-duckduckgo-search ()
  "keep it simple search"
  (interactive) (org-link-open-from-string "https://duckduckgo.com"))

(defun xueliang-git-command (git-cmd)
  "silently execute a git command in eshell"
  (xueliang-cd-current-dir) (save-buffer)
  (xueliang-eshell-popup) (insert git-cmd) (eshell-send-input) (evil-window-delete)
  (evil-force-normal-state) (message git-cmd))

(defun Gwrite ()
  "Support :Gwrite similar to vim." (interactive)
  (setq git-cmd (message "git add %s" (file-name-nondirectory buffer-file-name)))
  (xueliang-git-command git-cmd))

(defun Gcommit ()
  "Support :Gcommit similar to vim." (interactive)
  (xueliang-cd-current-dir)
  (setq git-cmd (message "git commit -m \"Update %s\"" (file-name-nondirectory buffer-file-name)))
  (xueliang-git-command git-cmd))

(defun xueliang-find-file-in-dotfiles () (interactive)
       (counsel-find-file nil "~/workspace/dotfiles/"))

(defun xueliang-just-make () (interactive)
       (xueliang-eshell-popup) (insert "just all") (eshell-send-input))

;;
;; Some useful alias
;;
(defalias 'xueliang-cap-region 'capitalize-region)
(defalias 'xueliang-org-sort   'org-sort)
(defalias 'eshell/e   'find-file-other-window)
(defalias 'eshell/vi  'eshell/e)
(defalias 'eshell/vim 'eshell/e)
(defalias 'eshell/f   'counsel-fzf)
(defalias 'eshell/fzf 'eshell/f)

;; Start Emacs
(provide 'init)

;; auto generated code
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-ivy-rich ivy-rich all-the-icons-dired all-the-icons evil-leader which-key evil doom-themes counsel company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
