(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages that are in use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============packages-config=============())
(defvar xueliang/packages '(evil
                            evil-leader
                            vimrc-mode
                            helm
                            helm-swoop
                            helm-chrome
                            helm-descbinds
                            helm-projectile
                            helm-flyspell
                            helm-themes
                            helm-google
                            company
                            company-statistics
                            vdiff
                            magit
                            evil-magit
                            eshell-did-you-mean
                            ivy
                            smart-mode-line
                            rainbow-delimiters
                            org-bullets
                            git-gutter-fringe+
                            guide-key
                            neotree
                            hl-line+
                            nlinum
                            whitespace
                            fiplr
                            heroku-theme
                            zenburn-theme
                            graphviz-dot-mode
                            projectile))

(defun xueliang-reinstall-packages ()
  "resintall a package if it is missing on this machine." (interactive)
  (dolist (pkg xueliang/packages)
    (when (not (package-installed-p pkg))
      (package-refresh-contents)
      (package-install pkg))))

;; check at emacs start up, make sure all packages are ready to use.
(xueliang-reinstall-packages)

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

;;(require 'evil-matchit)
;;(global-evil-matchit-mode -1)

(setq evil-shift-width 2)

(setq-default xueliang-leader-key "<SPC>")
(evil-leader/set-leader xueliang-leader-key)
(evil-leader/set-key
  "<SPC>" 'helm-for-files
  "]" 'helm-etags-select-android-art
  "a" 'xueliang-ag-search-in-project  ;; behaves better than helm-projectile-ag.
  "b" 'helm-for-files
  "e" 'xueliang-eshell
  "E" 'xueliang-eshell-current-line
  "f" 'fiplr-find-file
  "g" 'magit-status
  "i" 'helm-semantic-or-imenu
  "I" 'helm-imenu-in-all-buffers
  "n" 'xueliang-toggle-narrow-to-defun-widen
  "r" 'helm-recentf
  "s" 'xueliang-eshell-new  ;; s means 'shell'
  "S" 'xueliang-send-current-line-to-scratch
  "u" 'universal-argument
  "x" 'helm-M-x  ;; for easier use in the dark
)

(setq evil-mode-line-format 'before)

;; use helm-swoop instead of vim style */# find.
(define-key evil-normal-state-map (kbd "*") 'xueliang-helm-swoop-at-point)
(define-key evil-normal-state-map (kbd "#") 'xueliang-helm-swoop-at-point)
(define-key evil-normal-state-map (kbd "/") 'xueliang-helm-swoop-without-pre-input)
(define-key evil-normal-state-map (kbd "?") 'helm-occur)  ;; sometimes helm-occur behaves better, e.g. for small window.

;; use company use C-n completion.
(define-key evil-insert-state-map (kbd "C-n") 'company-manual-begin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============helm-config=============())
(require 'helm)
(require 'helm-config)
(require 'helm-swoop)
(require 'helm-chrome)
(require 'helm-projectile)

(setq helm-split-window-in-side-p t) ; open helm buffer inside current window
(setq helm-echo-input-in-header-line t)
(setq helm-mode-fuzzy-match nil)
(setq helm-ff-file-name-history-use-recentf nil)

;; I don't like any delay.
(setq helm-input-idle-delay 0)
(setq helm-quick-update 0)

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
      helm-swoop-use-fuzzy-match  nil
      helm-imenu-fuzzy-match      t)

;; rebind tab to the next/previous line in helm window, same behavior as company.
(define-key helm-map (kbd "TAB")       'helm-next-line)
(define-key helm-map (kbd "<tab>")     'helm-next-line)
(define-key helm-map (kbd "<backtab>") 'helm-previous-line)  ;; Shift-Tab is <backtab>
(define-key helm-map (kbd "M-x") 'helm-select-action) ;; list actions using M-x inside helm.

;; use the helm-swoop style preview.
;; because helm-execute-persistent-action kills processes in helm-top, that's why it is bound to Ctrl-Up/Down.
(define-key helm-map (kbd "C-<up>")   '(lambda() (interactive) (helm-previous-line) (helm-execute-persistent-action)))
(define-key helm-map (kbd "C-<down>") '(lambda() (interactive) (helm-next-line) (helm-execute-persistent-action)))

(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 15)
(helm-autoresize-mode 1)

;; if this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; helm-swoop split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

;; all helm windows don't cycle from bottom to beginning.
(setq helm-swoop-move-to-line-cycle nil)

;; quite useful to see what I've deleted.
(define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)
(define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)

;; include flyspell-mode into helm as well.
(add-hook 'flyspell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-M-i") 'helm-flyspell-correct)))
(add-hook 'flyspell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "z=") 'helm-flyspell-correct)))

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============ivy-config=============())
(require 'ivy)

;; helm for M-x, ivy for several scenarios where helm cannot complete.
(ivy-mode 1)

;; TAB behaves as ivy-partial-or-next-line
(define-key ivy-mode-map (kbd "TAB") '(lambda() (interactive) (ivy-partial) (ivy-next-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============org-config=============())
(setq org-todo-keywords '((sequence "TODO" "PROGRESS" "ON-GOING-EFFORT" "DONE")))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; add dotty support in org-babel
(org-babel-do-load-languages (quote org-babel-load-languages)
    (quote ((emacs-lisp . t)
            (java . t)
            (dot . t)
            (ditaa . t)
            (R . t)
            (python . t)
            (ruby . t)
            (gnuplot . t)
            (clojure . t)
            (sh . t)
            (ledger . t)
            (org . t)
            (plantuml . t)
            (latex . t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============company-config=============())

(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 2)

; tab to select in company.
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB")   'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

;; instead of any key bindings for company-complete.
(add-hook 'prog-mode-hook '(lambda () (setq company-idle-delay 0)))
(add-hook 'org-mode-hook '(lambda () (setq company-idle-delay 0)))

;; not to downcase the returned candidates, keep them as-is.
(setq company-dabbrev-downcase nil)

;; better ranking of candidates in company completion.
(company-statistics-mode)

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
(which-function-mode -1)

;; always show which git branch I'm in.
(add-hook 'prog-mode-hook '(lambda () (vc-mode-line (buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============eshell-config=============())
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell

;; avoid company-complete being annoying in eshell mode.
(add-hook 'eshell-mode-hook '(lambda () (setq-local company-idle-delay 5)))

;; bash reverse-i-search style history search, even more powerful with helm.
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-r") 'helm-eshell-history)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-r") 'helm-eshell-history)))

;; begin-of-line, end-of-line in eshell.
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-a") 'eshell-bol)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-a") 'eshell-bol)))

;; it's nice to have C-a C-k combination to delete commands.
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-k") 'evil-delete-line)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-k") 'evil-delete-line)))

(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-e") 'evil-append-line)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-e") 'evil-append-line)))

;; TAB to complete command in eshell, currently I'm using helm style.
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "TAB")   'helm-esh-pcomplete)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "<tab>") 'helm-esh-pcomplete)))

;; Eshell will run a term session to support following complex commands
(add-hook 'eshell-mode-hook '(lambda () (add-to-list 'eshell-visual-commands "htop")))

;; Avoid clearing the whole buffer when typing clear in eshell.
(defun eshell/clear ()
  "Clear the eshell buffer."
  (dotimes (i 5) (eshell-send-input)))

(defun eshell/ec ()
  "create emacsclient frame from eshell."
  (xueliang-make-frame))

(defun eshell/gcommit (arg)
  "make git commit easier."
  (shell-command (message "git commit -m \"%s\"" arg)))

(defun eshell/x ()
  "exit eshell and close the window."
  (insert "exit") (eshell-send-input)
  (delete-window))

(defun eshell/cdroot ()
  "cd to project root"
  (require 'fiplr)
  (cd (fiplr-root)))

;; Ctrl-d just simply closes the eshell window.
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-d") 'delete-window)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-d") 'delete-window)))

;; eshell prompt configs
(setq eshell-prompt-function (lambda () (concat
   (propertize (format-time-string "[%a %d %b, %H:%M] " (current-time)) 'face `(:foreground "gold"))
   (propertize (eshell/pwd) 'face `(:foreground "LightSkyBlue"))
   (propertize " $ " 'face `(:foreground "white")))))
(setq eshell-highlight-prompt nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vdiff mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============vdiff-mode-config=============())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============misc-modes-config=============())
; default theme good themes: tango-dark, zenburn, monokai, wombat, heroku
(if window-system
  (load-theme 'tango-dark t)  ;; themes that work nice with transparency: wombat, tango-dark
  (load-theme 'wombat t))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(93 . 93))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(when window-system
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode))

(require 'guide-key)
(setq guide-key/guide-key-sequence (list xueliang-leader-key "C-h" "C-x" "C-x c" "C-x n"))
(setq guide-key/idle-delay 1.0)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)  ; Enable guide-key-mode

(require 'neotree)
(setq neo-smart-open 1)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") '(lambda () (interactive) (neotree-enter) (neotree-hide)))
(evil-define-key 'normal neotree-mode-map (kbd "RET") '(lambda () (interactive) (neotree-enter) (neotree-hide)))
(evil-define-key 'normal neotree-mode-map (kbd "q")   'neotree-hide)

; said to make emacs faster
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; graphviz-dot-mode
;; build and preview current dot file.
(add-hook 'graphviz-dot-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-c C-c") '(lambda () (interactive) (recompile) (graphviz-dot-preview)))))
(add-hook 'graphviz-dot-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-c C-c") '(lambda () (interactive) (recompile) (graphviz-dot-preview)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============xueliang-git-config/functions=============())
;; xueliang's Git helper functions/commands.

(setq-default xueliang-font "DejaVu Sans Mono")

;; for windows to display diff, put them far-right.
;; for windows for the user to do select, put them very-bottom.
(setq-default shell-output-buffer-name "*Shell Command Output*")

(defun xueliang-gcommit ()
  "run git commit.
   *** HANDLE WITH CARE !!! only used after gwrite & gstatus ***" (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command (message "git commit -m \"improve %s\"" (file-name-nondirectory buffer-file-name))))

(defun xueliang-gstatus ()
  "run git status" (interactive) (shell-command "git status"))

(defun xueliang-gbranch()
  "run git branch" (interactive)
  (vc-mode-line (buffer-file-name)) (shell-command "git branch"))

(defun xueliang-glog ()
  "run git log" (interactive)
  (neotree-hide)  ;; if there is neotree window, make sure neotree doesn't bring wierd window behavior.
  (vc-mode-line (buffer-file-name))  ;; for updating mode-line
  (if (get-buffer-window shell-output-buffer-name)
    (switch-to-buffer-other-window shell-output-buffer-name)
    ;; else
    (split-window-below) (switch-to-buffer shell-output-buffer-name))
  (shell-command "git log -n 100 --pretty=\"%Cred%h %Creset * %Cgreen %<(70)%s %Creset| %<(16)%an | %Cgreen%cr\"")
  (ansi-color-apply-on-region (point-min) (point-max))  ;; display ansi colors (%Cred/green/blue/reset), requres ansi-color.
  (evil-window-move-very-bottom) (evil-beginning-of-line))

(defun xueliang-gdiff ()
  "run git diff HEAD;
   use C-M-i to browse diff hunks; C-c C-c to jump to source code." (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command "git diff HEAD")
  (if (= (buffer-size (switch-to-buffer-other-window shell-output-buffer-name)) 0)
    (kill-buffer-and-window) ;; kill the buffer that we just switched to, should be the shell output buffer window.
    (evil-window-move-far-right) (diff-mode) (evil-next-line 7)))

(defun xueliang-gdiff-revision-at-point ()
  "run 'git diff' using the revision number at point.
   workflow: get git revision in output, browse revisions, apply this function." (interactive)
  (if (null (buffer-file-name (current-buffer)))
      (funcall (lambda () ;; already in shell command output buffer.
                 (evil-beginning-of-line)
                 (shell-command (message "git diff %s " (thing-at-point 'word)))
                 (evil-window-move-far-right) (diff-mode) (evil-next-line 7) (diff-goto-source)))
      (funcall (lambda () ;; in some other file.
                 (xueliang-glog) (message "try apply this function in glog.")))))

(defun xueliang-gshow ()
  "run git show" (interactive)
  (shell-command "git show")
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-far-right) (diff-mode) (evil-next-line 10) (diff-goto-source))

(defun xueliang-gshow-revision-at-point()
  "run 'git show' using the revision number at point.
   workflow: get git revision in output, browse revisions, apply this function." (interactive)
   (if (null (buffer-file-name (current-buffer)))
       (funcall (lambda () ;; already in shell command output buffer.
                  (shell-command (message "git show %s " (thing-at-point 'word)))
                  (evil-window-move-far-right) (diff-mode) (evil-next-line 10) (diff-goto-source)))
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
  (evil-window-move-very-bottom) (hl-line-mode) (toggle-truncate-lines 1))

(defun xueliang-grebase-last-commits()
  "run git rebase on current line and lines above in glog.\nRequire $EDITOR to be set properly.\nOr use magit-rebase-interactive." (interactive)
  (if (null window-system)
    (print "Use (magit-rebase-interactive) command instead.")
    ;; else in window-system
    (if (null (buffer-file-name (current-buffer)))
        (funcall (lambda () ;; already in shell command output buffer.
                   (setq default-frame-alist '((font . xueliang-font)))  ;; make fonts pretty in emacsclient
                   (setq-local rebase-cmd
                     ;; have to write following way because git rebase seems to require a highly functional shell,
                     ;; which the default emacs shell-command cannot provide.
                     (message "/bin/bash ~/bin/git-rebase-head %d" (count-lines (region-beginning) (region-end))))
                   (print rebase-cmd) (async-shell-command rebase-cmd)))
        (funcall (lambda () ;; in some other file.
                   (xueliang-glog) (message "try apply this function in glog."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============global-config/settings=============())

;; start server for emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; make things faster?
(setq echo-keystrokes 0.1)
(setq redisplay-dont-pause t)  ;; improve display performance

;; hide welcome screen
(setq inhibit-splash-screen nil)

;; requires build emacs with: ./configure --with-x-toolkit=gtk
;; good fonts: "Liberation Mono", "DejaVu Sans Mono", "Droid Sans Mono", "Ubuntu Mono"
(set-default-font xueliang-font)

;; font size
(if window-system
    (if (> (x-display-pixel-width) 2000)
        (set-face-attribute 'default nil :height 130)
        (set-face-attribute 'default nil :height 140)))

; line/column related
(column-number-mode)

;; high-light line for programming and org.
(require 'hl-line+)
(add-hook 'prog-mode-hook '(lambda () (hl-line-mode 1)))
(add-hook 'org-mode-hook  '(lambda () (hl-line-mode -1)))  ;; don't enable hl-line in org-mode.
(set-face-background hl-line-face "gray25")

;; nlinum/nlinum-relative for programming and org.
;; cost lots of CPU. And nlinum-relative-mode is even more CPU consuming.
(require 'nlinum)
(add-hook 'prog-mode-hook '(lambda () (nlinum-mode 1)))
(add-hook 'org-mode-hook  '(lambda () (nlinum-mode 1)))

;; tabs
(setq-default tab-width 2)
;; make sure '=' don't insert tab in evil-mode.
(add-hook 'prog-mode-hook '(lambda () (setq indent-tabs-mode nil)))
;; tab in insert mode is simply interpreted as inserting two spaces.
(add-hook 'prog-mode-hook
          '(lambda () (define-key evil-insert-state-local-map (kbd "TAB") '(lambda () (interactive) (insert "  ")))))

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

;; whitespace mode config
(require 'whitespace)
(global-whitespace-mode -1)  ;; don't enable whitespace mode everywhere.
(add-hook 'prog-mode-hook '(lambda () (whitespace-mode 1)))
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face trailing lines-tail tabs))

; '_' as part of word
(add-hook 'c-mode-hook    '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'java-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))

;; modern style 'paste' in evil insert mode.
(define-key evil-insert-state-map (kbd "C-v") 'yank)

;; make kill-ring more convenient.
(setq x-select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; window move
(define-key evil-normal-state-map (kbd "C-M-k")    'windmove-up)
(define-key evil-normal-state-map (kbd "C-M-j")  'windmove-down)
(define-key evil-normal-state-map (kbd "C-M-h")  'windmove-left)
(define-key evil-normal-state-map (kbd "C-M-l") 'windmove-right)
(define-key evil-insert-state-map (kbd "C-M-k")    'windmove-up)
(define-key evil-insert-state-map (kbd "C-M-j")  'windmove-down)
(define-key evil-insert-state-map (kbd "C-M-h")  'windmove-left)
(define-key evil-insert-state-map (kbd "C-M-l") 'windmove-right)

;; glasses-mode o^o settings
(setq glasses-separator "_")

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

;; this is for easier code reading and help drawing flowgraph of the coode (dot graph)
(defun xueliang-send-current-line-to-scratch ()
   "send current line to scratch for further edit." (interactive)
   (kill-ring-save (point) (line-end-position))
   (switch-to-buffer-other-window "*scratch*") (yank) (insert "\n") (evil-window-next 1))

; invoke e-shell
(defun xueliang-create-eshell-or-switch-to-existing ()
   (when (buffer-file-name)
     (xueliang-cd-current-buffer-directory)
     (if (get-buffer-window "*eshell*")
       (switch-to-buffer-other-window "*eshell*")
       ;; else
       (split-window-below) (evil-window-move-very-bottom) (eshell)))
   (evil-goto-line) (evil-append-line 1))

(defun xueliang-eshell-current-line ()
   "invokes eshell in a split window, send current line to eshell." (interactive)
   (kill-ring-save (point) (line-end-position))
   (xueliang-create-eshell-or-switch-to-existing) (yank))

(defun xueliang-eshell ()
   "invokes eshell in a split window." (interactive)
   (xueliang-create-eshell-or-switch-to-existing))

(setq-default eshell-buffer-number 0)
(defun xueliang-eshell-new ()
   "invokes a new eshell in a split window, shell starts in the root of current project." (interactive)
   (setq eshell-buffer-number (+ eshell-buffer-number 1))
   (split-window-below) (evil-window-move-very-bottom) (eshell eshell-buffer-number)
   (evil-goto-line) (evil-append-line 1))

; search in project using ag
(defun xueliang-ag-search-in-project(argument)
  "search in project using ag; use fiplr to goto the root dir of the project"
  (interactive "P")
  (require 'fiplr)
  (cd (fiplr-root)) (helm-do-grep-ag argument))

(defun xueliang-helm-projectile ()
  "switch to current buffer before calling helm-projectile."
  (interactive)
  (require 'helm-projectile)
  (helm-projectile))

(defun xueliang-run-linaro-art-test()
  "shows command for linaro target test single test, which can be further sent to shell to execute."
  (interactive)
  (split-window-horizontally)
  (find-file "~/workspace/dotfiles/linaro-build-scripts")
  (xueliang-helm-swoop-at-point))

;; nice helm-swoop
(defun xueliang-helm-split-window-swoop (use-pre-input)
  (setq new-helm-swoop-window nil)
  (when (window-full-width-p (get-buffer-window (buffer-name)))
    (setq new-helm-swoop-window (split-window-horizontally)))
  (if use-pre-input (helm-swoop) (helm-swoop-without-pre-input))
  (when new-helm-swoop-window
    (delete-window new-helm-swoop-window)))

(defun xueliang-helm-swoop-at-point ()
  "show helm-swoop results in a side window."
  (interactive)
  (xueliang-helm-split-window-swoop t)
  (add-to-list 'regexp-search-ring helm-swoop-pattern))

(defun xueliang-helm-swoop-without-pre-input ()
  "show helm-swoop results in a side window."
  (interactive)
  (xueliang-helm-split-window-swoop nil)
  (add-to-list 'regexp-search-ring helm-swoop-pattern))

(setq-default xueliang-current-font 0)
(defun xueliang-switch-fonts ()
  "it's nice to be able to switch between pretty fonts.\nEspecially for writing emails, docs, and code."
  (interactive)
  (if (= (mod xueliang-current-font 2) 0)
      (set-default-font "DejaVu Sans")
      (set-default-font xueliang-font))
   (setq xueliang-current-font (+ xueliang-current-font 1)))

(defun xueliang-turn-on-transparency ()
  "turn on transparency easier for any eamcs frames."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(93 . 93)))

(setq-default xueliang-current-narrow 1)
(defun xueliang-toggle-narrow-to-defun-widen ()
  "make the switch between narrow and wide view easier."
  (interactive)
  (if (= (mod xueliang-current-narrow 2) 0)
    (narrow-to-defun)
    (widen))
   (setq xueliang-current-narrow (+ xueliang-current-narrow 1)))

(defun xueliang-make-frame ()
  "create a new emacsclient window frame, with nice fonts."
  (interactive)
  (nlinum-mode -1)  ;; a temp fix for the bug: Invalid face linum.
  (setq default-frame-alist '((font . xueliang-font)))
  (make-frame)
  (nlinum-mode 1))

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
(require 'helm-descbinds)
(global-set-key (kbd "C-h b") 'helm-descbinds)

; good way to learn all completion functions.
(global-set-key (kbd "M-/") 'hippie-expand)

; <f1> .. <f4> :
; <f5> .. <f8> : with in buffer : coding, development, tags,
; <f9> .. <f12>: with in project: buffer, find in project, related.

(global-set-key (kbd "<f5>")  'xueliang-helm-swoop-at-point) ; find in current buffer using helm-swoop.
(global-set-key (kbd "<f6>")  'helm-semantic-or-imenu)        ; imenu in current file.
(global-set-key (kbd "<f8>")  'helm-etags-select-android-art) ; find tag and jump to tag in android-art.

(global-set-key (kbd "<f9>")  'neotree-toggle)        ; neotree
(global-set-key (kbd "<f10>") 'helm-for-files)        ; find files in project.
(global-set-key (kbd "<f11>") 'helm-chrome-bookmarks)
(global-set-key (kbd "<f12>") 'helm-google-suggest)   ;; F12 - search the web with google.

; simply save current file
(defun xueliang-ctrl-s() "simply save current file like other modern editors do." (interactive)
       (write-file (buffer-file-name))
       (evil-force-normal-state))
(global-set-key (kbd "C-s") 'xueliang-ctrl-s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/xueliang/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang good tips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * package install RET command-log-mode, toggle-command-log/buffer
;; * Use compilation mode to parse command line outputs, e.g. cpplint's output.
;; * helm-show-kill-ring
;; * evil mode, helm, org mode, company mode.
;; * M-n/p is usually going through history items.
;; * Useful functions for coding: beginning-of-defun (C-M-home), end-of-defun (C-M-end).
;; * compilation-minor-mode in eshell, after use eshell for compiling project.
;; * helm-google and helm-google-suggest is really awesome.
;; * in eshell: ls > /dev/clip can send output to clipboard for future use.
;; * highlight-regexp / unhighlight-regexp for hi-lighting important things on screen.
;; * narrow: helps reading/writing code: narrow-to-defun, narrow-to-region.
;; * evil: in normal mode: evil-find-char/evil-find-char-backward/evil-find-char-to: f, F, t keys.
;; * emacs --debug-init to debug if emacs fails to launch.
;; * ctrl-l to reposition current line: top, middle, bottom.
;; * use glasses-mode for code reading, it makes variable easier to read.
;; * org-redisplay-inline-images / org-toggle-inline-images (C-c C-x C-v).
;; * in org mode, '<s' to start a new code block. C-c ' to open a temp buffer modify the code buffer.

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
;; https://github.com/emacs-helm/helm/blob/master/helm-config.el
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
;; https://pawelbx.github.io/emacs-theme-gallery/
;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
;; http://orgmode.org/worg/org-contrib/babel/intro.html
;; http://orgmode.org/worg/org-contrib/babel/languages.html
