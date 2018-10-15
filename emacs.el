(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages that are in use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============packages-config=============())
(defvar xueliang/packages '(
                            anti-zenburn-theme
                            autopair
                            ace-window
                            company
                            company-statistics
                            company-quickhelp
                            confluence
                            counsel
                            counsel-projectile
                            dakrone-theme
                            eclipse-theme
                            eshell-did-you-mean
                            evil
                            evil-magit
                            fiplr
                            git-gutter-fringe+
                            graphviz-dot-mode
                            general
                            helm
                            helm-ag
                            helm-chrome
                            helm-descbinds
                            helm-flyspell
                            helm-google
                            helm-projectile
                            helm-swoop
                            helm-themes
                            heroku-theme
                            ivy
                            ivy-rich
                            ivy-historian
                            json-mode
                            keyfreq
                            magit
                            markdown-mode+
                            nlinum
                            nyan-mode
                            org-bullets
                            projectile
                            rainbow-delimiters
                            smart-mode-line
                            smex
                            spacemacs-theme
                            telephone-line
                            vdiff
                            vimrc-mode
                            whitespace
                            which-key
                            zenburn-theme
                            ))

(defun xueliang-reinstall-packages ()
  "resintall a package if it is missing on this machine." (interactive)
  (dolist (pkg xueliang/packages)
    (when (not (package-installed-p pkg))
      (package-refresh-contents)
      (package-install pkg))))

;; check at emacs start up, make sure all packages are ready to use.
(xueliang-reinstall-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============vars=============())

(when (string-equal system-type "gnu/linux") (setq xueliang-home "~"))
(when (string-equal system-type "windows-nt")
  (when (file-exists-p "c:/Users/xuezho01") (setq xueliang-home "c:/Users/xuezho01"))
  (when (file-exists-p "c:/Users/xueliang") (setq xueliang-home "c:/Users/xueliang")))
(setq dot-files               (concat xueliang-home "/workspace/dotfiles"))
(setq dropbox-home            (concat xueliang-home "/Dropbox/"))
(setq android-root            (concat xueliang-home "/workspace/linaro"))
(setq android-art             (concat android-root "/art"))
(setq android-bionic          (concat android-root "/bionic"))
(setq android-libcore         (concat android-root "/libcore"))
(setq android-benchmarks      (concat android-root "/benchmarks"))
(setq android-scripts         (concat android-root "/scripts"))
(setq android-vixl            (concat android-root "/external/vixl/src"))
(setq android-xueliang_test   (concat android-root "/xueliang_test"))
(setq android-build           (concat android-root "/build"))
(setq android-device          (concat android-root "/device"))
(setq android-dalvik          (concat android-root "/dalvik"))
(setq android-frameworks-base (concat android-root "/frameworks/base"))

(setq xueliang-project-list (list android-art
                                  android-benchmarks
                                  android-bionic
                                  android-libcore
                                  android-scripts
                                  android-vixl
                                  android-xueliang_test
                                  android-build
                                  android-device
                                  android-dalvik
                                  android-frameworks-base
                                  dot-files
                                  dropbox-home))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p dropbox-home)
  (add-to-list 'load-path (concat dropbox-home "/emacs"))
  (require 'xzhong)
)

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

(setq evil-shift-width 2)

(setq evil-mode-line-format 'before)

;; use helm-swoop instead of vim style */# find.
(define-key evil-normal-state-map (kbd "*") 'xueliang-search-word-forward)
(define-key evil-normal-state-map (kbd "#") 'xueliang-search-word-backward)

;; swiper is slow, for quick searching with '/' and '?', I'm still keeping the old vim way.
(define-key evil-normal-state-map (kbd "/") 'counsel-grep-or-swiper)
(define-key evil-normal-state-map (kbd "?") 'evil-search-backward)

;; emacs style search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

;; like vim, ESC also removes highlight.
(define-key evil-normal-state-map (kbd "<escape>") '(lambda() (interactive)
                                                      (evil-force-normal-state)
                                                      (unhighlight-regexp t)
                                                      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <leader> config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============leader-general-config=============())

(require 'general)
(general-evil-setup t)

(nvmap :prefix "SPC"
  "SPC" 'ivy-switch-buffer
  "]" 'semantic-ia-fast-jump
  "a" 'xueliang-ag-search-in-project
  "e" 'xueliang-eshell
  "E" 'xueliang-eshell-current-line
  "i" 'counsel-imenu
  "I" 'counsel-imenu
  "j" 'semantic-ia-fast-jump  ;; j means 'jump to tag'
  "J" 'semantic-complete-jump ;; J means 'jump to tag'
  "k" 'xueliang-google-current-word
  "m" 'counsel-bookmark
  "n" 'xueliang-toggle-narrow-to-defun-widen
  "r" 'counsel-recentf
  "t" 'undo-tree-visualize     ;; very useful function
  "S" 'xueliang-send-current-line-to-scratch
  "u" 'universal-argument
  "X" 'kill-buffer-and-window  ;; common operation.
)

;; b for buffer commands
(nvmap :prefix "SPC"
  "bs" 'xueliang-open-scratch-buffer
  "bw" 'read-only-mode
)

;; g for git commands
(nvmap :prefix "SPC"
  "gb" 'xueliang-gblame-current-buffer
  "gd" 'xueliang-gdiff
  "gg" 'magit-status
  "gl" 'xueliang-glog
)

;; p for project commands
(nvmap :prefix "SPC"
  "pf" 'xueliang-find-file    ;; fast search a file in current directory
  "pp" 'xueliang-switch-project
)

;; s for search commands
(nvmap :prefix "SPC"
  "sa" 'xueliang-ag-search-in-project
  "sg" 'counsel-grep-or-swiper
  "ss" 'counsel-grep-or-swiper
)

;; w for windows commands
(nvmap :prefix "SPC"
  ;; select/switch windows
  "ww" 'ace-window
  ;; window splits
  "ws" 'split-window-below
  "wv" 'split-window-right
  ;; windows resize
  "w0" 'balance-windows
  "w-" '(lambda () (interactive) (evil-window-decrease-height 50) (evil-window-decrease-width 50))
  "w=" '(lambda () (interactive) (evil-window-increase-height 50) (evil-window-increase-width 50))
  ;; window move commands, similar to vim's C-W-h/j/k/l moves.
  "wl" 'evil-window-move-far-right
  "wh" 'evil-window-move-far-left
  "wj" 'evil-window-move-very-bottom
  "wk" 'evil-window-move-very-top
  "wd" 'delete-window  ;; same as spacemacs
  "wx" 'delete-window
  ;; frames
  "wF" 'xueliang-make-frame
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============helm-config=============())
(require 'helm)
(require 'helm-config)
(require 'helm-swoop)
(require 'helm-chrome)
(require 'helm-projectile)

(setq helm-split-window-default-side 'below)
(setq helm-split-window-in-side-p t) ; open helm buffer inside current window
(setq helm-mode-fuzzy-match nil)
(setq helm-ff-file-name-history-use-recentf nil)

;; avoid visual noise
(setq helm-echo-input-in-header-line t)
(setq helm-display-header-line t)

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
      helm-projectile-fuzzy-match t
      helm-imenu-fuzzy-match      t)

;; rebind tab to the next/previous line in helm window, same behavior as company.
(define-key helm-map (kbd "TAB")       'helm-next-line)
(define-key helm-map (kbd "<tab>")     'helm-next-line)
(define-key helm-map (kbd "<backtab>") 'helm-previous-line)  ;; Shift-Tab is <backtab>
(define-key helm-map (kbd "M-x")       'helm-select-action) ;; list actions using M-x inside helm.

(define-key helm-map (kbd "C-f")       'helm-next-page)
(define-key helm-map (kbd "C-b")       'helm-previous-page)

;; Similar behavior to ivy.
;; M-w to mark all candidates and send 'em to current buffer.
(defun xueliang-helm-copy-to-scratch-buffer ()
  (helm-run-after-exit (lambda (marked-candidates)
                         (switch-to-buffer-other-window "*scratch*")
                         (evil-goto-line) (evil-append-line 1)
                         (insert "\n\n\n")
                         (insert (mapconcat (lambda (c) (format "%s" c)) marked-candidates "\n"))
                         (evil-beginning-of-line) (evil-normal-state))
                       (helm-marked-candidates)))

(define-key helm-map (kbd "M-w")       '(lambda() (interactive) (helm-mark-all) (xueliang-helm-copy-to-scratch-buffer)))

;; use the helm-swoop style preview.
;; because helm-execute-persistent-action kills processes in helm-top, that's why it is bound to Ctrl-Up/Down.
(define-key helm-map (kbd "C-<up>")   '(lambda() (interactive) (helm-previous-line) (helm-execute-persistent-action)))
(define-key helm-map (kbd "C-<down>") '(lambda() (interactive) (helm-next-line) (helm-execute-persistent-action)))

(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)
(helm-autoresize-mode 1)

;; if this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; helm-swoop split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

;; all helm windows don't cycle from bottom to beginning.
(setq helm-swoop-move-to-line-cycle nil)

;; avoid wrap in helm-swoop window
(setq truncate-partial-width-windows 100)

;; limit candidate number to improve helm's performance.
;; however, sometimes it's nice to know the total number of candiates (total proccesses, files, etc),
;; for these scenarios, ivy seems to be a better.
(setq helm-candidate-number-limit 200)

;; include flyspell-mode into helm as well.
(add-hook 'flyspell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-M-i") 'helm-flyspell-correct)))
(add-hook 'flyspell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "z=") 'helm-flyspell-correct)))

;; in helm-top, avoid killing process by mistake.
(add-hook 'helm-top-after-init-hook '(lambda () (define-key helm-top-map (kbd "RET") 'helm-next-line))) 

;; make helm window always stay at the bottom, just like a mini buffer.
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(helm-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============ivy-config=============())
(require 'ivy)

(ivy-mode 1)
(ivy-historian-mode -1)
(counsel-mode 1)

;; number of result lines to display
(setq ivy-fixed-height-minibuffer t)
(setq ivy-height 20)

;; enable more stuff
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers nil)

;; TAB behaves as ivy-partial-or-next-line
(define-key ivy-mode-map (kbd "TAB") '(lambda() (interactive) (ivy-partial) (ivy-next-line)))

;; I don't like the default "^" for M-x command.
(add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))

;; Make sure C-a C-k work in ivy mode as well.
(define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)

;; Make counsel-grep case insensitive.
(setq counsel-grep-base-command "grep -inE '%s' %s")

;; Better ivy switch buffer.
(require 'ivy-rich)
(ivy-rich-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============org-config=============())
(setq org-todo-keywords '((sequence "TODO" "PROGRESS" "DONE")))

;; Don't add a time stamp line to the 'DONE' task.
(setq org-log-done nil)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○" "✿" "✼"))

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

(add-hook 'org-mode-hook '(lambda () (define-key evil-normal-state-map (kbd "C-c C-o") 'org-open-at-point)))

;; Make sure M-RET behaves correctly on Windows system.
(when (string-equal system-type "windows-nt")
  (add-hook 'org-mode-hook '(lambda ()
     (define-key evil-normal-state-map (kbd "M-RET") 'org-meta-return)
     (define-key evil-insert-state-map (kbd "M-RET") 'org-meta-return)
)))

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

;; quick help popup
(company-quickhelp-mode -1)
(setq company-quickhelp-delay 0)

(add-to-list 'completion-styles 'initials t)
(setq completion-cycle-threshold 5)

;; having a stable completion in c++ mode is more important than having a fancy one.
(add-hook 'c++-mode-hook '(lambda ()
                            (setq-local company-backends '(company-dabbrev-code))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-line config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============telephone-mode-line-config=============())
(require 'telephone-line)

(setq telephone-line-height 20
      telephone-line-evil-use-short-tag nil)

(if (string-equal system-type "windows-nt")
    ;; flat mode-line on windows
    (setq telephone-line-primary-left-separator 'telephone-line-flat
          telephone-line-primary-right-separator 'telephone-line-flat)
    ;; beatiful mode-line on other systems
    (setq telephone-line-primary-left-separator 'telephone-line-halfcos-left
          telephone-line-primary-right-separator 'telephone-line-halfcos-left)
)

(set-face-background 'telephone-line-evil-normal "DimGrey")
(set-face-attribute  'telephone-line-evil-normal nil :bold nil)
(set-face-attribute  'telephone-line-evil-insert nil :bold nil)
(set-face-attribute  'telephone-line-evil-visual nil :bold nil)
(set-face-foreground 'telephone-line-accent-active "LightGrey")
(set-face-foreground 'telephone-line-accent-inactive "LightGrey")

;; only way I can set mode-line faces so far.
(add-hook 'emacs-startup-hook '(lambda()
                                 (set-face-background 'mode-line "DeepSkyBlue4")
                                 (set-face-foreground 'mode-line "LightGrey")
                                 (set-face-background 'mode-line-inactive "DarkGrey")
                                 (set-face-foreground 'mode-line-inactive "DimGrey")))

;; always show which git branch I'm in.
(add-hook 'prog-mode-hook '(lambda () (vc-mode-line (buffer-file-name))))

;; put all important information on the left side in mode line.
(setq telephone-line-lhs '(
   (evil   . (telephone-line-evil-tag-segment))
   (accent . (telephone-line-buffer-segment))
   (nil    . (telephone-line-vc-segment telephone-line-erc-modified-channels-segment telephone-line-process-segment))
   (evil .   (telephone-line-major-mode-segment))
   (nil    . (telephone-line-misc-info-segment telephone-line-minor-mode-segment))
))

(setq telephone-line-rhs '(
   (evil . (telephone-line-airline-position-segment))
))

;; above settings should be called before enabling telephone-line-mode.
(telephone-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============eshell-config=============())
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell

;; avoid company-complete being annoying in eshell mode.
(add-hook 'eshell-mode-hook '(lambda () (setq-local company-idle-delay 5)))

;; bash reverse-i-search style history search
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

;; TAB to complete command in eshell
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "TAB")   'completion-at-point)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "<tab>") 'completion-at-point)))

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

(defun eshell/cdandroid-root ()
  "cd to project root"
  (cd android-root))

(defun eshell/e (file-name)
  "handle large files better"
  (if (> (string-to-number (car (split-string (shell-command-to-string (message "du -sm %s" file-name)))))
         30)
      (shell-command (message "gnome-terminal -e \"vim %s \"" file-name))
      (find-file-other-window file-name)))

(defun eshell/vimdiff (file1 file2)
  (shell-command (message "gnome-terminal -e \"vimdiff %s %s\"" file1 file2)))

(defalias 'eshell/vi 'eshell/e)
(defalias 'eshell/vim 'eshell/e)

;; Ctrl-d just simply closes the eshell window.
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-d") 'delete-window)))
(add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-d") 'delete-window)))

(setq eshell-highlight-prompt t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============other-misc-modes-config=============())

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(when window-system
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode))

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)
(setq which-key-idle-delay 0.3)
;; (which-key-setup-minibuffer)

; said to make emacs faster
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; Make sure emacs opens link with google-chrome on my Linux.
(when (string-equal system-type "gnu/linux")
  (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome"))

;; graphviz-dot-mode
;; build and preview current dot file.
(add-hook 'graphviz-dot-mode-hook '(lambda () (define-key evil-normal-state-local-map (kbd "C-c C-c") '(lambda () (interactive) (recompile) (graphviz-dot-preview)))))
(add-hook 'graphviz-dot-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-c C-c") '(lambda () (interactive) (recompile) (graphviz-dot-preview)))))

;; nice progress bar in mode line.
(nyan-mode -1)

;; use keyfreq-show to see how many times you used a command.
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; autopair mode
(add-hook 'prog-mode-hook '(lambda () (autopair-mode -1)))

;; winner mode
(winner-mode 1)

;; don't make sound on windows.
(when (string-equal system-type "windows-nt")
  (setq visible-bell t))

;; files and their major modes.
(add-to-list 'auto-mode-alist '("Android.bp" . json-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============projectile-configs=============())

;; don't enable projectile for performance considerations.
(projectile-mode -1)

(setq projectile-require-project-root nil)
(setq projectile-enable-caching nil)

(setq projectile-globally-ignored-directories
      (append '(".dropbox.cache" "dropbox/.dropbox.cache" "dropbox (Case Conflict)")
              projectile-globally-ignored-directories))

;; TODO: ignore file names begining with # or . and ignore file names ending with # or ~
(setq projectile-globally-ignored-files
   (append '(
    "#*#"
    ) projectile-globally-ignored-files))

;; ignore some files in find-file.
(setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============xueliang-git-config/functions=============())

(setq-default shell-output-buffer-name "*Shell Command Output*")

(defun xueliang-gcommit ()
  "run git commit.
   *** HANDLE WITH CARE !!! ***" (interactive)
   (xueliang-cd-current-buffer-directory)
   (shell-command (message "git commit -m \"%s\""
                           (ivy-read "COMMIT MSG: " (list 
                                                           (message "Improve code in %s." (file-name-nondirectory buffer-file-name))
                                                           (message "Address review comments to %s." (file-name-nondirectory buffer-file-name))
                                                           )))))
(defun xueliang-gstatus ()
  "run git status" (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command "git status") (switch-to-buffer-other-window shell-output-buffer-name))

(defun xueliang-gcheckout-branch()
  "list git branches, and git checkout selected branch" (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command-to-string (concat "git checkout "
                                   (ivy-read "Git branch: "
                                                   (split-string (shell-command-to-string "git branch") "\n")
                                                   :preselect "*")))
  (revert-buffer :ignore-auto :noconfirm)  ;; force reload the file and update git info on mode line.
  (vc-mode-line (buffer-file-name)))
(defalias 'xueliang-gbranch 'xueliang-gcheckout-branch)

(defun xueliang/glog (&optional xueliang-cword)
  "git log with ivy"
  (require 'fiplr)
  (xueliang-cd-current-buffer-directory) (cd (fiplr-root))
  (car (split-string
        (ivy-read "Git Log: "
                        (split-string (shell-command-to-string "git log -n 100 --pretty=\"%h * %<(70)%s | %<(16)%an | %cr\"") "\n")
                        :preselect "|"  ;; this makes sure that the first candidate in the log is pre-selected.
                        :initial-input (if xueliang-cword                                      ;; if current word at point is a string
                                           (if (= 0 (string-match "[a-f0-9]+" xueliang-cword)) ;; and it is a git version string
                                               xueliang-cword                                  ;; then use it as initial-input;
                                             "") "")))))                                       ;; otherwise, avoid giving any initial-input.

(defun xueliang-gdiff ()
 "run git diff HEAD" (interactive)
  (when (buffer-file-name) (require 'fiplr) (xueliang-cd-current-buffer-directory) (cd (fiplr-root)))
  (magit-diff-working-tree) (evil-next-line 7)
;;
;; my alternative implementation:
;; (shell-command "git diff HEAD")
;; (if (= (buffer-size (switch-to-buffer-other-window shell-output-buffer-name)) 0)
;;  (kill-buffer-and-window) ;; kill the buffer that we just switched to, should be the shell output buffer window.
;; (evil-window-move-far-right) (diff-mode) (view-mode) (evil-next-line 7))
)

(defun xueliang-gdiff-revision-at-point ()
   "run 'git diff' using the revision number from ivy glog" (interactive)
   (when (buffer-file-name) (require 'fiplr) (xueliang-cd-current-buffer-directory) (cd (fiplr-root)))
   (magit-diff (xueliang/glog)) (evil-next-line 7)
;;
;; my alternative implementation:
;; (shell-command (message "git diff %s " (xueliang/glog (thing-at-point 'word))))
;; (switch-to-buffer-other-window shell-output-buffer-name)
;; (evil-window-move-far-right) (diff-mode) (evil-next-line 10)
)

;; similar behavior as org mode.
(require 'magit-status)
(define-key magit-status-mode-map (kbd "TAB") 'magit-section-cycle)
(define-key magit-status-mode-map (kbd "<tab>") 'magit-section-cycle)
(require 'magit-diff)
(define-key magit-diff-mode-map (kbd "TAB") 'magit-section-cycle)
(define-key magit-diff-mode-map (kbd "<tab>") 'magit-section-cycle)

(defun xueliang/gdiff-window ()
  "show a diff window at the right side."
  (if (get-buffer-window shell-output-buffer-name)
      (select-window (get-buffer-window shell-output-buffer-name))
      (switch-to-buffer-other-window shell-output-buffer-name))
  (evil-window-move-far-right) (diff-mode) (evil-next-line 10))

(defun xueliang-gshow ()
  "run 'git show' using the ivy glog" (interactive)
  (magit-show-commit (xueliang/glog)) (evil-next-line 17)
;;
;; my alternative implementation:
;; (shell-command (message "git show %s " (xueliang/glog))) (xueliang/gdiff-window)
)

(defun xueliang-gshow-revision-at-point-OPEN-GERRIT-REVIEW ()
  "run 'git show' using the ivy glog" (interactive)
  (setq git-revision-string (thing-at-point 'word))
  (when git-revision-string
    (magit-show-commit git-revision-string) (evil-next-line 17)
;;  (shell-command (message "git show %s " git-revision-string)) (xueliang/gdiff-window)
    (org-open-link-from-string (concat
                                (ivy-read "Select Gerrit: "
                                          (list
                                          "about:blank"
                                          "https://android-review.googlesource.com/#/q/"
                                          "https://dev-private-review.linaro.org/#/q/")
                                          :preselect "blank")
                                git-revision-string))))

(defun xueliang-gread-current-buffer ()
  "run git checkout on current buffer" (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command (concat "git checkout " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm)
  (message "git checkout: " (buffer-file-name)))

(defun xueliang-gwrite-current-buffer ()
  "run git add on current buffer" (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command (concat "git add " (buffer-file-name)))
  (xueliang-gread-current-buffer) ;; gread it again to refresh git-gutter.
  (message "git add: %s" (buffer-file-name)))

(defun xueliang-gblame-current-buffer ()
  "run git blame on current buffer, esp. current line" (interactive)
  (xueliang-cd-current-buffer-directory)
  (setq-local gblame-line (line-number-at-pos))
  (shell-command (concat "git blame " (buffer-file-name)))
  (goto-line gblame-line (switch-to-buffer-other-window shell-output-buffer-name))
  (evil-window-move-very-bottom) (toggle-truncate-lines 1))

(defalias 'xueliang-grebase 'magit-rebase-interactive)
(defalias 'xueliang-glog 'magit-log-all)

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

;; hide welcome screen, so that I can all daily-websites immediate after emacs started.
(setq inhibit-splash-screen t)

; line/column related
(column-number-mode)

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

;; cursor, bar settings
(setq blink-cursor-mode -1)
(add-hook 'prog-mode-hook '(lambda () (blink-cursor-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)))
(add-hook 'org-mode-hook  '(lambda () (blink-cursor-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)))
(menu-bar-mode -1)

(add-hook 'prog-mode-hook '(lambda () (nlinum-mode 1)))

; show match parentheses
(show-paren-mode)

; wrap long lines
(set-default 'truncate-lines t)

; no bold font by default
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
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face trailing lines-tail tabs))
(set-face-background 'whitespace-line "cyan")
(set-face-foreground 'whitespace-line "black")
(add-hook 'c-mode-hook    '(lambda () (whitespace-mode 1)))
(add-hook 'c++-mode-hook  '(lambda () (whitespace-mode 1)))
(add-hook 'java-mode-hook '(lambda () (whitespace-mode 1)))

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

;; auto revert buffers
(global-auto-revert-mode 1)

;; make sure I have same behavior like org mode, M-RET doesn't maximize the frame/window.
(define-key evil-normal-state-map (kbd "M-RET") 'evil-ret)

;; frame window maximize: also ESC-f10, M-RET.
(define-key evil-insert-state-map (kbd "C-M-m") 'toggle-frame-maximized)
(define-key evil-normal-state-map (kbd "C-M-m") 'toggle-frame-maximized)

;; other-frame, make it easier for me to switch between frames/projects.
(define-key evil-insert-state-map (kbd "C-M-f") 'other-frame)
(define-key evil-normal-state-map (kbd "C-M-f") 'other-frame)

;; glasses-mode o^o settings
(setq glasses-separator "_")

;; goto-address-mode related
(add-hook 'prog-mode-hook '(lambda ()  (goto-address-prog-mode 1)))
(add-hook 'org-mode-hook '(lambda ()  (goto-address-prog-mode 1)))
;; org mode style open link
(define-key evil-normal-state-map (kbd "C-c C-o") 'goto-address-at-point)

(defun =============theme-color-settings=============())

(defun xueliang/minimal-theme()
  "Make the colors feel better in my eyes, with minimal tweaks to original theme."
  (when (string-equal system-type "gnu/linux") (set-default-font "Monospace"))
  (when (string-equal system-type "windows-nt") (set-default-font "Consolas"))

  (if (<= (display-pixel-height) 1080)
     (set-face-attribute 'default nil :height 140)  ;; laptop
     (set-face-attribute 'default nil :height 130)) ;; workstation

  (set-face-attribute 'org-level-1 nil :bold nil :height 1.0)
  (set-face-attribute 'org-level-2 nil :bold nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :bold nil :height 1.0)
)

(defun xueliang-minimal-dark-theme() (interactive)
  (load-theme 'spacemacs-dark t) (xueliang/minimal-theme))

(defun xueliang-minimal-light-theme() (interactive)
  (load-theme 'spacemacs-light t) (xueliang/minimal-theme))

(if (and (string-equal system-type "gnu/linux") (> (display-pixel-height) 1080))
    (xueliang-minimal-light-theme)  ;; only for big screen at work.
    (xueliang-minimal-dark-theme)   ;; works great on all my laptops: windows & linux.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun =============xueliang-functions=============())

(require 'cl-lib)
(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

(defun xueliang-flush-blank-lines (start end)
  "Remove blank lines in selected lines." (interactive "r")
   (flush-lines "^\\s-*$" start end nil))

(defun xueliang-open-scratch-buffer ()
  "open switch buffer quickly" (interactive)
  (switch-to-buffer-other-window "*scratch*")
  (evil-goto-line) (evil-append-line 1)
  (when (> (buffer-size) 1) (insert "\n"))
  (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.")
  (insert "\n\n")
)

(defun xueliang-search-word-forward ()
  "swiper for small buffers, vim style / for big buffers" (interactive)
  (if (< (buffer-size) 10000000) ;; 10MB limit
      ;;(helm-swoop-symble-pre-input)
      (swiper (thing-at-point 'symbol))
      (evil-search-word-forward 1 (thing-at-point 'symbol))
  )
)

(defun xueliang-search-word-backward ()
  "swiper for small buffers, vim style / for big buffers" (interactive)
  (if (< (buffer-size) 10000000) ;; 10MB limit
      ;;(helm-swoop-symble-pre-input)
      (swiper (thing-at-point 'symbol))
      (evil-search-word-backward 1 (thing-at-point 'symbol))
  )
)

(defun xueliang-untabify-replaced-all-TAB() (interactive)
   "easily replace all TAB in current buffer with spaces."
   (untabify (point-min) (point-max)))

(defun xueliang-what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
                        (get-char-property (point) 'face))
                    ))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun xueliang-switch-project()
  "switch among my projects" (interactive)
  (find-file (ivy-read "Project: " xueliang-project-list))
  (counsel-projectile-find-file)
)

(defun xueliang-find-file ()
  "my fast find file in project" (interactive)
  (counsel-projectile-find-file)
;;
;;  (require 'fiplr)
;;  (if (string-equal system-type "windows-nt")
;;      (projectile-find-file)
;;      (xueliang/find-file (fiplr-root) "")
)

(defun xueliang-find-file-from-pwd ()
  (xueliang/find-file "." ""))

(defun xueliang-find-file-similar ()
  "my fast find file with similar file names, e.g. switch between .cc, .h, _test.cc files" (interactive)
  (xueliang-cd-current-buffer-directory)
  (xueliang/find-file "." (string-remove-suffix "_test"  ;; remove the test suffix for _test.cc files.
                                                (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))

(defun xueliang/find-file (path init-input)
  "my fast find file in project"
  (find-file (ivy-read (concat "Find File " path ": ")
                             (split-string (shell-command-to-string
                                            (concat "ag " path " -l --nocolor -g \"\" "))  ;; faster than find command.
                                           "\n")
                             :initial-input init-input)))

;;(defun xueliang-top() "my top command in emacs" (interactive) (ivy-read "Top: " (split-string (shell-command-to-string "top -b -n 1 | tail -n +6") "\n")))
(defalias 'xueliang-top 'helm-top)

;; avoid company-complete being annoying in gdb mode.
(add-hook 'gdb-mode-hook '(lambda () (setq-local company-idle-delay 60)))

(defun xueliang-cnext-compilation-error ()
  "get compilation error easily in current buffer" (interactive)
  (compilation-mode)
  (compilation-next-error 1))

(defun xueliang-cprev-compilation-error ()
  "get compilation error easily in current buffer" (interactive)
  (compilation-mode)
  (compilation-previous-error 1))

; Help to make code reviews easier; requires cpplint.py in $PATH.
(defun xueliang-art-cpplint ()
  "invokes AOSP/art/tools/cpplint.py on current buffer" (interactive)
  (setq-local cpplint-cmd-and-options
              (concat android-root
                      "/external/google-styleguide/cpplint/cpplint.py "
                      "--filter=-whitespace/line_length,-build/include "))
  (shell-command (concat cpplint-cmd-and-options (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (evil-window-move-very-bottom) (compilation-mode))

; Helper to cd to directory of current buffer/file.
(defun xueliang-cd-current-buffer-directory ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

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
(defun xueliang-eshell-pwd ()
   "invokes a new eshell in a split window, shell starts in the root of current project." (interactive)
   (xueliang-cd-current-buffer-directory)
   (setq eshell-buffer-number (% (+ eshell-buffer-number 1) 10))  ;; eshell number 0-9.
   (split-window-below) (evil-window-move-very-bottom) (eshell eshell-buffer-number)
   (evil-goto-line) (evil-append-line 1))

; search in project using ag
(setq helm-ag-insert-at-point 'symbol)

(defun xueliang-ag-search-in-project (argument)
  "search in project using ag; use fiplr to goto the root dir of the project"
  (interactive "P")
  (xueliang-cd-current-buffer-directory)
  (require 'fiplr) (cd (fiplr-root))
  (counsel-ag (thing-at-point 'word)) ;; counsel-ag allows initial input to play with.
 )

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
  (setq default-frame-alist '((font . "Monospace"))) ;; DejaVu Sans Mono, ubuntu mono
  (make-frame)
  (nlinum-mode 1))

(defun xueliang-htop-cpu ()
  "invokes htop easier." (interactive)
  (split-window-below) (evil-window-move-very-bottom) (evil-window-increase-height 30)
  (term "bash") (rename-buffer (concat "*htop-CPU%-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert "htop -d 10 --sort-key PERCENT_CPU") (term-send-input))

(defun xueliang-htop-io ()
  "invokes htop easier." (interactive)
  (split-window-below) (evil-window-move-very-bottom) (evil-window-increase-height 30)
  (term "bash") (rename-buffer (concat "*htop-IO-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert "htop -d 10 --sort-key IO") (term-send-input))

;; make it easier for me to remember & type some commands.
(defalias 'xueliang-helm-htop 'helm-top)
(defalias 'xueliang-spell-check-on-the-fly-check-mode 'flyspell-mode)
(defalias 'xueliang-eval-region 'eval-region)

(defun xueliang-highlight-current-word ()
  "makes highlight-regexp easier" (interactive)
  (highlight-regexp (thing-at-point 'word)))

(defun xueliang-pwd-string ()
  "get pwd string of current file-buffer easily."
  (interactive) (kill-new (ivy-read "PWD: " (list (buffer-file-name)))))

(defun xueliang-terminal-shell ()
  "start my terminal, e.g. gnome-terminal." (interactive)
  (start-process "my-shell" shell-output-buffer-name "~/bin/terminal")
)

(defun xueliang-eshell-quick-command (str)
  (xueliang-eshell-pwd) (insert str) (eshell-send-input))

(defun xueliang-dev-machine-temperature ()
  "check temperature" (interactive)
  (xueliang-eshell-quick-command "cat /sys/class/thermal/thermal_zone*/temp"))

(defun xueliang-dropbox-status ()
  "check dropbox status quickly" (interactive)
  (xueliang-eshell-quick-command "dropbox status"))

(defun xueliang-open-link ()
  "" (interactive)
  (org-open-link-from-string (car (cdr (split-string
                                        (ivy-read "Link: " xueliang-private-weblink-list)
                                        ))))
)

(defun xueliang-art-proj-monitor () (interactive)
  (when (string-equal system-type "gnu/linux")
    (start-process "my-art-proj-monitor" nil "google-chrome" (concat dropbox-home "art_proj_monitor.svg")))
)

(defun xueliang-daily-websites ()
  (interactive)
  ;; Work
  (org-open-link-from-string "https://mail.google.com/mail/u/0/#inbox")
  (org-open-link-from-string "https://mail.google.com/mail/u/1/#inbox")
  (org-open-link-from-string "https://outlook.office.com/owa/")
  ;; Net working
  (org-open-link-from-string "https://secure.skype.com/portal/overview")
  (org-open-link-from-string "https://www.yammer.com/arm.com/")
  ;; Proj monitor
  (xueliang-art-proj-monitor)
)

(defun xuelinag-bold-text (start end)
  (interactive "r")
  (xueliang-tag-word-or-region "*" "*")
)

(defun xueliang-tag-word-or-region (text-begin text-end)
  "Surround current word or region with given text."
  (interactive "sStart tag: \nsEnd tag: ")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (progn
          (goto-char (region-end))
          (insert text-end)
          (goto-char (region-beginning))
          (insert text-begin))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (goto-char (cdr bds))
        (insert text-end)
        (goto-char (car bds))
        (insert text-begin)))))

(defun xueliang-google-translate-region () (interactive)
   (org-open-link-from-string (concat "https://translate.google.co.uk/#en/zh-CN/"
     (replace-regexp-in-string "[/ \n()]" "%20"            ;; replace some special character with space.
        (buffer-substring (region-beginning) (region-end)) ;; translate current selection/region in buffer.
))))

(defun xueliang-markdown-to-PDF-slides ()
  "Use odpdown/soffice to convert current markdown .md file to PDF slides."
  (interactive)
  (when (string-equal "md" (file-name-extension (buffer-file-name)))
     (setq-local xueliang-file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
     (shell-command (message "odpdown %s -p0 ~/workspace/dropbox/arm.otp %s.odp --break-master Title --content-master Normal" (buffer-file-name) xueliang-file-name))
     (shell-command (message "soffice --convert-to pdf %s.odp && evince %s.pdf" xueliang-file-name xueliang-file-name))
  )
)

(defun xueliang-rm-clean-home-rubish () (interactive)
  (xueliang-eshell-quick-command (concat "rm -rf ~/rubish/out." (format-time-string "%Y-%m-%d-*" (current-time))))
)

(defun xueliang-df () (interactive)
  (xueliang-eshell-quick-command "df -h /data")
)

(defun =============xueliang-linaro-development=============())

(defun xueliang-linaro-art-gtest-host ()
  (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*eshell-linaro-make-art-gtest-host" (format-time-string "-%H:%M:%S" (current-time)) "*"))
  (insert "cd $android-root") (eshell-send-input)
  (insert "art/test/testrunner/run_build_test_target.py -j33 art-gtest-asan") (eshell-send-input)
  ;; (insert "echo y | scripts/tests/test_art_host.sh") (eshell-send-input)
)

(defun xueliang/make-android-system-image (lunch-target)
  "invoke build android system image from andriod-root source tree"
  (split-window-below) (evil-window-move-very-bottom)
  (term "bash") (rename-buffer (concat "*make-android-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert (message "cd %s" android-root)) (term-send-input)
  (insert "source build/envsetup.sh") (term-send-input)
  (insert (concat "lunch " lunch-target)) (term-send-input)
  (insert "time make dx -j33") (term-send-input)
  (insert "time make -j33") (term-send-input)
)

(defun xueliang-make-android-system-image ()
  "Choose from build targets" (interactive)
  (xueliang/make-android-system-image
   (ivy-read "Lunch Targets: " (list
                                      ;; common ones
                                      "aosp_arm64-eng"
                                      "hikey960-userdebug"
                                      "aosp_angler-userdebug"
                                      "-------------------------------"
                                      ;; also useful
                                      "arm_krait-eng"
                                      "arm_v7_v8-eng"
                                      "armv8-eng"
                                      "aosp_arm64-eng"
                                      "aosp_x86_64-eng"
                                      "silvermont-eng"
                                      "aosp_marlin-userdebug"
                                      "aosp_marlin_svelte-userdebug"
                                      "aosp_sailfish-userdebug"
                                      "aosp_muskie-userdebug"
                                      "aosp_walleye-userdebug"
                                      "aosp_walleye_test-userdebug"
                                      "aosp_taimen-userdebug"
                                      "aosp_angler-userdebug"
                                      "aosp_bullhead-userdebug"
                                      "aosp_bullhead_svelte-userdebug"
                                      ))))

(defun xueliang-linaro-gdb ()
  "invoke gdb linaro tree" (interactive)
  (require 'gdb-mi)
  ;; build and regenerate the test case before starting gdb.
  (tool-bar-mode -1)
  (cd android-xueliang_test) (shell-command (concat android-xueliang_test "/run.sh"))
  (cd android-root) (gdb-many-windows) (gdb "gdb -i=mi -x gdb.init"))

(defun xueliang-linaro-repo-sync-PINNED-MANIFEST ()
  "repo sync linaro tree with pinned manifest under $android-root/pinned-manifest.xml" (interactive)
  (xueliang/linaro-repo-sync "repo sync -d -m /home/xuezho01/workspace/linaro/pinned-manifest.xml -j33"))

(defun xueliang-cfg-analyze-c1visualizer-irhydra ()
  "analyze ART generated .cfg file" (interactive)
  ;;(org-open-link-from-string "http://mrale.ph/irhydra/2.bak/")
  (start-process "cfg-analysis" nil "~/workspace/c1visualizer/bin/c1visualizer"))

(defun xueliang-clean-android-linaro-mv-rm-out-files () (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*mv-rm-out-" (format-time-string "%H:%M:%S*" (current-time)) "*"))
  (insert "cd $android-root") (eshell-send-input)
  (insert (concat "mv out ~/rubish/out.") (format-time-string "%Y-%m-%d-%H_%M_%S" (current-time))) (eshell-send-input)
  (eshell/x)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============xueliang-key-bindings=============())

; Nice M-x
(global-set-key (kbd "M-x") 'counsel-M-x)

; vim way of page up, the original universal argument is <leader>-u.
(global-set-key (kbd "C-u") 'evil-scroll-page-up)

;; my vim way of moving screen
(global-set-key (kbd "C-j") 'evil-scroll-line-down)

;; emacs style kill-line
(global-set-key (kbd "C-k") 'evil-delete-line)

; Use default (describe-function) and (describe-variable) with ivy/counsel.
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h v") 'helm-apropos)

;; quite useful to see what I've deleted.
(define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-y") 'counsel-yank-pop)

; describe key-bindings
(global-set-key (kbd "C-h b") 'counsel-descbinds)

; good way to learn all completion functions.
(global-set-key (kbd "M-/") 'hippie-expand)

;; work with <leader>-j (jump to tag), and Ctrl-t to jump back.
(define-key evil-normal-state-map (kbd "C-t") 'helm-all-mark-rings)

;; <f1> .. <f4> :
(global-set-key (kbd "<f4>")  'kill-buffer-and-window)

;; <f5> .. <f8> :
;; code development related: debug/test, program structure, build.
(global-set-key (kbd "<f5>")   'xueliang-linaro-gdb)
(global-set-key (kbd "<f6>")   'xueliang-eshell-pwd)
(global-set-key (kbd "C-<f6>") 'xueliang-terminal-shell)
(global-set-key (kbd "<f7>")   'xueliang-linaro-art-gtest-host)
(global-set-key (kbd "C-<f7>") 'xueliang-make-android-system-image)

;; Magic key <f8>
;; <f8> shows program/output/content structure in various languages.
(add-hook 'c-mode-hook '(lambda () (define-key
                                     evil-normal-state-local-map (kbd "<f8>")
                                     '(lambda() (interactive) (swiper "void visit ([a-z]* ")))))

(add-hook 'c++-mode-hook '(lambda () (define-key
                                       evil-normal-state-local-map (kbd "<f8>")
                                       '(lambda() (interactive) (swiper "void visit ([a-z]* ")))))

(add-hook 'emacs-lisp-mode-hook '(lambda () (define-key
                                              evil-normal-state-local-map (kbd "<f8>")
                                              '(lambda() (interactive) (swiper "(defun =[=]* ")))))

(add-hook 'magit-diff-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                              (kbd "<f8>")
                                              '(lambda() (interactive) (swiper "modified ")))))

(add-hook 'diff-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                        (kbd "<f8>")
                                        '(lambda() (interactive) (swiper "^diff ")))))

;; for my repo-sync terminal
(add-hook 'term-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                        (kbd "<f8>")
                                        '(lambda() (interactive) (swiper "Fetching projects ")))))

(add-hook 'markdown-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                            (kbd "<f8>")
                                            '(lambda() (interactive) (swiper "^#")))))

;; for selecting date easily in my daily.org
(add-hook 'org-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                        (kbd "<f8>")
                                        '(lambda() (interactive)
                                           (org-shifttab)
                                           (evil-goto-first-line)
                                           ;;(helm-swoop :$query
                                           (swiper
                                            (format-time-string "<%Y-%m-%d" (current-time)))
                                           (org-sort-entries t ?o)
                                           (org-cycle) (org-cycle)
                                           ))))

; <f9> .. <f12>:
(global-set-key (kbd "<f9>")  '(lambda() (interactive) (xueliang-cd-current-buffer-directory) (counsel-find-file)))
(global-set-key (kbd "<C-f9>")  'xueliang-find-file-similar)

(global-set-key (kbd "<f10>") 'ace-window)
(global-set-key (kbd "<f11>") 'helm-google)
(global-set-key (kbd "<C-f11>") '(lambda() (interactive) (org-open-link-from-string "https://google.co.uk")))
(global-set-key (kbd "<f12>") 'xueliang-open-link)
