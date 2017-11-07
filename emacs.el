(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
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
                            counsel
                            counsel-projectile
                            eclipse-theme
                            eshell-did-you-mean
                            evil
                            evil-leader
                            evil-magit
                            fiplr
                            git-gutter-fringe+
                            graphviz-dot-mode
                            guide-key
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
                            hl-line+
                            ivy
                            ivy-historian
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
                            telephone-line
                            vdiff
                            vimrc-mode
                            whitespace
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

(setq android-root          "~/workspace/linaro")
(setq android-art           (concat android-root "/art"))
(setq android-bionic        (concat android-root "/bionic"))
(setq android-libcore       (concat android-root "/libcore"))
(setq android-benchmarks    (concat android-root "/benchmarks"))
(setq android-scripts       (concat android-root "/scripts"))
(setq android-vixl          (concat android-root "/external/vixl/src"))
(setq android-xueliang_test (concat android-root "/xueliang_test"))
(setq dot-files             "~/workspace/dotfiles")
(setq dropbox               "~/workspace/dropbox")

(setq xueliang-project-list (list android-art
                                  android-benchmarks
                                  ;;android-bionic
                                  android-libcore
                                  android-scripts
                                  android-vixl
                                  android-xueliang_test
                                  dot-files
                                  dropbox))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/Dropbox/emacs/")
(require 'xzhong)

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

(setq evil-mode-line-format 'before)

;; Same behavior on last line as vim.
;(define-key evil-normal-state-map (kbd "j") '(lambda() (interactive) (if (<= (+ (line-number-at-pos) 1) (line-number-at-pos (point-max)))
;                                                                         (evil-next-line)
;                                                                         (message "Last line of buffer.")
;                                                                       )))

;; use helm-swoop instead of vim style */# find.
(define-key evil-normal-state-map (kbd "*") 'xueliang-search-word-forward)
(define-key evil-normal-state-map (kbd "#") 'xueliang-search-word-backward)

;; swiper is slow, for quick searching with '/' and '?', I'm still keeping the old vim way.
(define-key evil-normal-state-map (kbd "/") 'helm-swoop-without-pre-input)
(define-key evil-normal-state-map (kbd "?") 'evil-search-backward)

;; give the old vim style /? search a more swiper way: space key inserts ".*"
(define-key isearch-mode-map (kbd "<SPC>") '(lambda() (interactive) (isearch-printing-char 46 1) (isearch-printing-char 42 1)))

;; give the old vim style /? search another improvement: with occur window.
(define-key isearch-mode-map (kbd "<return>") '(lambda() (interactive)
                                                 ;;(when (> (length isearch-string) 1) (occur isearch-string))
                                                 (isearch-exit)
                                              ))

;; emacs style search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

(defun xueliang-close-occur-window()
  "A robust functiont close occur window & buffer."
  (setq xueliang-current-buffer (buffer-name (current-buffer)))
  (when (get-buffer-window "*Occur*")
    (switch-to-buffer-other-window "*Occur*")  ;; switch to the occur buffer window
    (evil-switch-to-windows-last-buffer)       ;; go back to the previous buffer if it was changed by occur
    (if (null (get-buffer-window "*Occur*"))   ;; check if occur is still there, which it means that window is created by occur
        (switch-to-buffer-other-window xueliang-current-buffer)
        (kill-buffer-and-window))              ;; close that occur window, if it was created by occur.
    )
)

;; like vim, ESC also removes highlight.
(define-key evil-normal-state-map (kbd "<escape>") '(lambda() (interactive)
                                                      (evil-force-normal-state)
                                                      (unhighlight-regexp t)
                                                      (xueliang-close-occur-window)
                                                      ))

;; vim style 'G' (end of buffer): doesn't work well in eshell.
;; (define-key evil-normal-state-map (kbd "G") '(lambda() (interactive) (goto-char (- (point-max) 1)) (move-beginning-of-line 1)))

(defun xueliang-search-next-and-update-occur() (interactive)
       (evil-search-next 1)
       (when (get-buffer-window "*Occur*")
         ;; the following code makes the occur window a mini map of searching.
         (setq xueliang-highlight-line (line-number-at-pos))
         (switch-to-buffer-other-window "*Occur*")
         (unhighlight-regexp t) (highlight-regexp (message "^[ ]*%d:.*$" xueliang-highlight-line) 'ivy-current-match)
         (other-window -1))
)

(define-key evil-normal-state-map (kbd "n") 'xueliang-search-next-and-update-occur)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <leader> config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============leader-config=============())
(setq-default xueliang-leader-key "<SPC>")
(evil-leader/set-leader xueliang-leader-key)
(evil-leader/set-key
  "<SPC>" 'helm-for-files
  "]" 'semantic-ia-fast-jump
  "a" 'xueliang-ag-search-in-project
  "b" 'ivy-switch-buffer-other-window
  "e" 'xueliang-eshell
  "E" 'xueliang-eshell-current-line
  "f" 'xueliang-find-file    ;; fast search a file in current directory
  "g" 'magit-status
  "i" 'helm-semantic-or-imenu
  "I" 'helm-semantic-or-imenu
  "j" 'semantic-ia-fast-jump  ;; j means 'jump to tag'
  "J" 'semantic-complete-jump ;; J means 'jump to tag'
  "k" 'xueliang-google-current-word
  "m" 'helm-bookmark
  "n" 'xueliang-toggle-narrow-to-defun-widen
  "p" 'xueliang-find-project
  "r" 'helm-recentf
  "s" 'xueliang-eshell-pwd  ;; s means 'shell'
  "t" 'undo-tree-visualize  ;; very useful function
  "S" 'xueliang-send-current-line-to-scratch
  "u" 'universal-argument
  "x" 'delete-window  ;; common operation.
  "X" 'kill-buffer-and-window  ;; common operation.
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

;; quite useful to see what I've deleted.
(define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)
(define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)

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
;; ido mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============ido-config=============())
(setq ido-enable-flex-matching t)
(setq ido-everywhere nil)
(ido-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============ivy-config=============())
(require 'ivy)

(ivy-mode 1)
(ivy-historian-mode -1)
(counsel-mode 1)

;; number of result lines to display
(setq ivy-height 15)

;; enable more stuff
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers nil)

;; TAB behaves as ivy-partial-or-next-line
(define-key ivy-mode-map (kbd "TAB") '(lambda() (interactive) (ivy-partial) (ivy-next-line)))

;; I don't like the default "^" for M-x command.
(add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))

;; Make sure C-a C-k work in ivy mode as well.
(define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============org-config=============())
(setq org-todo-keywords '((sequence "TODO" "PROGRESS" "ON-GOING-EFFORT" "DONE")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============semantic-config=============())

;; semantic mode, quite slow.
;; (setq semantic-default-submodes
;;       '(global-semantic-idle-scheduler-mode   ;; Perform semantic actions during idle time
;;         global-semanticdb-minor-mode          ;; Use a database of parsed tags
;;         global-semantic-decoration-mode       ;; Decorate buffers with additional semantic information
;;         global-semantic-highlight-func-mode   ;; Highlight the name of the current function
;;         global-semantic-stickyfunc-mode       ;; show the name of the function at the top
;;         global-semantic-idle-summary-mode     ;; Generate a summary of the current tag when idle
;;         global-semantic-idle-breadcrumbs-mode ;; Show a breadcrumb of location during idle time
;;         global-semantic-mru-bookmark-mode))   ;; Switch to recently changed tags with semantic-mrub-switch-tags
;; 
;; (add-hook 'prog-mode-hook '(lambda () (semantic-mode)))
;; 
;; (semantic-mode)
;; (semantic-reset-system-include)
;; (semantic-add-system-include android-art 'c++-mode)
;; (semantic-add-system-include (concat android-art "/compiler") 'c++-mode)
;; (semantic-add-system-include (concat android-art "/compiler/driver") 'c++-mode)
;; (semantic-add-system-include (concat android-art "/compiler/optimizing") 'c++-mode)
;; 
;; including art runtime files would make semantic quite slow.
;;(semantic-add-system-include (concat android-art "/runtime") 'c++-mode)

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
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-buffer-segment))
        (evil    . (telephone-line-airline-position-segment))
        (nil    . (telephone-line-vc-segment telephone-line-erc-modified-channels-segment telephone-line-process-segment))
        (accent . (telephone-line-major-mode-segment))
        (nil    . (telephone-line-misc-info-segment telephone-line-minor-mode-segment))
        ))

(setq telephone-line-rhs '())

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

;; eshell prompt configs
(setq eshell-prompt-function (lambda () (concat
   (propertize (format-time-string "[%a %d %b, %H:%M] " (current-time)) 'face `(:foreground "DarkOrange3")) ;; orange, DarkOrange3
   (propertize (eshell/pwd) 'face `(:foreground "MediumBlue")) ;; SkyBlue, MediumBlue
   (propertize " $ " 'face `(:foreground "black"))))) ;; LightGrey, black
(setq eshell-highlight-prompt nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============misc-modes-config=============())

;; set ivy/counsel faces under anti-zenburn theme
;; (set-face-attribute  'ivy-current-match nil :underline t)
;; (set-face-background 'ivy-match-required-face      "MediumSlateBlue")
;; (set-face-background 'ivy-minibuffer-match-face-1  "LightGrey")
;; (set-face-background 'ivy-minibuffer-match-face-2  "LightSeaGreen")
;; (set-face-background 'ivy-minibuffer-match-face-3  "MediumSeaGreen")
;; (set-face-background 'ivy-minibuffer-match-face-4  "SeaGreen")
;;
;; transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(when window-system
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode))

(require 'guide-key)
(setq guide-key/guide-key-sequence (list xueliang-leader-key "C-h" "C-x" "C-x c" "C-x n" "C-c" "C-c p"))
(setq guide-key/idle-delay 1.0)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)  ; Enable guide-key-mode

; said to make emacs faster
(remove-hook 'find-file-hooks 'vc-find-file-hook)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun =============projectile-configs=============())

;; don't enable projectile for performance considerations.
(projectile-mode -1)
(setq projectile-completion-system 'helm)
;; (helm-projectile-on)

(setq projectile-require-project-root nil)
(setq projectile-enable-caching nil)

(setq projectile-globally-ignored-directories
   (append '(
    ".dropbox.cache"
    "dropbox/.dropbox.cache"
    "dropbox"
    ) projectile-globally-ignored-directories))

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
;; xueliang's Git helper functions/commands.

;; for windows to display diff, put them far-right.
;; for windows for the user to do select, put them very-bottom.
(setq-default shell-output-buffer-name "*Shell Command Output*")

(require 'cl-lib)
(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

(defun xueliang-gcommit ()
  "run git commit.
   *** HANDLE WITH CARE !!! ***" (interactive)
   (xueliang-cd-current-buffer-directory)
   (shell-command (message "git commit -m \"%s\""
                           (helm-comp-read "COMMIT MSG: " (list 
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
                                   (helm-comp-read "Git branch: "
                                                   (split-string (shell-command-to-string "git branch") "\n")
                                                   :preselect "*")))
  (revert-buffer :ignore-auto :noconfirm)  ;; force reload the file and update git info on mode line.
  (vc-mode-line (buffer-file-name)))
(defalias 'xueliang-gbranch 'xueliang-gcheckout-branch)

(defun xueliang-gdiff ()
 "run git diff HEAD;
  use C-M-i to browse diff hunks; C-c C-c to jump to source code." (interactive)
 (xueliang-cd-current-buffer-directory)
 (shell-command "git diff HEAD")
 (if (= (buffer-size (switch-to-buffer-other-window shell-output-buffer-name)) 0)
   (kill-buffer-and-window) ;; kill the buffer that we just switched to, should be the shell output buffer window.
   (evil-window-move-far-right) (diff-mode) (view-mode) (evil-next-line 7)))

;; (defalias 'xueliang-gdiff 'magit-diff-staged)
(defalias 'xueliang-gdiff-UNSTAGED 'magit-diff-unstaged)

(defun xueliang-gshow ()
  "run git show" (interactive)
  (require 'fiplr)
  (xueliang-cd-current-buffer-directory)
  (cd (fiplr-root))
  (shell-command "git show")
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-far-right) (diff-mode) (evil-next-line 10) (diff-goto-source))

(defun xueliang/ivy-glog ()
  "git log with ivy" (interactive)
  (require 'fiplr)
  (xueliang-cd-current-buffer-directory) (cd (fiplr-root))
  (setq-local xueliang-cword (thing-at-point 'word))
  (car (split-string
        (helm-comp-read "Git Log: "
                        (split-string (shell-command-to-string "git log -n 100 --pretty=\"%h * %<(70)%s | %<(16)%an | %cr\"") "\n")
                        :preselect "|"  ;; this makes sure that the first candidate in the log is pre-selected.
                        :initial-input (if xueliang-cword                                      ;; if current word at point is a string
                                           (if (= 0 (string-match "[a-f0-9]+" xueliang-cword)) ;; and it is a git version string
                                               xueliang-cword                                  ;; then use it as initial-input;
                                             "") "")))))                                       ;; otherwise, avoid giving any initial-input.

(defun xueliang-gdiff-revision-at-point ()
  "run 'git diff' using the revision number from ivy glog" (interactive)
   (when (buffer-file-name) (xueliang-cd-current-buffer-directory) (cd (fiplr-root)))
   (shell-command (message "git diff %s " (xueliang/ivy-glog)))
   ;;(magit-diff (xueliang/ivy-glog))
   (switch-to-buffer-other-window shell-output-buffer-name)
   (evil-window-move-far-right) (diff-mode) (evil-next-line 10))

(defun xueliang-gshow-revision-at-point()
  "run 'git show' using the ivy glog" (interactive)
  (shell-command (message "git show %s " (xueliang/ivy-glog)))
  ;;(magit-show-commit (xueliang/ivy-glog))
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-far-right) (diff-mode) (evil-next-line 10))

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

;; simply calls magit-rebase-interactive.
(defalias 'xueliang-glog 'xueliang/ivy-glog)
(defalias 'xueliang-grebase 'magit-rebase-interactive)

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
;; good fonts: "Liberation Mono", "DejaVu Sans Mono", "Droid Sans Mono", "Ubuntu Mono", "Monospace"
(set-default-font "Monospace")

;; font size
(when window-system
    (if (> (x-display-pixel-width) 2000)
        (set-face-attribute 'default nil :height 110)
        (set-face-attribute 'default nil :height 110)))

; line/column related
(column-number-mode)

;; high-light line for programming and org.
(require 'hl-line+)
(add-hook 'prog-mode-hook '(lambda () (hl-line-mode -1)))
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

;; cursor setting
(setq blink-cursor-mode -1)
(add-hook 'prog-mode-hook '(lambda () (blink-cursor-mode -1) (scroll-bar-mode -1)))
(add-hook 'org-mode-hook  '(lambda () (blink-cursor-mode -1) (scroll-bar-mode -1)))

; some bars settings
(menu-bar-mode 1)
(tool-bar-mode 1)
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

;; window move
(define-key evil-normal-state-map (kbd "C-M-k")    'windmove-up)
(define-key evil-normal-state-map (kbd "C-M-j")  'windmove-down)
(define-key evil-normal-state-map (kbd "C-M-h")  'windmove-left)
(define-key evil-normal-state-map (kbd "C-M-l") 'windmove-right)
(define-key evil-insert-state-map (kbd "C-M-k")    'windmove-up)
(define-key evil-insert-state-map (kbd "C-M-j")  'windmove-down)
(define-key evil-insert-state-map (kbd "C-M-h")  'windmove-left)
(define-key evil-insert-state-map (kbd "C-M-l") 'windmove-right)

;; window resize (Ctrl Alt +/-/0)
(define-key evil-insert-state-map (kbd "C-M--") '(lambda () (interactive) (evil-window-decrease-height 20) (evil-window-decrease-width 20)))
(define-key evil-normal-state-map (kbd "C-M--") '(lambda () (interactive) (evil-window-decrease-height 20) (evil-window-decrease-width 20)))
(define-key evil-insert-state-map (kbd "C-M-=") '(lambda () (interactive) (evil-window-increase-height 20) (evil-window-increase-width 20)))
(define-key evil-normal-state-map (kbd "C-M-=") '(lambda () (interactive) (evil-window-increase-height 20) (evil-window-increase-width 20)))
(define-key evil-insert-state-map (kbd "C-M-0") '(lambda () (interactive) (evil-window-increase-height 20) (evil-window-increase-width 20)))
(define-key evil-normal-state-map (kbd "C-M-0") '(lambda () (interactive) (balance-windows)))

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

(defun xueliang/dark-theme()
  "Make the color more vim feel like"
  (load-theme 'tango-dark t)

  (set-default-font "Monospace")
  (set-face-attribute 'default nil :height 110)

  ;; Useful commands: list-faces-display, counsel-colors-emacs.
  ;; Tuned from tango-dark theme.
  (set-face-foreground 'default "DarkGrey")
  (set-face-foreground 'font-lock-comment-face "LightSeaGreen")
  (set-face-foreground 'font-lock-doc-face "LightSeaGreen")
  (set-face-foreground 'font-lock-comment-delimiter-face "LightSeaGreen")
  (set-face-foreground 'font-lock-keyword-face "LightGoldenrod2")
  (set-face-foreground 'font-lock-variable-name-face "DarkGrey")
  (set-face-foreground 'font-lock-type-face "PaleGreen")
  (set-face-foreground 'font-lock-builtin-face "DarkGrey")
  (set-face-foreground 'font-lock-negation-char-face "DarkGrey")
  (set-face-foreground 'font-lock-function-name-face "DarkGrey")
  (set-face-foreground 'font-lock-warning-face "DarkGrey")
  (set-face-foreground 'font-lock-string-face "#ad7fa8")
  (set-face-foreground 'font-lock-constant-face "DarkGrey")

  (require 'diff-mode)
  (set-face-background 'diff-refine-added "DarkOliveGreen")

  ;; make linum more vim like.
  (set-face-foreground 'linum "gold1")

  ;; make the completion colors closer to vim feeling.
  (set-face-background 'company-tooltip "#ad7fa8")
  (set-face-foreground 'company-tooltip-common "black")
  (set-face-background 'company-scrollbar-bg "DimGrey")
  (set-face-background 'company-scrollbar-fg "LightGrey")
  (set-face-background 'company-tooltip-selection "DimGrey")
  (set-face-foreground 'company-tooltip-selection "LightGrey")
  (set-face-background 'company-preview-common "grey20")
  (set-face-foreground 'company-preview-common "DarkGrey")

  ;; git gutter colors.
  (when window-system
    (set-face-background 'fringe "grey20")
    (set-face-attribute 'git-gutter-fr+-added nil :bold nil)
    (set-face-attribute 'git-gutter-fr+-deleted nil :bold nil)
    (set-face-attribute 'git-gutter-fr+-modified nil :bold nil))

  ;; helm colors
  (set-face-attribute 'helm-source-header nil :bold nil :height 150 :family "ubuntu mono")
  (set-face-foreground 'helm-source-header "LightGrey")
  (set-face-background 'helm-selection "LightSkyBlue")
  (set-face-foreground 'helm-selection "DarkGrey")
  (set-face-foreground 'helm-helper "LightGrey")
  (set-face-foreground 'helm-header "LightGrey")
  (set-face-foreground 'header-line "LightGrey")
  (set-face-foreground 'helm-grep-file "MediumPurple1")
  ;; helm swoop
  (set-face-background 'helm-swoop-target-word-face "grey20")
  (set-face-foreground 'helm-swoop-target-word-face "LightGoldenrod2")

  ;; vim /? search
  (set-face-foreground 'evil-ex-search "black")
  (set-face-background 'evil-ex-search "yellow2")
  (set-face-foreground 'isearch "black")
  (set-face-background 'isearch "yellow2")
  (set-face-foreground 'lazy-highlight "black")
  (set-face-background 'lazy-highlight "yellow4")

  ;; fontify code in code blocks and tables
  (setq org-src-fontify-natively t)
  (set-face-background 'org-block-background "DarkSlateGrey")
  (set-face-background 'org-block-begin-line "DarkSlateGrey")
  (set-face-background 'org-block-end-line "DarkSlateGrey")
  (set-face-foreground 'org-block-begin-line "LightSeaGreen")
  (set-face-foreground 'org-block-end-line "LightSeaGreen")
  (set-face-background 'org-table "DarkSlateGrey")
  (set-face-foreground 'org-table "DarkGrey")

  ;; Occur window colors
  (set-face-attribute 'underline nil :bold t :height 160 :family "DejaVu Sans Mono")
  (set-face-foreground 'underline "black")
  (set-face-background 'underline "yellow4")
  (set-face-foreground 'match "black")
  (set-face-background 'match "yellow4")
)

(defun xueliang/light-theme()
  (load-theme 'anti-zenburn t)

  (set-default-font "Monospace")
  (set-face-attribute 'default nil :height 110)

  ;; Useful commands: list-faces-display, counsel-colors-emacs.
  (set-face-foreground 'font-lock-comment-face "DarkGreen")
  (set-face-foreground 'font-lock-doc-face "DarkGreen")
  (set-face-foreground 'font-lock-comment-delimiter-face "DarkGreen")

  (require 'diff-mode)
  (set-face-background 'diff-refine-added "DarkOliveGreen")

  (set-face-foreground 'linum "DarkSlateBlue")

  ;; git gutter colors.
  (when window-system
    (set-face-background 'fringe "#c0c0c0")
    (set-face-attribute 'git-gutter-fr+-added nil :bold nil)
    (set-face-attribute 'git-gutter-fr+-deleted nil :bold nil)
    (set-face-attribute 'git-gutter-fr+-modified nil :bold nil))

  ;; make the completion colors closer to vim feeling.
  (set-face-background 'company-tooltip "#ad7fa8")
  (set-face-foreground 'company-tooltip-common "black")
  (set-face-background 'company-scrollbar-bg "DimGrey")
  (set-face-background 'company-scrollbar-fg "LightGrey")
  (set-face-background 'company-tooltip-selection "DimGrey")
  (set-face-foreground 'company-tooltip-selection "LightGrey")
  (set-face-background 'company-preview-common "grey20")
  (set-face-foreground 'company-preview-common "DarkGrey")

  ;; Org mode colors
  (set-face-foreground 'org-done "DarkSlateGrey")
  (setq org-src-fontify-natively t)
  (set-face-background 'org-block-background "DarkGrey")
  (set-face-background 'org-block-begin-line "DarkGrey")
  (set-face-background 'org-block-end-line "DarkGrey")
  (set-face-foreground 'org-block-begin-line "SeaGreen")
  (set-face-foreground 'org-block-end-line "SeaGreen")
  (set-face-background 'org-table "DarkSlateGrey")
  (set-face-foreground 'org-table "DarkGrey")

  ;; helm colors
  (require 'helm-command)
  (set-face-foreground 'helm-M-x-key "DarkOrange3")
  (set-face-background 'helm-selection "LightSkyBlue")
  (set-face-background 'helm-selection-line "LightSkyBlue")
  (set-face-background 'helm-match "PaleGreen3")

  ;; flyspell
  (require 'flyspell)
  (set-face-foreground 'flyspell-incorrect "DarkRed")
  (set-face-foreground 'flyspell-duplicate "DarkRed")
)

; default theme good themes: tango-dark, zenburn, monokai, wombat, heroku, anti-zenburn
(when window-system
  (if (= (x-display-pixel-width) 1920)
    (xueliang/dark-theme)   ;; home laptop
    (xueliang/light-theme)  ;; themes that's good for work at office
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xueliang's functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun =============xueliang-functions=============())

(defun xueliang-search-word-forward ()
  "swiper for small buffers, vim style / for big buffers" (interactive)
  (if (< (buffer-size) 10000000) ;; 10MB limit
      (helm-swoop-symble-pre-input)
      (evil-search-word-forward 1 (thing-at-point 'symbol))
  )
)

(defun xueliang-search-word-backward ()
  "swiper for small buffers, vim style / for big buffers" (interactive)
  (if (< (buffer-size) 10000000) ;; 10MB limit
      (helm-swoop-symble-pre-input)
      (evil-search-word-backward 1 (thing-at-point 'symbol))
  )
)

(defun xueliang-what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
                        (get-char-property (point) 'face))
                    ))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun xueliang-find-project()
  "switch among my projects" (interactive)
  (cd (helm-comp-read "Project: " xueliang-project-list))
  (xueliang-find-file-from-pwd))

(defun xueliang-find-file ()
  "my fast find file in project" (interactive)
  (require 'fiplr)
  (xueliang/find-file (fiplr-root) ""))

(defun xueliang-find-file-from-pwd ()

  (xueliang/find-file "." ""))

(defun xueliang-find-file-similar ()
  "my fast find file with similar file names, e.g. switch between .cc, .h, _test.cc files" (interactive)
  (xueliang-cd-current-buffer-directory)
  (xueliang/find-file "." (string-remove-suffix "_test"  ;; remove the test suffix for _test.cc files.
                                                (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))

(defun xueliang/find-file (path init-input)
  "my fast find file in project"
  (find-file (helm-comp-read (concat "Find File " path ": ")
                             (split-string (shell-command-to-string
                                            (concat "ag " path " -l --nocolor -g \"\" "))  ;; faster than find command.
                                           "\n")
                             :initial-input init-input)))

;;(defun xueliang-top() "my top command in emacs" (interactive) (ivy-read "Top: " (split-string (shell-command-to-string "top -b -n 1 | tail -n +6") "\n")))
(defalias 'xueliang-top 'helm-top)

(defun xueliang-google-current-word ()
  "google current word" (interactive)
  (helm-google (thing-at-point 'symbol)))

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
  (cd android-vixl) (counsel-ag arg))

; Help to make cross-project search easier.
(defun xueliang-ag-art (arg)
  "look for WORD in Android ART using ag searcher" (interactive "P")
  (cd android-art) (counsel-ag arg))

; Helper to cd to directory of current buffer/file.
(defun xueliang-cd-current-buffer-directory ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

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
(defun xueliang-eshell-pwd ()
   "invokes a new eshell in a split window, shell starts in the root of current project." (interactive)
   (xueliang-cd-current-buffer-directory)
   (setq eshell-buffer-number (% (+ eshell-buffer-number 1) 10))  ;; eshell number 0-9.
   (split-window-below) (evil-window-move-very-bottom) (eshell eshell-buffer-number)
   (evil-goto-line) (evil-append-line 1))

; search in project using ag
(defun xueliang-ag-search-in-project (argument)
  "search in project using ag; use fiplr to goto the root dir of the project"
  (interactive "P")
  (xueliang-cd-current-buffer-directory)
  (require 'fiplr) (cd (fiplr-root))
  (kill-ring-save (point) (+ (point) (length (thing-at-point 'symbol))))   ;; so that it can be pasted in helm using shift-insert.
  (helm-do-grep-ag (thing-at-point 'symbol)))
  ;;(counsel-ag (thing-at-point 'word))) ;; counsel-ag allows initial input to play with.

(setq-default xueliang-current-font 0)
(defun xueliang-switch-fonts ()
  "it's nice to be able to switch between pretty fonts.\nEspecially for writing emails, docs, and code."
  (interactive)
  (if (= (mod xueliang-current-font 2) 0)
      (set-default-font "DejaVu Sans")
      (set-default-font "DejaVu Sans Mono"))
   (setq xueliang-current-font (+ xueliang-current-font 1)))

(defun xueliang-turn-on-transparency ()
  "turn on transparency easier for any eamcs frames."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100)))

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
  (term "bash") (rename-buffer (concat "*htop-CPU%-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert "htop -d 10 --sort-key PERCENT_CPU") (term-send-input))

(defun xueliang-htop-io ()
  "invokes htop easier." (interactive)
  (term "bash") (rename-buffer (concat "*htop-IO-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert "htop -d 10 --sort-key IO") (term-send-input))

(defalias 'xueliang-htop 'helm-top)

(defun xueliang-highlight-current-word ()
  "makes highlight-regexp easier" (interactive)
  (highlight-regexp (thing-at-point 'word)))

(defun xueliang/pwd-string ()
  "get pwd string of current file-buffer easily."
  (interactive)
  (newline-and-indent)
  (insert (buffer-file-name))
  (save-excursion (newline-and-indent)))

(defun xueliang-helm-open-link ()
  "" (interactive)
  (org-open-link-from-string (car (cdr (split-string (helm-comp-read "Link: " xueliang-weblink-list))))))

(defun xueliang-daily-websites ()
  (interactive)
  ;; Work
  (org-open-link-from-string "https://mail.google.com/mail/u/0/#inbox")
  (org-open-link-from-string "https://mail.google.com/mail/u/1/#inbox")
  (org-open-link-from-string "https://outlook.office.com/owa/")
  (org-open-link-from-string "https://outlook.office.com/owa/?path=/calendar/view/Week")

  ;; Net working
  (org-open-link-from-string "https://secure.skype.com/portal/overview")
  (org-open-link-from-string "https://hipchat.arm.com/chat/room/176")
  (org-open-link-from-string "https://www.yammer.com/arm.com/")
)

(defun xueliang-daily-websites-ALL ()
  "Open all of them" (interactive)
  (dolist (link xueliang-weblink-list)
  (org-open-link-from-string (car (cdr (split-string link))))))

(defun =============xueliang-linaro-development=============())

(defun xueliang-monitor-linaro-art ()
  "Important Linaro ART related to monitor daily." (interactive)
  ;; gerrit code reviews for ART/Android
  (org-open-link-from-string "https://dev-private-review.linaro.org")
  (org-open-link-from-string "https://android-review.linaro.org/")
  (org-open-link-from-string "https://android-review.googlesource.com/")
  ;; daily build tests for ART/Android
  (org-open-link-from-string "https://ci.linaro.org/view/ART-monitor/")
  (org-open-link-from-string "https://build.chromium.org/p/client.art/console")
  (org-open-link-from-string "https://art-reports.linaro.org")
  ;; documents, BKMs, manuals, 
  (org-open-link-from-string "https://projects.linaro.org/browse/LMG-390")
  (org-open-link-from-string "https://projects.linaro.org/secure/RapidBoard.jspa")
  (org-open-link-from-string "https://wiki.linaro.org/Internal/LMG/ART-CI")
  (org-open-link-from-string "https://docs.google.com/document/d/1LLRtpbuUM6ggBUsmyBV_S_5nER5CAx1PFfAltVbMPO8")
  ;; other useful linaro links:
  ;; http://snapshots.linaro.org/android/android-generic-build/
)

(defun xueliang-linaro-art-target-test ()
  "invoke linaro host test" (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*eshell-linaro-art-target-test-optimizing" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert "cd $android-root") (eshell-send-input)
  (insert "scripts/tests/test_art_target.sh --64bit --optimizing") (eshell-send-input))

(defun xueliang-linaro-make ()
  "invoke linaro host test" (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*eshell-linaro-make-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert "cd $android-root") (eshell-send-input)
  (insert "echo y | scripts/tests/test_art_host.sh") (eshell-send-input))

(defun xueliang-make-android-system-image ()
  "invoke build android system image from andriod-root source tree" (interactive)
  (split-window-below) (evil-window-move-very-bottom)
  (term "bash") (rename-buffer (concat "*make-android-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert (message "cd %s" android-root)) (term-send-input)
  (insert "source build/envsetup.sh") (term-send-input)
  (insert "lunch 2") (term-send-input)
  ;;(insert "lunch aosp_angler-userdebug") (term-send-input)
  (insert "time make -j33") (term-send-input))

(defun xueliang-linaro-make-ALL ()
  "save me some time in building" (interactive)
  ;; just make, don't sync - repo sync may delete my local changes.
  (xueliang-make-android-system-image)
  (sleep-for 3600)  ;; wait, avoid triggering multiple build tasks.
  (xueliang-linaro-make)
)

(defun xueliang-linaro-gdb ()
  "invoke gdb linaro tree" (interactive)
  (require 'gdb-mi)
  ;; build and regenerate the test case before starting gdb.
  (cd android-xueliang_test) (shell-command (concat android-xueliang_test "/run.sh"))
  (cd android-root) (gdb-many-windows) (gdb "gdb -i=mi -x gdb.init"))

(defun xueliang/linaro-repo-sync (sync-cmd-string)
  "repo sync linaro tree"
  (split-window-below) (evil-window-move-very-bottom)
  (term "bash") (rename-buffer (concat "*repo-sync-android-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert (message "cd %s" android-root)) (term-send-input)
  (insert sync-cmd-string) (term-send-input))

(defun xueliang-linaro-repo-sync ()
  "repo sync linaro tree" (interactive)
  (xueliang/linaro-repo-sync "repo sync -j33"))

(defun xueliang-linaro-repo-sync-PINNED-MANIFEST ()
  "repo sync linaro tree with pinned manifest under $android-root/pinned-manifest.xml" (interactive)
  (xueliang/linaro-repo-sync "repo sync -d -m /home/xuezho01/workspace/linaro/pinned-manifest.xml -j33"))

(defun xueliang-cfg-analyze-c1visualizer-irhydra ()
  "analyze ART generated .cfg file" (interactive)
  ;;(org-open-link-from-string "http://mrale.ph/irhydra/2.bak/")
  (start-process "cfg-analysis" nil "~/workspace/c1visualizer/bin/c1visualizer"))

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

;; emacs style kill-line
(global-set-key (kbd "C-k") 'evil-delete-line)

;; same as the key in chrome
(global-set-key (kbd "C-q") 'xueliang-helm-open-link)

; Use default (describe-function) and (describe-variable) with ivy/counsel.
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h v") 'helm-apropos)

;; good practice for reading code.
;; (global-set-key (kbd "C-s") 'xueliang-send-current-line-to-scratch)

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
(global-set-key (kbd "<f7>")   'xueliang-linaro-make)
(global-set-key (kbd "C-<f7>") 'xueliang-make-android-system-image)

;; <f6> shows program structure in various languages.
(add-hook 'c-mode-hook '(lambda () (define-key
                                     evil-normal-state-local-map (kbd "<f6>")
                                     '(lambda() (interactive) (helm-swoop :$query "void visit ([a-z]* ")))))

(add-hook 'c++-mode-hook '(lambda () (define-key
                                       evil-normal-state-local-map (kbd "<f6>")
                                       '(lambda() (interactive) (helm-swoop :$query "void visit ([a-z]* ")))))

(add-hook 'emacs-lisp-mode-hook '(lambda () (define-key
                                              evil-normal-state-local-map (kbd "<f6>")
                                              '(lambda() (interactive) (helm-swoop :$query "(defun =[=]* ")))))

(add-hook 'magit-diff-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                              (kbd "<f6>")
                                              '(lambda() (interactive) (helm-swoop :$query "modified ")))))

(add-hook 'diff-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                        (kbd "<f6>")
                                        '(lambda() (interactive) (helm-swoop :$query "diff ")))))

; <f9> .. <f12>: 
(global-set-key (kbd "<f9>")  'xueliang-find-file-similar)
(global-set-key (kbd "<f10>") 'ace-window)
(global-set-key (kbd "<f11>") 'xueliang-helm-open-link)
(global-set-key (kbd "<f12>") 'helm-google-suggest)   ;; F12 - search the web with google.
