;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;
;; Global Settings
;;
(toggle-frame-maximized)
(add-hook 'prog-mode-hook #'(lambda() (interactive) (toggle-truncate-lines 1)))

;;
;; MacOS Settings
;;
(when (string-equal system-type "darwin")
  (setq doom-font (font-spec :family "JetBrains Mono" :size 19)) ;; Menlo
)

;;
;; Linux Settings
;;
(when (string-equal system-type "gnu/linux")
  (setq doom-theme 'doom-one-light)
  (setq doom-font (font-spec :family "Monospace" :size 21))
)

;;
;; Windows Settings
;;
(when (string-equal system-type "windows-nt")
  (setq doom-theme 'doom-one-light)
  (setq doom-font (font-spec :family "Consolas" :size 21))
)

;;
;; evil normal mode settings
;;
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)
(setq evil-shift-width 2)
(define-key evil-normal-state-map (kbd "/") 'counsel-grep-or-swiper)
(define-key evil-visual-state-map (kbd "s-x") 'counsel-M-x)

;;
;; evil insert mode settings
;;
;; modern style 'paste' in evil insert mode.
(define-key evil-insert-state-map (kbd "C-v") #'yank)
(define-key evil-insert-state-map (kbd "TAB") #'(lambda() (interactive) (insert "  ")))

;;
;; Leader key bindings
;;
(map! :leader
      "SPC" #'ivy-switch-buffer
      "bs"  #'doom/open-scratch-buffer
      "bw"  #'read-only-mode
      "ff"  #'xueliang-find-file-in-project
      "gg"  #'xueliang-magit-status-window
      "th"  #'hl-line-mode
)

;;
;; Org mode settings
;;
(after! org
  (setq
   org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
   org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓)) ; changes +/- symbols in item lists
   org-ellipsis " ▶"
   org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("DDG" . "https://duckduckgo.com/?q=")
            ("SC" . "https://stockcharts.com/h-sc/ui?s=%s")
            ("FV" . "https://elite.finviz.com/quote.ashx?t=%s")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
   org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "FOCUS(f)"     ; A task that I am focusing
             "TODO(t)"      ; A task that is ready to be tackled
             "PROG(p)"      ; A task that is in progress but no need to focus right now
             "|"            ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"      ; Task has been completed
  )))
)
;; Make sure these settings are called only when org-mode are loaded, and when I'm entering org
(add-hook 'org-mode-hook #'(lambda()
                             (toggle-truncate-lines 1)
                             (org-superstar-mode 1)
                             ;; key bindings
                             (evil-define-key 'normal evil-org-mode-map
                               (kbd "<return>")  #'xueliang-org-open-at-point
                               (kbd "RET")       #'xueliang-org-open-at-point
                               (kbd "<f8>")      #'xueliang-org-find-today)
                             ;; face settings
                             (set-face-attribute 'org-level-1 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-2 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-3 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-4 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-5 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-6 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-link    nil :bold nil :height 1.0)
                             (set-face-attribute 'org-table   nil :bold nil :height 1.0)))

;;
;; ivy/counsel settings
;;
(setq counsel-grep-swiper-limit 30000000)
(define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)
(define-key ivy-mode-map (kbd "TAB") 'ivy-next-line)

;;
;; Compoany Mode Settings
;;
(after! company
  (setq
   company-dabbrev-code-other-buffers 'all
   company-dabbrev-char-regexp "[\\0-9a-zA-Z-_'/]"
   company-idle-delay 0
   company-tooltip-minimum 9
   company-tooltip-limit 9
   company-tooltip-minimum-width 33
   company-minimum-prefix-length 1)
  (setq-default company-backends '(company-files company-capf company-dabbrev-code company-dabbrev company-gtags company-keywords))
  (global-company-mode))

;;
;; eshell settings
;;
(add-hook 'eshell-mode-hook #'(lambda () (setq-local company-idle-delay 600)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-r") #'counsel-esh-history)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window)))

;;
;; Function Keys
;;
(global-set-key (kbd "<f2>")  #'xueliang-T-open-T-in-browser)
(global-set-key (kbd "<f4>")  #'evil-window-delete)
(global-set-key (kbd "<f5>")  #'xueliang-eshell-pwd)
(global-set-key (kbd "<f6>")  #'counsel-yank-pop)
(global-set-key (kbd "<f8>")  #'counsel-semantic-or-imenu)
(global-set-key (kbd "<f9>")  #'xueliang-find-file-in-project)
(global-set-key (kbd "<f12>") #'xueliang-open-link-in-browser)

(global-set-key (kbd "C-<f4>") #'kill-buffer-and-window)

;;
;; My own functions
;;
(defun xueliang-cd-current-dir ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

(defun xueliang-find-file () (interactive)
  (xueliang-cd-current-dir) (counsel-find-file))

(defun xueliang-eshell-pwd ()
   "Invokes a new eshell in a split window." (interactive)
   (xueliang-cd-current-dir)
   (split-window-below) (evil-window-move-very-bottom) (eshell)
   (evil-goto-line) (evil-append-line 1))

(defun xueliang-open-link-in-browser ()
   "F12 to select from a list of favourite links to open." (interactive)
   (setq weblink-list
         (with-temp-buffer (insert-file-contents "~/Dropbox/vim/xzhong-links.txt")
                           (split-string (buffer-string) "\n" t)))
   (setq-local xueliang-weblink-str (nth 1 (split-string (ivy-read "Link: " weblink-list))))
   (org-link-open-from-string xueliang-weblink-str))

(defun xueliang-T-open-T-in-browser () (interactive)
  (setq ticker (thing-at-point 'word))
  (unless ticker (setq ticker "SPY"))
  (org-link-open-from-string (message "https://elite.finviz.com/quote.ashx?t=%s" ticker))
  (org-link-open-from-string (message "https://stockcharts.com/h-sc/ui?s=%s" ticker)))

(defun xueliang-org-find-today () (interactive)
  (org-shifttab) (evil-goto-first-line)
  (swiper (format-time-string "<%Y-%m-%d" (current-time)))
  (org-cycle) (evil-ex-nohighlight))

(defun xueliang-replace-tab-trailing-spaces()
   "Easily replace all TAB in current buffer with spaces." (interactive)
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace))

(defun xueliang-what-face (pos)
  "Show the face under current cursor" (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name) (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun xueliang-org-open-at-point()
  "Open links in org-mode headings, otherwise just behave like dwim-at-point." (interactive)
  (when (string-equal major-mode "org-mode")
    (if (string-match "^\*[\*]* " (thing-at-point 'line))
        (org-open-at-point) (+org/dwim-at-point))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

(defun xueliang-find-file-in-project ()
   "projectile-find-file if it's in a project, find-file otherwise" (interactive)
   (xueliang-cd-current-dir)
   (if (projectile-project-root) (counsel-projectile-find-file) (counsel-find-file)))

(defun xueliang-daily-website ()
  "Open daily website more easily" (interactive)
  (mapcar 'org-open-link-from-string (split-string (shell-command-to-string "head -n 8 ~/Dropbox/daily_2022.org | tail -n 5"))))

(defun xueliang-magit-status-window ()
  (interactive)
  (xueliang-cd-current-dir) (+evil/window-vsplit-and-follow) (magit-status))

