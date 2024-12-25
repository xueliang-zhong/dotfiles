;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

;;
;; Global Settings - after evil has loaded seems a good time to set these things
;;
(after! evil
  (toggle-frame-maximized)

  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-shift-width 2)

  (setq doom-theme 'doom-one) ;; or doom-bluloco-{dark,light}

  (setq doom-font (cond
                   ((string-equal system-type "darwin") (font-spec :family "JetBrains Mono" :size 16))
                   ((string-equal system-type "gnu/linux") (font-spec :family "Monospace" :size 24))
                   ((string-equal system-type "windows-nt") (font-spec :family "JetBrains Mono" :size 27))))
)

;;
;; evil normal mode settings
;;
;; (define-key evil-normal-state-map (kbd "/") 'counsel-grep-or-swiper)
(define-key evil-visual-state-map (kbd "s-x") 'counsel-M-x)

;;
;; evil insert mode settings
;;
;; modern style 'paste' in evil insert mode.
(define-key evil-insert-state-map (kbd "C-v") #'yank)
(define-key evil-insert-state-map (kbd "TAB") #'(lambda() (interactive) (insert "  ")))

;; MacBook UK layout '#' symbol
(define-key evil-insert-state-map (kbd "M-3") #'(lambda() (interactive) (insert "#")))

;;
;; Leader key bindings
;;
(map! :leader
      "SPC" #'counsel-switch-buffer
      "bs"  #'xueliang-open-scratch-buffer-window
      "bw"  #'read-only-mode
      "ff"  #'xueliang-find-file-in-project
      "gg"  #'xueliang-magit-status-window
      "th"  #'hl-line-mode
      "tf"  #'toggle-frame-fullscreen
      "/"   #'counsel-grep-or-swiper
      "x"   #'counsel-M-x
      "scp" #'xueliang-scp_sync-script
      "wg"  #'golden-ratio
      "w="  #'balance-windows
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
                               (kbd "<f7>")      #'xueliang-refresh
                               (kbd "<f8>")      #'xueliang-org-focus-tasks)
                             (evil-define-key 'insert evil-org-mode-map
                               (kbd "M-<left>")  #'org-shiftmetaleft
                               (kbd "M-<right>") #'org-shiftmetaright)
                             ;; face settings
                             (set-face-attribute 'org-level-1 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-2 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-3 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-4 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-5 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-level-6 nil :bold nil :height 1.0)
                             (set-face-attribute 'org-link    nil :bold nil :height 1.0)
                             (set-face-attribute 'org-table   nil :bold nil :height 1.0)
                             ;; light mode settings
                             (when (eq 'light (frame-parameter nil 'background-mode))
                             (set-face-attribute 'org-level-3 nil :foreground "#275fe4")
                             (set-face-attribute 'org-level-4 nil :foreground "#23974a")
                             (set-face-attribute 'font-lock-variable-name-face nil :foreground "DarkSlateBlue"))
))


;;
;; ivy/counsel settings
;;
(after! ivy
  (setq counsel-grep-swiper-limit 30000000)
  (define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)
  (define-key ivy-mode-map (kbd "TAB") 'ivy-next-line)
)

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
(add-hook 'eshell-mode-hook #'(lambda () (setq-local company-idle-delay 600)))  ;; company auto complete can be annoying in eshell
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-r") #'counsel-esh-history)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window)))

;;
;; Magit
;;
(after! magit
  ;; The h,j,k,l keys should be for basic moving around, even in magit buffer
  (define-key magit-mode-map (kbd "l") 'evil-forward-char)
  (define-key magit-mode-map (kbd "h") 'evil-backward-char)
)

;; script
;; <leader>cc and C-c C-c both trigger compile
(add-hook 'sh-mode-hook #'(lambda () (define-key evil-normal-state-map (kbd "C-c C-c") #'+ivy/compile)))

;;
;; Function Keys
;;
(global-set-key (kbd "<f2>")  #'xueliang-T-open-T-in-browser)
(global-set-key (kbd "<f3>")  #'xueliang-org-zen-mode-start-present)
(global-set-key (kbd "<f4>")  #'evil-window-delete)
(global-set-key (kbd "<f5>")  #'xueliang-eshell-popup)
(global-set-key (kbd "<f6>")  #'counsel-yank-pop)
(global-set-key (kbd "<f7>")  #'xueliang-refresh)
(global-set-key (kbd "<f8>")  #'counsel-imenu)
(global-set-key (kbd "<f9>")  #'xueliang-find-file-in-project)
(global-set-key (kbd "<f10>") #'counsel-switch-buffer)
(global-set-key (kbd "<f11>") #'xueliang-duckduckgo-search)
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

(defun xueliang-eshell-popup ()
   "Invokes a new eshell in a popup window and ready for command" (interactive)
   (xueliang-cd-current-dir) (+evil/window-split-and-follow) (eshell) (evil-append-line 1))

(defun xueliang-open-link-in-browser ()
   "F12 to select from a list of favourite links to open." (interactive)
   (setq weblink-list
         (with-temp-buffer (insert-file-contents "~/Dropbox/xzhong-links.txt")
                           (split-string (buffer-string) "\n" t)))
   (setq-local xueliang-weblink-str (nth 1 (split-string (ivy-read "Link: " weblink-list))))
   (org-link-open-from-string xueliang-weblink-str))

(defun xueliang-T-open-T-in-browser () (interactive)
  ;; only enable this on my MacOS
  (when (string-equal system-type "darwin")
        (setq ticker (thing-at-point 'word))
        (unless ticker (setq ticker "SPY"))
        (setq sc-string "https://stockcharts.com/h-sc/ui?s=%s")
        (org-link-open-from-string (message sc-string ticker))))


(defun xueliang-org-find-today () (interactive)
   (setq-local date-string (format-time-string "<%Y-%m-%d %a>" (current-time)))
   (goto-char (point-min))
   (unless (search-forward date-string nil t)
     (goto-char (point-max)) (insert (format "\n\n* %s\n" date-string)))
   (swiper date-string)
   (evil-ex-nohighlight) (evil-force-normal-state))

(defun xueliang-replace-tab-trailing-spaces()
   "Easily replace all TAB in current buffer with spaces." (interactive)
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace))

(defun xueliang-split-words-to-lines (start end)
  "Easily split tab or space separated words in the region into multiple lines."
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "[ \t]" end t) (replace-match "\n")))

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
  (mapcar 'org-open-link-from-string (split-string (shell-command-to-string "head -n 11 ~/Dropbox/daily_work_2024.org | tail -n 8"))))

(defun xueliang-doom-emacs-doctor()
  "Run the doctor command" (interactive)
  (xueliang-eshell-popup) (insert "~/.config/emacs/bin/doom doctor") (eshell-send-input))

(defun xueliang-open-scratch-buffer-window ()
  "Open scratch buffer window" (interactive)
  (evil-window-vsplit) (+evil/window-move-right) (doom/switch-to-scratch-buffer))

(defun xueliang-magit-status-window ()
  (interactive)
  (xueliang-cd-current-dir) (+evil/window-vsplit-and-follow) (magit-status))

(defun xueliang-refresh () (interactive)
  (xueliang-cd-current-dir) (evil-force-normal-state)
  (doom/reload-theme) (set-auto-mode) (hl-line-mode -1)
  (message "Refresh!"))

(defun xueliang-duckduckgo-search ()
  (interactive)
  (let ((my-word (if (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'word t))))
    (if my-word
        (org-link-open-from-string (concat "https://duckduckgo.com/?q=" (url-hexify-string my-word)))
        (org-link-open-from-string "https://duckduckgo.com/aichat"))))

(defun xueliang-open-notes-app ()
  (interactive)
  (when (string-equal system-type "darwin")
        (setq app-cmd (message "open -a Notes"))
        (xueliang-eshell-popup) (insert app-cmd) (eshell-send-input) (evil-window-delete)
        (evil-force-normal-state) (message app-cmd))
  (when (string-equal system-type "windows-nt") (org-link-open-from-string "https://www.icloud.com/notes"))
  (when (string-equal system-type "gnu/linux") (org-link-open-from-string "https://www.icloud.com/notes")))

(defun Gcommit-xueliang-gcommit-git-commit ()
  "Support :Gcommit similar to vim." (interactive)
  (xueliang-cd-current-dir) (setq os-name system-type)
  (when (string-equal system-type "darwin") (setq os-name "MacOS"))
  (setq commit-msg (message "git commit -m \"Update %s on %s.\"" (file-name-nondirectory buffer-file-name) os-name))
  (xueliang-eshell-popup) (insert commit-msg) (eshell-send-input) (evil-window-delete)
  (evil-force-normal-state) (message "Git Commit: %s" commit-msg))

(defun Gwrite-xueliang-gwrite-git-add ()
  "Support :Gwrite similar to vim." (interactive)
  (xueliang-cd-current-dir) (save-buffer)
  (setq git-cmd (message "git add %s" (file-name-nondirectory buffer-file-name)))
  (xueliang-eshell-popup) (insert git-cmd) (eshell-send-input) (evil-window-delete)
  (evil-force-normal-state) (message git-cmd))

(defun xueliang-create-regular-routine (my-routine-text days)
  "Generate a 6 months' regular routine based on the input and days (frequency)."
  (let* ((routine-text my-routine-text)
         (current-date (current-time))
         (output-buffer (generate-new-buffer "*Weekly Routine*")))
    (with-current-buffer output-buffer
      (dotimes (i 24) ; 6 months * 4 weeks/month approximately
        (insert "* " (format-time-string "<%Y-%m-%d %a>" current-date) "\n")
        (insert "** " routine-text "\n\n")
        (setq current-date (time-add current-date (days-to-time days))))
      (display-buffer output-buffer) (org-mode))))

(defun xueliang-generate-DAY-TO-DAY-routine (start end)
  "Generate a 6 month weekly routine based on the selected region."
  (interactive "r") (setq-local my-routine-text (buffer-substring-no-properties start end))
  (xueliang-create-regular-routine my-routine-text 1))

(defun xueliang-generate-WEEKLY-routine (start end)
  "Generate a 6 month weekly routine based on the selected region."
  (interactive "r")
  (setq-local my-routine-text (buffer-substring-no-properties start end))
  (xueliang-create-regular-routine my-routine-text 7))

(defun xueliang-org-focus-tasks () (interactive)
  ;; Search for FOCUS tasks in org mode, helping roll over tasks to
  ;; current/future days.
  (setq-local date-string (format-time-string "<%Y-%m-%d %a>" (current-time)))
  (setq-local my-task-list
              (if (eq 'dark (frame-parameter nil 'background-mode))
                  ;; pretty colours for dark theme
                  (list (propertize date-string 'face '(:foreground "LightGoldenrod" :background "grey15"))
                        (propertize "FOCUS" 'face '(:foreground "GreenYellow" :background "grey15"))
                        (propertize "TODO" 'face '(:foreground "orchid2" :background "grey15"))
                        (propertize "PROG" 'face '(:foreground "LightGoldenrod" :background "grey15"))
                        (propertize "DONE" 'face '(:foreground "grey51" :background "grey15")))
                  ;; pretty colours for light theme
                  (list (propertize date-string 'face '(:foreground "DarkGoldenrod3"))
                        (propertize "FOCUS" 'face '(:foreground "ForestGreen"))
                        (propertize "TODO" 'face '(:foreground "DeepPink1"))
                        (propertize "PROG" 'face '(:foreground "DarkGoldenrod3"))
                        (propertize "DONE" 'face '(:foreground "grey51")))))
  (swiper (concat "^ * " (ivy-read "Task: " my-task-list)))
  (evil-ex-nohighlight))

(defun xueliang-scp_sync-script () (interactive)
       (find-file-other-window "~/workspace/local_dir/scp_sync.sh")
       (+evil/window-move-right)
)

(defun eshell/e (f) (find-file-other-frame f))
(defalias 'eshell/vi 'eshell/e)
(defalias 'eshell/vim 'eshell/e)

;;
;; My Alias Functions
;;
(defalias 'xueliang-sort 'org-sort)
(defalias 'xueliang-capitalize-region 'capitalize-region)
