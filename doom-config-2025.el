;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (cond
                 ;; quickly look for fonts: counsel-fonts
                ((string-equal system-type "darwin") (font-spec :family "JetBrains Mono" :size 18 :weight 'regular))
                ((string-equal system-type "gnu/linux") (font-spec :family "Monospace" :size 24))
                ((string-equal system-type "windows-nt") (font-spec :family "JetBrains Mono" :size 30))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

(map! :leader
      "SPC" #'ivy-switch-buffer
      "bs"  #'xueliang-open-scratch-buffer
      "bw"  #'read-only-mode
      "gg"  #'xueliang-magit-status
      "/"   #'counsel-grep-or-swiper
      "RET" #'counsel-recentf
)

;;
;; Function Keys
;;
(global-set-key (kbd "<f3>")  #'xueliang-treemacs)
(global-set-key (kbd "<f4>")  #'evil-window-delete)
(global-set-key (kbd "<f5>")  #'xueliang-eshell-popup)
(global-set-key (kbd "<f6>")  #'counsel-yank-pop)
(global-set-key (kbd "<f7>")  #'xueliang-eshell-just-make)
(global-set-key (kbd "<f8>")  #'xueliang-imenu-or-org-today)
(global-set-key (kbd "<f9>")  #'xueliang-treemacs)
(global-set-key (kbd "<f10>") #'counsel-switch-buffer)
(global-set-key (kbd "<f12>") #'xueliang-open-link-in-browser)
(global-set-key (kbd "C-<f4>") #'kill-buffer-and-window)

;;
;; evil
;;
(after! evil
  (toggle-frame-maximized)

  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-shift-width 2)

  (define-key evil-visual-state-map (kbd "s-x") #'counsel-M-x)
  (define-key evil-insert-state-map (kbd "TAB") #'(lambda() (interactive) (insert "  ")))

  ;; Windows:
  ;; modern style 'paste' in evil insert mode.
  (define-key evil-insert-state-map (kbd "C-v") #'yank)

  ;; Mac:
  ;; MacBook UK layout '#' symbol
  (define-key evil-insert-state-map (kbd "M-3") #'(lambda() (interactive) (insert "#")))
)

;;
;; ivy/counsel settings
;;
(after! ivy
  (setq counsel-grep-swiper-limit 30000000)
  (define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)
  (define-key ivy-mode-map (kbd "TAB") 'ivy-next-line)
)

;;
;; eshell settings
;;
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-r") #'cape-history)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)))
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window)))
;; to be consistent with my vim/nvim/tmux: in shell, F1 acts like ESC key
(add-hook 'eshell-mode-hook #'(lambda () (define-key evil-insert-state-local-map (kbd "<f1>") #'evil-force-normal-state)))

;;
;; Magit
;;
;; The h,j,k,l keys should be for basic moving around, even in magit buffer
(after! magit-mode
  (define-key magit-mode-map (kbd "h") 'evil-backward-char)
  (define-key magit-mode-map (kbd "l") 'evil-forward-char)
)

;;
;; Auto completion
;;
(setq-default completion-at-point-functions
            (list
             #'cape-file         ; File path completion.
             #'cape-dabbrev      ; Dynamic abbrev completion (from all buffers).
             #'cape-keyword      ; Programming language keywords.
             #'yasnippet-capf)
)

(setq-default cape-dabbrev-min-length 1)
(setq-default corfu-auto-delay 0.1)

;;
;; Org
;;
(add-hook 'org-mode-hook #'xueliang-org-refresh)

;;
;; My own functions
;;
(defun xueliang-cd-current-dir ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

(defun xueliang-eshell-popup ()
   "Invokes a new eshell in a popup window and ready for command" (interactive)
   (xueliang-cd-current-dir)
   (+evil/window-split-and-follow) ;; more natural position of eshell window
   (eshell) (evil-append-line 1)
   ;; avoid auto completion in eshell
   (setq-local cape-dabbrev-min-length 100)
   (setq-local corfu-auto-delay 100)
   ;; make eshell window more prominent among other emacs windows;
   ;; this colour setting should work with both light and dark themes
   (face-remap-add-relative 'default :background "grey9" :foreground "grey75")
)

(defun xueliang-eshell-just-make ()
   "Invokes a new eshell in a popup window and ready for command" (interactive)
   (xueliang-org-refresh) ;; just a good point to do my org refresh
   (xueliang-eshell-popup)
   (insert "just all") (eshell-send-input))

(defun xueliang-eshell-fzf ()
  (xueliang-open-scratch-buffer) (counsel-fzf))

(defun xueliang-open-scratch-buffer ()
  "Open scratch buffer in my preferred layout" (interactive)
  (xueliang-cd-current-dir) (+evil/window-vsplit-and-follow)
  (doom/open-scratch-buffer nil nil t))

(defun xueliang-magit-status ()
  "my :Git" (interactive)
  (xueliang-open-scratch-buffer) (magit-status-setup-buffer))

(defun xueliang-org-refresh ()
   "reset my org-mode settings" (interactive)
  (xueliang-cd-current-dir) (evil-force-normal-state)
  ;; appearance
  (toggle-truncate-lines 1) (hl-line-mode -1)
  (org-superstar-mode 1) (xueliang-setup-org-superstar)
  (setq org-ellipsis " ▶")
  ;; key bindings
  (evil-define-key 'normal evil-org-mode-map (kbd "RET") #'xueliang-org-open-at-point)
  ;; better org-mode CMD key behaviour on MacOS
  (global-set-key (kbd "s-<right>")  'org-metaright)
  (global-set-key (kbd "s-<left>")   'org-metaleft)
  (global-set-key (kbd "s-<return>") 'org-meta-return)
  ;; misc
  (setq org-todo-keywords   ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "FOCUS(f)"     ; A task that I am focusing
             "TODO(t)"      ; A task that is ready to be tackled
             "PROG(p)"      ; A task that is in progress but no need to focus right now
             "|"            ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"      ; Task has been completed
             )))
  ;; face settings
  (set-face-attribute 'org-level-1 nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-level-2 nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-level-3 nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-level-4 nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-level-5 nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-level-6 nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-link    nil :bold nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-table   nil :bold nil :height 1.0 :weight 'medium)

  (message "Refresh!"))

(defun xueliang-setup-org-superstar ()
  "Custom configuration for org-superstar-mode."
  (org-superstar-mode 1)
  (setq
   org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
   org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓))))

(defun xueliang-imenu-or-org-today ()
  "" (interactive)
  (if (derived-mode-p 'org-mode)
      (swiper (format-time-string "<%Y-%m-%d %a>"))
      (counsel-imenu)))

(defun xueliang-treemacs ()
  "my treemacs settings" (interactive)
  (+treemacs/toggle) (treemacs-follow-mode t) (treemacs-tag-follow-mode t)
  (setq treemacs-tag-follow-delay 0.3))

(defun xueliang-org-open-at-point()
  "Open links in org-mode headings, otherwise just behave like dwim-at-point." (interactive)
  (when (derived-mode-p 'org-mode)
    (if (string-match "^\*[\*]* " (thing-at-point 'line))
        (org-open-at-point) (+org/dwim-at-point))))

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

(defun xueliang-open-knowledge-links ()
  "My knowledge links quick open" (interactive)
  (setq-local current-buffer-string (split-string (buffer-string) "\n" t))
  (setq-local url-list (cl-remove-if-not (lambda (line) (or (string-match "http" line) (string-match "file:" line))) current-buffer-string))
  (setq-local xueliang-url-str (nth 1 (split-string (ivy-read "Knowledge Link: " url-list))))
  (org-link-open-from-string xueliang-url-str))

(defun xueliang-open-link-in-browser ()
   "F12 to select from a list of favourite links to open." (interactive)
   (setq weblink-list
         (with-temp-buffer (insert-file-contents "~/Dropbox/xzhong-links.txt")
                           (split-string (buffer-string) "\n" t)))
   (setq-local xueliang-weblink-str (nth 1 (split-string (ivy-read "Link: " weblink-list))))
   (org-link-open-from-string xueliang-weblink-str))

;;
;; Some useful alias
;;
(defalias 'xueliang-cap-region 'capitalize-region)
(defalias 'xueliang-org-sort   'org-sort)
(defalias 'eshell/e   'find-file-other-window)
(defalias 'eshell/vi  'eshell/e)
(defalias 'eshell/vim 'eshell/e)
(defalias 'eshell/f   'xueliang-eshell-fzf)
(defalias 'eshell/fzf 'eshell/f)

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
