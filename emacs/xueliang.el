;;; xueliang.el --- My Emacs habits, framework-neutral -*- lexical-binding: t; -*-
(defconst xl-packages                   ; must stay the first form: shims read it
  '(ivy counsel ivy-rich ivy-prescient company
    magit evil-org org-superstar golden-ratio spacemacs-theme doom-themes
    just-mode lua-mode graphviz-dot-mode))
;; Loaded last by a framework shim (emacs/crafted/init.el, ...) which must
;; provide `xl-leader-map' and install `xl-packages'.  Only plain Emacs and
;; package APIs here: no spacemacs/, doom, crafted- symbols.

;;; Platform, paths, appearance
(defconst xl-mac-p (eq system-type 'darwin))
(defvar xl-dotfiles-dir "~/workspace/dotfiles/")
(defvar xl-links-file "~/workspace/xzhong-links.txt")
(defvar xl-mindmap-dir "~/workspace/mindmap/")
(defvar xl-yi-file "~/workspace/life-notes/YI/YI.el")
(let ((local (expand-file-name "~/.emacs-profiles/local.el"))) ; per-machine paths
  (when (file-exists-p local) (load local nil t)))

(when (display-graphic-p)
  (let ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                        '("JetBrains Mono" "Menlo" "Consolas" "DejaVu Sans Mono"))))
    (when font
      (set-face-attribute 'default nil :family font
                          :height (if (eq system-type 'windows-nt) 120 180))))
  (set-frame-parameter nil 'fullscreen 'maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(or (ignore-errors (load-theme 'spacemacs-dark t))
    (ignore-errors (load-theme 'doom-tokyo-night t)))
(when xl-mac-p (keymap-global-set "s-x" #'xl-M-x))

;;; Plain Emacs options
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t
      initial-major-mode 'text-mode
      recentf-max-saved-items 1000
      recentf-max-menu-items 50
      recentf-auto-cleanup 'never
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      read-process-output-max (* 1024 1024)
      which-key-idle-delay 0.3
      which-key-sort-order 'which-key-key-order
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1)
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              scroll-margin 0
              indent-tabs-mode nil
              tab-width 2
              standard-indent 2
              split-width-threshold 160) ; works better for org-timestamp
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode -1)
(dolist (hook '(dired-mode-hook org-mode-hook prog-mode-hook))
  (add-hook hook (lambda () (setq truncate-lines t))))

(defun xueliang-create-missing-directories-h ()
  "Create missing parent directories before saving current file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook #'xueliang-create-missing-directories-h)

;;; Intents: one name per habit, whichever completion stack is installed
(defun xl-pick (&rest commands) (seq-find #'fboundp commands))
(defalias 'xl-switch-buffer (xl-pick 'ivy-switch-buffer 'switch-to-buffer))
(defalias 'xl-recent-file (xl-pick 'counsel-recentf 'recentf-open))
(defalias 'xl-imenu (xl-pick 'imenu 'counsel-imenu))
(defalias 'xl-yank-pop (xl-pick 'counsel-yank-pop 'yank-pop))
(defalias 'xl-history (xl-pick 'counsel-esh-history 'eshell-list-history))
(defalias 'xl-find (xl-pick 'counsel-fzf 'find-file))
(defalias 'xl-M-x (xl-pick 'counsel-M-x 'execute-extended-command))

(defun xl-complete-file ()
  "In-buffer file path completion (company backend)."
  (interactive)
  (require 'company)
  (company-files))

(defun xl-search-line (&optional initial)
  "Search lines of this buffer, starting from INITIAL."
  (interactive)
  (cond ((fboundp 'swiper) (swiper initial))
        (t (isearch-forward))))

(defun xueliang-read-in-order (prompt candidates &optional initial)
  "Pick one of CANDIDATES with PROMPT, keeping their order (the UI may re-sort)."
  (completing-read prompt
                   (lambda (str pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity))
                       (complete-with-action action candidates str pred)))
                   nil nil initial))

;;; Evil
(defface xueliang-yank-flash '((t (:background "#e0af68" :foreground "#1a1b26")))
  "Face for flashing yanked text." :group 'faces)
(defun xueliang-flash-yanked-text (beg end &rest _)
  (when (called-interactively-p 'any)
    (pulse-momentary-highlight-region beg end 'xueliang-yank-flash)))
(defun xueliang-split-right () (interactive) (select-window (split-window-right)))
(defun xueliang-split-below () (interactive) (select-window (split-window-below)))

(with-eval-after-load 'evil
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-shift-width 2
        evil-want-fine-undo t
        evil-flash-delay 0.15)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (define-key evil-insert-state-map (kbd "C-v") #'yank)
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  (define-key evil-insert-state-map (kbd "TAB") (lambda () (interactive) (insert "  ")))
  (define-key evil-insert-state-map (kbd "C-x C-f") #'xl-complete-file)
  (define-key evil-insert-state-map (kbd "C-x C-l") #'xueliang-complete-line)
  (define-key evil-normal-state-map (kbd "C-x C-l") #'xueliang-complete-line)
  ;; vim habit: C-u scrolls up half a page (C-d already scrolls down).
  ;; Direct rebind: `evil-want-C-u-scroll' must be set before evil loads,
  ;; which is too late here. Motion state keeps C-u as universal-argument.
  (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") #'evil-scroll-up)
  (dolist (cmd '("vs" "vsplit")) (evil-ex-define-cmd cmd #'xueliang-split-right))
  (dolist (cmd '("sp" "split")) (evil-ex-define-cmd cmd #'xueliang-split-below))
  (advice-add 'evil-yank :after #'xueliang-flash-yanked-text)
  ;; IME: back to the English layout when leaving insert state (macOS + macism)
  (when-let* ((macism (or (executable-find "macism")
                          (car (file-expand-wildcards "~/.local/bin/macism")))))
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (call-process macism nil 0 nil "com.apple.keylayout.British")))))

;;; Completion: minibuffer (ivy & counsel), in-buffer popup (company)
(require 'counsel)
(fido-mode -1) (icomplete-mode -1)      ; plain-Emacs completion UIs, off under ivy
(ivy-mode 1) (counsel-mode 1)
(when (fboundp 'ivy-rich-mode) (ivy-rich-mode 1))
(when (fboundp 'ivy-prescient-mode)
  (ivy-prescient-mode 1) (prescient-persist-mode 1))
(keymap-set ivy-minibuffer-map "C-k" #'kill-line)
(setq-default company-dabbrev-char-regexp "[\\0-9a-zA-Z-_'/]"
              company-idle-delay 0
              company-minimum-prefix-length 1
              company-tooltip-align-annotations t
              company-require-match 'never
              company-selection-wrap-around t
              company-global-modes '(not eshell-mode comint-mode ielm-mode)
              company-backends '(company-capf company-dabbrev company-files))
(setq company-transformers '(company-sort-by-occurrence))
(global-company-mode 1)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
(add-hook 'just-mode-hook
          (lambda ()
            (company-mode 1)
            (setq-local company-backends '(company-files company-capf company-dabbrev))))

;;; Dired
(setq dired-use-ls-dired nil)
(setq-default dired-listing-switches "-alh")
(with-eval-after-load 'dired
  (require 'dired-x)
  (evil-define-key 'normal dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "TAB") #'dired-display-file)
  (define-key dired-mode-map (kbd "RET") #'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "<return>") #'dired-find-file-other-window))

;;; Eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            ;; RET in insert state submits the command. Newer evil-collection
            ;; binds it to `newline' (repl-newline theme), which silently
            ;; swallows the command - restore the old behavior.
            (define-key evil-insert-state-local-map (kbd "RET") #'eshell-send-input)
            (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)
            (define-key evil-insert-state-local-map (kbd "C-k") #'kill-line)
            (define-key evil-insert-state-local-map (kbd "C-r") #'xl-history)
            (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)
            (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window)
            (define-key evil-insert-state-local-map (kbd "<f1>") #'evil-force-normal-state)
            (evil-insert-state)))
(defalias 'eshell/e   #'find-file-other-window)
(defalias 'eshell/vi  #'eshell/e)
(defalias 'eshell/vim #'eshell/e)
(defalias 'eshell/fzf #'xl-find)

;;; Org: set once when org loads, only buffer-local work in the hook
(with-eval-after-load 'org
  (setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓))
        org-ellipsis " ▶"
        org-log-done nil
        org-hide-emphasis-markers nil
        org-link-abbrev-alist '(("SC"    . "https://stockcharts.com/h-sc/ui?s=%s")
                                ("piano" . "https://www.scales-chords.com/chord/piano/%s"))
        org-todo-keywords '((sequence "FOCUS(f)" "TODO(t)" "STORY(s)" "PROG(p)" "|" "DONE(d)"))
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-pretty-entities nil
        org-confirm-babel-evaluate nil
        org-file-apps '(("\\.svg\\'" . default) (auto-mode . emacs)
                        (directory . emacs) (remote . emacs)))
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  (define-key org-mode-map (kbd "s-<right>")  #'org-metaright)
  (define-key org-mode-map (kbd "s-<left>")   #'org-metaleft)
  (define-key org-mode-map (kbd "s-<return>") #'org-meta-return)
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                  org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight 'regular))
  (advice-add 'org-open-at-point :around #'xueliang-org-open-at-point-split-advice))
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (when (fboundp 'org-superstar-mode) (org-superstar-mode 1))
            (when (fboundp 'evil-org-mode) (evil-org-mode 1))))
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "<return>") #'xueliang-org-open-at-point
    (kbd "RET")      #'xueliang-org-open-at-point))

(defun xueliang-org-open-at-point ()
  "Open Org links, otherwise keep normal RET behavior." (interactive)
  (if (and (derived-mode-p 'org-mode)
           (eq (org-element-type (org-element-context)) 'link))
      (org-open-at-point)
    (evil-ret)))

(defun xueliang-org-open-at-point-split-advice (orig-fn &optional arg)
  "Open Org links with RET in a split, except web URLs and files."
  (let* ((ret-pressed (member (key-description (this-single-command-keys))
                              '("RET" "<return>")))
         (context (and (derived-mode-p 'org-mode) (org-element-context)))
         (linkp (and context (eq (org-element-type context) 'link)))
         (web-linkp (member (and linkp (org-element-property :type context))
                            '("http" "https" "file"))))
    (when (and (null arg) ret-pressed linkp (not web-linkp))
      (select-window (or (ignore-errors (split-window-right)) (split-window-below))))
    (funcall orig-fn arg)))

(defun xueliang-org-sort-upheading ()
  "Sort org tree entries by todo order." (interactive)
  (when (derived-mode-p 'org-mode)
    (evil-next-line 1) (outline-up-heading 1 t) (org-sort-entries nil ?o)))

;;; Magit
(setq magit-display-buffer-function #'display-buffer
      magit-diff-refine-hunk 'all)
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "h") #'evil-backward-char)
  (define-key magit-mode-map (kbd "j") #'magit-next-line)
  (define-key magit-mode-map (kbd "k") #'magit-previous-line)
  (define-key magit-mode-map (kbd "l") #'evil-forward-char))

;;; My commands
(defun xueliang-cd-current-dir ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" default-directory)))

(defun xueliang-complete-line ()
  "Like vim's Blines completion." (interactive)
  (let ((line (xueliang-read-in-order "Complete Line > "
                                      (split-string (buffer-string) "\n" t)
                                      (string-trim-right (or (thing-at-point 'line) "")))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert line)))

(defun xueliang-dired-sidebar ()
  "Toggle a dired sidebar on the far left." (interactive)
  (let ((sidebar (seq-find (lambda (win)
                             (eq (buffer-local-value 'major-mode (window-buffer win))
                                 'dired-mode))
                           (window-list))))
    (if sidebar
        (delete-window sidebar)
      (xueliang-cd-current-dir)
      (evil-window-vsplit) (evil-window-move-far-left)
      (dired-jump) (dired-hide-details-mode) (hl-line-mode 1)
      (evil-window-decrease-width (floor (* (window-width) 0.382))))))

(defun xueliang-imenu-or-org-today ()
  "In Org jump to today's timestamp, elsewhere imenu." (interactive)
  (if (derived-mode-p 'org-mode)
      (progn (xl-search-line (format-time-string "<%Y-%m-%d")) (evil-ex-nohighlight))
    (xl-imenu)))

(defun xueliang-eshell-popup ()
  "Open eshell below, in the current file's directory, ready for input." (interactive)
  (xueliang-cd-current-dir) (xueliang-split-below) (eshell) (evil-append-line 1))

(defun xueliang-open-knowledge-links ()
  "Pick a line from my links file and open its URL or file link." (interactive)
  (let ((line (xueliang-read-in-order
               "Knowledge Link: "
               (with-temp-buffer (insert-file-contents xl-links-file)
                                 (split-string (buffer-string) "\n" t)))))
    (when (string-match "\\(\\(?:https?://\\|file:\\)[^ \t]+\\)" line)
      (org-link-open-from-string (match-string 1 line)))))

(defun xueliang-open-mindmap ()
  "Pick an SVG from my mindmap folder and open it in the system viewer." (interactive)
  (let ((files (and (file-directory-p xl-mindmap-dir)
                    (directory-files xl-mindmap-dir nil "\\.svg\\'"))))
    (if (null files)
        (message "No SVG files found in %s" xl-mindmap-dir)
      (let ((file (expand-file-name (completing-read "Open Mindmap: " files nil t)
                                    xl-mindmap-dir)))
        (if xl-mac-p (call-process "open" nil 0 nil file) (browse-url-of-file file))))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (apply #'+ (mapcar #'string-to-number
                                        (split-string (buffer-substring start end))))))

(defun xueliang-open-scratch-buffer-window ()
  "Open the scratch buffer in a right split." (interactive)
  (xueliang-split-right) (switch-to-buffer (get-scratch-buffer-create)))

(defun xueliang-magit-status-window ()
  "Open magit status, preferring a right split." (interactive)
  (setq-local split-width-threshold 10)
  (xueliang-cd-current-dir) (magit-status))

(defun xueliang-telescope ()
  "M-x narrowed to the search commands of the ivy stack." (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert "counsel-"))
    (call-interactively #'execute-extended-command)))

(defun xueliang-find-file-in-dotfiles () (interactive)
  (let ((default-directory xl-dotfiles-dir)) (call-interactively #'find-file)))
(defun xueliang-golden-ratio () (interactive)
  (if (fboundp 'golden-ratio) (golden-ratio) (balance-windows)))
(defun Gwrite () (interactive) (evil-ex-execute "!git add %"))
(defun Gcommit () "Like vim-fugitive :Gcommit." (interactive)
  (xueliang-cd-current-dir)
  (evil-ex-execute (format "!git commit -m \"Update %s\"" (file-name-nondirectory buffer-file-name))))
(defun xueliang-replace-tab-trailing-spaces () (interactive)
  (untabify (point-min) (point-max)) (delete-trailing-whitespace))
(defun xueliang-T-open-T-in-browser () (interactive)
  (org-link-open-from-string "https://stockcharts.com/sc3/ui/?s=VWRL.L"))
(defun xueliang-dired-jump () (interactive)
  (dired-jump) (dired-hide-details-mode -1) (hl-line-mode 1))
(defun xueliang-just-make () (interactive) (evil-ex-execute "!just all &"))
(defun xueliang-daily-websites () (interactive) (evil-ex-execute "!just daily-websites &"))
(defalias 'xueliang-what-face  #'describe-face)
(defalias 'xueliang-cap-region #'capitalize-region)
(defalias 'xueliang-org-sort   #'org-sort)

;;; Keys: function keys, then SPC leader (the string is the which-key label)
(dolist (binding '(("<f2>" . xl-imenu)
                   ("<f3>" . xueliang-open-mindmap)
                   ("<f4>" . evil-window-delete)
                   ("<f5>" . xueliang-eshell-popup)
                   ("<f6>" . xl-yank-pop)
                   ("<f7>" . xueliang-just-make)
                   ("<f8>" . xueliang-imenu-or-org-today)
                   ("<f9>" . xueliang-dired-sidebar)
                   ("<f10>" . xueliang-telescope)
                   ("<f11>" . xueliang-org-sort-upheading)
                   ("<f12>" . xueliang-open-knowledge-links)
                   ("C-<f4>" . kill-buffer-and-window)))
  (keymap-global-set (car binding) (cdr binding)))

(defun xl-leader (&rest specs)
  "Bind KEY LABEL COMMAND triples from SPECS under the SPC leader."
  (while specs
    (keymap-set xl-leader-map (pop specs) (cons (pop specs) (pop specs)))))

(xl-leader "SPC" "switch buffer" #'xl-switch-buffer
           "RET" "switch buffer" #'xl-switch-buffer
           "?"   "all keys" #'which-key-show-top-level
           "/"   "search buffer" #'xl-search-line
           "x"   "M-x" #'xl-M-x
           "b s" "scratch window" #'xueliang-open-scratch-buffer-window
           "b w" "read-only" #'read-only-mode
           "c c" "just all" #'xueliang-just-make
           "f f" "find file" #'find-file
           "f r" "recent files" #'xl-recent-file
           "f p" "dotfiles" #'xueliang-find-file-in-dotfiles
           "f T" "imenu" #'xl-imenu
           "g f" "git files" #'project-find-file
           "g g" "magit status" #'xueliang-magit-status-window
           "s i" "imenu" #'xl-imenu
           "w c" "close window" #'delete-window
           "w g" "golden ratio" #'xueliang-golden-ratio
           "w v" "split right" #'xueliang-split-right
           "w s" "split below" #'xueliang-split-below)

;; Starter set of Spacemacs defaults; trim or extend to what the fingers expect.
(xl-leader "TAB" "last buffer" #'mode-line-other-buffer
           "u"   "universal arg" #'universal-argument
           "f s" "save" #'save-buffer
           "b b" "switch buffer" #'xl-switch-buffer
           "b d" "kill buffer" #'kill-current-buffer
           "w h" "window left" #'evil-window-left
           "w j" "window down" #'evil-window-down
           "w k" "window up" #'evil-window-up
           "w l" "window right" #'evil-window-right
           "w d" "close window" #'delete-window
           "w m" "maximize" #'delete-other-windows
           "w /" "split right" #'xueliang-split-right
           "w -" "split below" #'xueliang-split-below
           "p f" "project file" #'project-find-file
           "q q" "quit" #'save-buffers-kill-terminal)

;;; Personal modules outside this repo (binds F3 and SPC y ...)
(when (file-exists-p xl-yi-file)
  (condition-case err (load xl-yi-file nil t)
    (error (message "YI.el failed to load: %s" (error-message-string err)))))
