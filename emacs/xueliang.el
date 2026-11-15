;;; xueliang.el --- My Emacs habits, framework-neutral -*- lexical-binding: t; -*-
;;
;; Loaded last by a framework shim (emacs/crafted/init.el, ...), which must
;; provide `xl-leader-map' and install `xl-packages'.  Spacemacs' own default
;; keys live in emacs/crafted/spacemacs-keys.el; only my overrides are here.
;; Plain Emacs and package APIs only: no spacemacs/, doom, crafted- symbols.

;; The shim reads this first form to know what to install, so keep it first.
(defconst xl-packages
  '(ivy counsel ivy-rich ivy-prescient company
    magit evil-org org-superstar golden-ratio spacemacs-theme doom-themes
    just-mode lua-mode graphviz-dot-mode markdown-mode))

;; ============================================================
;; Platform, Paths and Appearance
;; ============================================================
(defconst xl-mac-p (eq system-type 'darwin))
(defvar xl-dotfiles-dir "~/workspace/dotfiles/")
(defvar xl-links-file "~/workspace/xzhong-links.txt")
(defvar xl-mindmap-dir "~/workspace/mindmap/")
(defvar xl-yi-file "~/workspace/life-notes/YI/YI.el")
;; Machine-specific paths go in a local file that never enters the repo
(let ((local (expand-file-name "~/.emacs-profiles/local.el")))
  (when (file-exists-p local) (load local nil t)))

;; First installed font wins, so the same file works on macOS, Windows, Linux
(when (display-graphic-p)
  (let ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                        '("JetBrains Mono" "Menlo" "Consolas" "DejaVu Sans Mono"))))
    (when font
      (set-face-attribute 'default nil :family font
                          :height (if (eq system-type 'windows-nt) 120 180))))
  (set-frame-parameter nil 'fullscreen 'maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; new frames too
;; Same look as Spacemacs; fall back to tokyo-night if the theme is missing
(or (ignore-errors (load-theme 'spacemacs-dark t))
    (ignore-errors (load-theme 'doom-tokyo-night t)))
;; make sure M-x works on MacOS
(when xl-mac-p (keymap-global-set "s-x" #'counsel-M-x))

;; ============================================================
;; Global Settings - Preserving your existing habits
;; ============================================================
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t ; keep dired etc. fresh too
      initial-major-mode 'text-mode)        ; *scratch* is for notes, not Lisp

;; Recent files: keep a long history and never prune it behind my back
(setq recentf-max-saved-items 1000
      recentf-max-menu-items 50
      recentf-auto-cleanup 'never)

;; Backups: numbered copies in one folder instead of foo~ next to every file
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Performance: read more from subprocesses (git, rg, LSP) per chunk
(setq read-process-output-max (* 1024 1024))

;; Better help and discoverability: which-key pops up fast, keys in order
(setq which-key-idle-delay 0.3
      which-key-sort-order 'which-key-key-order
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1)

;; Performance: Disable bidirectional text for speed
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              scroll-margin 0)

;; Keep indentation and tab behaviour aligned with Neovim
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2)

;; Split side by side only on wide frames; works better for org-timestamp
(setq-default split-width-threshold 160)

;; keep editing state persistent and predictable
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode -1)                ; no current-line highlight

;; avoid long line wrap
(dolist (hook '(dired-mode-hook org-mode-hook prog-mode-hook))
  (add-hook hook (lambda () (setq truncate-lines t))))

;; Saving a file in a new folder creates the folder, like `mkdir -p'
(defun xueliang-create-missing-directories-h ()
  "Create missing parent directories before saving current file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook #'xueliang-create-missing-directories-h)

;; Completion in file order: vertico/prescient would otherwise re-sort lists
;; such as my links file or the lines of the buffer
(defun xueliang-read-in-order (prompt candidates &optional initial)
  "Pick one of CANDIDATES with PROMPT, keeping their order (the UI may re-sort)."
  (completing-read prompt
                   (lambda (str pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity))
                       (complete-with-action action candidates str pred)))
                   nil nil initial))

;; ============================================================
;; Evil Mode Settings - Preserving your vim habits
;; ============================================================
;; Flash yanked text briefly, like Neovim's highlight-on-yank
(defface xueliang-yank-flash '((t (:background "#e0af68" :foreground "#1a1b26")))
  "Face for flashing yanked text." :group 'faces)
(defun xueliang-flash-yanked-text (beg end &rest _)
  (when (called-interactively-p 'any)
    (pulse-momentary-highlight-region beg end 'xueliang-yank-flash)))

;; Split and move into the new window, like vim's :vs / :sp
(defun xueliang-split-right () (interactive) (select-window (split-window-right)))
(defun xueliang-split-below () (interactive) (select-window (split-window-below)))

(with-eval-after-load 'evil
  ;; better evil behaviour: every buffer starts in normal state
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-shift-width 2
        evil-want-fine-undo t           ; undo in small steps, not whole inserts
        evil-flash-delay 0.15)
  (customize-set-variable 'evil-want-Y-yank-to-eol t) ; Y yanks to end of line
  ;; * and # take the whole symbol: foo-bar is one word, not foo
  (setq-default evil-symbol-word-search t)

  ;; modern style 'paste' in evil insert mode
  (define-key evil-insert-state-map (kbd "C-v") #'yank)
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  ;; TAB always inserts two spaces, as in my vimrc
  (define-key evil-insert-state-map (kbd "TAB") (lambda () (interactive) (insert "  ")))

  ;; vim style completions: C-x C-f file names, C-x C-l whole lines
  (define-key evil-insert-state-map (kbd "C-x C-f") #'company-files)
  (define-key evil-insert-state-map (kbd "C-x C-l") #'xueliang-complete-line)
  (define-key evil-normal-state-map (kbd "C-x C-l") #'xueliang-complete-line)

  ;; vim habit: C-u scrolls up half a page (C-d already scrolls down).
  ;; Direct rebind: `evil-want-C-u-scroll' must be set before evil loads,
  ;; which is too late here. Motion state keeps C-u as universal-argument.
  (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") #'evil-scroll-up)

  ;; Make vs and sp behave like vim
  (dolist (cmd '("vs" "vsplit")) (evil-ex-define-cmd cmd #'xueliang-split-right))
  (dolist (cmd '("sp" "split")) (evil-ex-define-cmd cmd #'xueliang-split-below))

  (advice-add 'evil-yank :after #'xueliang-flash-yanked-text)

  ;; IME: back to the English layout when leaving insert state (macOS + macism)
  (when-let* ((macism (or (executable-find "macism")
                          (car (file-expand-wildcards "~/.local/bin/macism")))))
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (call-process macism nil 0 nil "com.apple.keylayout.British")))))

;; ============================================================
;; Ivy/Counsel Settings - Enhanced completion
;; ============================================================
(require 'counsel)
(fido-mode -1) (icomplete-mode -1)      ; plain-Emacs completion UIs, off under ivy
(ivy-mode 1) (counsel-mode 1)
(define-key counsel-mode-map [remap imenu] nil) ; plain imenu, not counsel-imenu
(when (fboundp 'ivy-rich-mode) (ivy-rich-mode 1)) ; extra columns in ivy lists

;; Most-used first, but only where that helps; these lists keep their own
;; order (imenu = position in file, yank-pop = kill ring, recentf = recency)
(when (fboundp 'ivy-prescient-mode)
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer counsel-imenu counsel-yank-pop
               counsel-recentf counsel-grep-or-swiper counsel-rg counsel-esh-history))
  (ivy-prescient-mode 1) (prescient-persist-mode 1))

;; Same ivy behaviour as in Spacemacs
(setq counsel-grep-swiper-limit 30000000 ; swiper stays usable on huge files
      ivy-initial-inputs-alist (remove '(counsel-M-x . "^") ivy-initial-inputs-alist))

;; Performance: Faster ivy settings
(setq ivy-fixed-height-minibuffer t
      ivy-count-format "(%d/%d) "
      ivy-virtual-abbreviate 'abbrev
      ivy-use-selectable-prompt t
      ivy-wrap t)

;; C-k deletes to end of line, C-q turns the candidates into an occur buffer
(define-key ivy-mode-map (kbd "C-k") #'evil-delete-line)
(define-key ivy-mode-map (kbd "C-q") #'ivy-occur)

;; ============================================================
;; Company - Better completion experience
;; ============================================================
;; Pop up at once after one character; words may contain - _ ' /
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

;; Better company keybindings (vim-friendly)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; Enable company-mode for just-mode with files prioritized
(add-hook 'just-mode-hook
          (lambda ()
            (company-mode 1)
            (setq-local company-backends '(company-files company-capf company-dabbrev))))

;; ============================================================
;; Dired Settings
;; ============================================================
(setq dired-use-ls-dired nil)           ; BSD ls on macOS has no --dired
(setq-default dired-listing-switches "-alh") ; List file details in human-readable format
(with-eval-after-load 'dired
  (require 'dired-x)                    ; Enable dired-x for extra features
  ;; h goes up a folder, l opens the file in the other window
  (evil-define-key 'normal dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "TAB") #'dired-display-file) ; preview, stay in dired
  (define-key dired-mode-map (kbd "RET") #'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "<return>") #'dired-find-file-other-window))

;; ============================================================
;; Eshell Settings - Shell keys like in a terminal
;; ============================================================
(add-hook 'eshell-mode-hook
          (lambda ()
            ;; RET in insert state submits the command. Newer evil-collection
            ;; binds it to `newline' (repl-newline theme), which silently
            ;; swallows the command - restore the old behavior.
            (define-key evil-insert-state-local-map (kbd "RET") #'eshell-send-input)
            (define-key evil-insert-state-local-map (kbd "<return>") #'eshell-send-input)
            (define-key evil-insert-state-local-map (kbd "s-<return>") #'eshell-send-input)
            ;; Readline habits: C-a line start, C-k kill, C-r history, C-d quit
            (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)
            (define-key evil-insert-state-local-map (kbd "C-k") #'kill-line)
            (define-key evil-insert-state-local-map (kbd "C-r") #'counsel-esh-history)
            (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)
            (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window)
            ;; F1 acts like ESC, just like tmux
            (define-key evil-insert-state-local-map (kbd "<f1>") #'evil-force-normal-state)
            (evil-insert-state)))           ; ready to type a command

;; Shell words that open files the vim way
(defalias 'eshell/e   #'find-file-other-window)
(defalias 'eshell/vi  #'eshell/e)
(defalias 'eshell/vim #'eshell/e)
(defalias 'eshell/fzf #'counsel-fzf)

;; ============================================================
;; Org Mode Settings - Enhanced appearance
;; ============================================================
;; Set once when org loads (not on every org buffer); only buffer-local
;; work happens in the hook below
(with-eval-after-load 'org
  (setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        ;; changes +/- symbols in item lists
        org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓))
        org-ellipsis " ▶"
        org-log-done nil
        org-hide-emphasis-markers nil   ; Crafted hides *bold* markers; show them
        ;; [[SC:VWRL.L]] and [[piano:Cmaj7]] style links
        org-link-abbrev-alist '(("SC"    . "https://stockcharts.com/h-sc/ui?s=%s")
                                ("piano" . "https://www.scales-chords.com/chord/piano/%s"))
        org-todo-keywords '((sequence "FOCUS(f)" "TODO(t)" "STORY(s)" "PROG(p)" "|" "DONE(d)"))
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-pretty-entities nil
        org-confirm-babel-evaluate nil  ; run shell blocks without asking
        ;; SVG links open in the system viewer, everything else in Emacs
        org-file-apps '(("\\.svg\\'" . default) (auto-mode . emacs)
                        (directory . emacs) (remote . emacs)))
  ;; org-babel improvements
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  ;; better org-mode CMD key behaviour on MacOS
  (define-key org-mode-map (kbd "s-<right>")  #'org-metaright)
  (define-key org-mode-map (kbd "s-<left>")   #'org-metaleft)
  (define-key org-mode-map (kbd "s-<return>") #'org-meta-return)
  ;; Face settings: headings same size and weight as body text
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                  org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight 'regular))
  (advice-add 'org-open-at-point :around #'xueliang-org-open-at-point-split-advice))

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (when (fboundp 'org-superstar-mode) (org-superstar-mode 1))
            (when (fboundp 'evil-org-mode) (evil-org-mode 1))))

;; RET on a link follows it; anywhere else it is a normal RET
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

;; Links to notes open in a split so the current note stays visible
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

;; ============================================================
;; Git settings - Improve magit experience
;; ============================================================
(setq magit-display-buffer-function #'display-buffer
      magit-diff-refine-hunk 'all)      ; word-level highlight in every hunk

;; Vim-style navigation in Magit: h,j,k,l stay for moving around
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "h") #'evil-backward-char)
  (define-key magit-mode-map (kbd "j") #'magit-next-line)
  (define-key magit-mode-map (kbd "k") #'magit-previous-line)
  (define-key magit-mode-map (kbd "l") #'evil-forward-char))

;; ============================================================
;; My own functions
;; ============================================================
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

;; F9: my file tree, a narrow dired window on the left (golden-ratio width)
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
      (progn (swiper (format-time-string "<%Y-%m-%d")) (evil-ex-nohighlight))
    (imenu)))

(defun xueliang-eshell-popup ()
  "Open eshell below, in the current file's directory, ready for input." (interactive)
  (xueliang-cd-current-dir) (xueliang-split-below) (eshell) (evil-append-line 1))

;; F12: fast search in my pre-compiled URL list
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
  (setq-local split-width-threshold 10) ; prefer vsplit
  (xueliang-cd-current-dir) (magit-status))

;; F10: like Telescope's command list in Neovim
(defun xueliang-telescope ()
  "M-x narrowed to the search commands of the ivy stack." (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert "counsel-"))
    (call-interactively #'execute-extended-command)))

;; My one liner commands
(defun xueliang-find-file-in-dotfiles () (interactive)
  (let ((default-directory xl-dotfiles-dir)) (call-interactively #'find-file)))
(defun xueliang-golden-ratio () (interactive)
  (if (fboundp 'golden-ratio) (golden-ratio) (balance-windows)))
(defun Gwrite () (interactive) (evil-ex-execute "!git add %")) ; like vim-fugitive
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

;; Some useful alias
(defalias 'xueliang-what-face  #'describe-face)
(defalias 'xueliang-cap-region #'capitalize-region)
(defalias 'xueliang-org-sort   #'org-sort)

;; ============================================================
;; Function keys - match with vim/nvim
;; ============================================================
(dolist (binding '(("<f2>" . imenu)
                   ("<f3>" . xueliang-open-mindmap) ; my domain learning hot key
                   ("<f4>" . evil-window-delete)
                   ("<f5>" . xueliang-eshell-popup)
                   ("<f6>" . counsel-yank-pop)
                   ("<f7>" . xueliang-just-make)
                   ("<f8>" . xueliang-imenu-or-org-today)
                   ("<f9>" . xueliang-dired-sidebar)
                   ("<f10>" . xueliang-telescope)
                   ("<f11>" . xueliang-org-sort-upheading)
                   ("<f12>" . xueliang-open-knowledge-links)
                   ("C-<f4>" . kill-buffer-and-window)))
  (keymap-global-set (car binding) (cdr binding)))

;; ============================================================
;; Leader keys - my overrides on top of the Spacemacs defaults
;; ============================================================
;; Each entry is KEY, which-key label, COMMAND
(defun xl-leader (&rest specs)
  "Bind KEY LABEL COMMAND triples from SPECS under the SPC leader."
  (while specs
    (keymap-set xl-leader-map (pop specs) (cons (pop specs) (pop specs)))))

(xl-leader "SPC" "switch buffer" #'ivy-switch-buffer ; Spacemacs default is M-x
           "RET" "switch buffer" #'ivy-switch-buffer
           "?"   "all keys" #'which-key-show-top-level
           "/"   "search buffer" #'counsel-grep-or-swiper
           "x"   "M-x" #'counsel-M-x
           "b s" "scratch window" #'xueliang-open-scratch-buffer-window
           "b w" "read-only" #'read-only-mode
           "c c" "just all" #'xueliang-just-make
           "f f" "find file" #'find-file
           "f r" "recent files" #'counsel-recentf
           "f p" "dotfiles" #'xueliang-find-file-in-dotfiles
           "f T" "imenu" #'imenu
           "g f" "git files" #'project-find-file
           "g g" "magit status" #'xueliang-magit-status-window
           "s i" "imenu" #'imenu
           "w c" "close window" #'delete-window
           "w g" "golden ratio" #'xueliang-golden-ratio
           "w v" "split right" #'xueliang-split-right
           "w s" "split below" #'xueliang-split-below)

;; ============================================================
;; Personal modules outside this repo
;; ============================================================
;; YI.el binds F3 and SPC y ...; a broken YI.el must not stop Emacs starting
(when (file-exists-p xl-yi-file)
  (condition-case err (load xl-yi-file nil t)
    (error (message "YI.el failed to load: %s" (error-message-string err)))))
