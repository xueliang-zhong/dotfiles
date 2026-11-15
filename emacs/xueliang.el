;;; xueliang.el --- My Emacs habits, framework-neutral -*- lexical-binding: t; -*-
;;
;; Loaded last by a framework shim (emacs/init.el, ...), which must
;; provide `xl-leader-map' and install `xl-packages'.  Spacemacs' own default
;; keys live in emacs/spacemacs-keys.el; only my overrides are here.
;; Plain Emacs and package APIs only: no spacemacs/, doom, crafted- symbols.

;; The shim reads this first form to know what to install, so keep it first.
(defconst xl-packages
  '(ivy counsel ivy-rich company
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
  (when-let* ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                              '("JetBrains Mono" "Menlo" "Consolas" "DejaVu Sans Mono"))))
    (set-face-attribute 'default nil :family font
                        :height (if (eq system-type 'windows-nt) 120 180)))
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
      initial-major-mode 'text-mode         ; *scratch* is notes
      initial-scratch-message nil)          ; *scratch* starts empty, no boilerplate

;; No beeps: C-g, reaching a buffer edge and similar errors stay silent
(setq ring-bell-function #'ignore
      visible-bell nil)

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
(defun xueliang-truncate-lines () (setq truncate-lines t))
(dolist (hook '(dired-mode-hook org-mode-hook prog-mode-hook))
  (add-hook hook #'xueliang-truncate-lines))

;; Saving a file in a new folder creates the folder, like `mkdir -p'
(defun xueliang-create-missing-directories-h ()
  "Create missing parent directories before saving current file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook #'xueliang-create-missing-directories-h)

;; Completion in file order: vertico or any sorter would otherwise re-sort lists
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

;; TAB always inserts two spaces, as in my vimrc
(defun xueliang-insert-two-spaces () (interactive) (insert "  "))

;; IME: back to the English layout when leaving insert state (macOS + macism)
(defvar xl-macism (or (executable-find "macism")
                      (car (file-expand-wildcards "~/.local/bin/macism"))))
(defun xueliang-ime-english ()
  (call-process xl-macism nil 0 nil "com.apple.keylayout.British"))

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

  (define-keymap :keymap evil-insert-state-map
    "C-v"     #'yank                    ; modern style 'paste' in insert mode
    "C-g"     #'evil-normal-state
    "TAB"     #'xueliang-insert-two-spaces
    "C-x C-f" #'company-files           ; vim style completions: file names,
    "C-x C-l" #'xueliang-complete-line) ; and whole lines
  (define-keymap :keymap evil-normal-state-map
    "C-x C-l" #'xueliang-complete-line
    ;; vim habit: C-u scrolls up half a page (C-d already scrolls down).
    ;; Direct rebind: `evil-want-C-u-scroll' must be set before evil loads,
    ;; which is too late here. Motion state keeps C-u as universal-argument.
    "C-u"     #'evil-scroll-up)
  (keymap-set evil-visual-state-map "C-u" #'evil-scroll-up)

  ;; Make vs and sp behave like vim
  (dolist (cmd '("vs" "vsplit")) (evil-ex-define-cmd cmd #'xueliang-split-right))
  (dolist (cmd '("sp" "split")) (evil-ex-define-cmd cmd #'xueliang-split-below))

  (advice-add 'evil-yank :after #'xueliang-flash-yanked-text)
  (when xl-macism (add-hook 'evil-insert-state-exit-hook #'xueliang-ime-english)))

;; ============================================================
;; Ivy/Counsel Settings - Enhanced completion
;; ============================================================
(require 'counsel)
(fido-mode -1) (icomplete-mode -1)      ; plain-Emacs completion UIs, off under ivy
(ivy-mode 1) (counsel-mode 1)
(ivy-rich-mode 1)                       ; extra columns in ivy lists
(keymap-set counsel-mode-map "<remap> <imenu>" nil) ; plain imenu, not counsel-imenu


;; Same ivy behaviour as in Spacemacs
(setq counsel-grep-swiper-limit 30000000 ; swiper stays usable on huge files
      ivy-initial-inputs-alist (remove '(counsel-M-x . "^") ivy-initial-inputs-alist))

;; Performance: Faster ivy settings
(setq ivy-fixed-height-minibuffer t
      ivy-count-format "(%d/%d) "
      ivy-virtual-abbreviate 'abbrev
      ivy-use-selectable-prompt t
      ivy-wrap t)

;; Golden ratio: ivy lists take 38.2% of the frame height, like the F9 sidebar
(defun xueliang-ivy-golden-height (caller) (round (* 0.382 (frame-height))))
(setq ivy-height-alist '((t . xueliang-ivy-golden-height))
      max-mini-window-height 0.5)       ; minibuffer may grow past the default 25%

(define-keymap :keymap ivy-mode-map
  "C-k" #'evil-delete-line              ; delete to end of line
  "C-q" #'ivy-occur)                    ; turn the candidates into a buffer

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
  (define-keymap :keymap company-active-map
    "C-n" #'company-select-next
    "C-p" #'company-select-previous))

;; Enable company-mode for just-mode with files prioritized
(defun xueliang-just-mode-setup ()
  (company-mode 1)
  (setq-local company-backends '(company-files company-capf company-dabbrev)))
(add-hook 'just-mode-hook #'xueliang-just-mode-setup)

;; ============================================================
;; Dired Settings
;; ============================================================
(setq dired-use-ls-dired nil)           ; BSD ls on macOS has no --dired
(setq-default dired-listing-switches "-alh") ; List file details in human-readable format
;; Layout stays  | sidebar | code window |  : open the file in the window
;; that is already there, and split only when dired is the sole window.
;; (`dired-find-file-other-window' keeps splitting on a wide frame.)
(defun xueliang-dired-open ()
  "Open the file at point in the other window, reusing it if there is one."
  (interactive)
  (let ((display-buffer-overriding-action
         (unless (one-window-p)
           '((display-buffer-reuse-window display-buffer-use-some-window)
             (inhibit-same-window . t)))))
    (dired-find-file-other-window)))

(with-eval-after-load 'dired
  (require 'dired-x)                    ; Enable dired-x for extra features
  ;; h goes up a folder; l and RET open the file in the code window.
  ;; Bound in normal state: evil-collection binds RET there too, and a
  ;; normal-state binding beats one in plain `dired-mode-map'.
  (evil-define-key 'normal dired-mode-map
    "h"              #'dired-up-directory
    "l"              #'xueliang-dired-open
    (kbd "RET")      #'xueliang-dired-open
    (kbd "<return>") #'xueliang-dired-open)
  (keymap-set dired-mode-map "TAB" #'dired-display-file)) ; preview, stay in dired

;; ============================================================
;; Eshell Settings - Shell keys like in a terminal
;; ============================================================
(defun xueliang-eshell-setup ()
  "Insert-state keys for eshell, then start ready to type a command."
  (define-keymap :keymap evil-insert-state-local-map
    ;; RET submits the command. Newer evil-collection binds it to `newline'
    ;; (repl-newline theme), which silently swallows the command.
    "RET"        #'eshell-send-input
    "<return>"   #'eshell-send-input
    "s-<return>" #'eshell-send-input
    ;; Readline habits: C-a line start, C-k kill, C-r history, C-d quit
    "C-a"        #'eshell-bol
    "C-k"        #'kill-line
    "C-r"        #'counsel-esh-history
    "TAB"        #'completion-at-point
    "C-d"        #'kill-buffer-and-window
    "<f1>"       #'evil-force-normal-state) ; F1 acts like ESC, just like tmux
  (evil-insert-state))
(add-hook 'eshell-mode-hook #'xueliang-eshell-setup)

;; Shell words that open files the vim way
(defalias 'eshell/e   #'find-file-other-window)
(defalias 'eshell/vi  #'eshell/e)
(defalias 'eshell/vim #'eshell/e)
(defalias 'eshell/fzf #'counsel-fzf)

;; ============================================================
;; Org Mode Settings - Enhanced appearance
;; ============================================================
;; Set once when org loads (not on every org buffer); only buffer-local
;; work happens in `xueliang-org-setup'
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
  (define-keymap :keymap org-mode-map
    "s-<right>"  #'org-metaright
    "s-<left>"   #'org-metaleft
    "s-<return>" #'org-meta-return)
  ;; RET on a link follows it; anywhere else it is a normal RET
  (evil-define-key 'normal org-mode-map
    (kbd "<return>") #'xueliang-org-open-at-point
    (kbd "RET")      #'xueliang-org-open-at-point)
  ;; Face settings: headings same size and weight as body text
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                  org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight 'regular))
  (advice-add 'org-open-at-point :around #'xueliang-org-open-at-point-split-advice))

(defun xueliang-org-setup ()
  (org-indent-mode 1)
  (org-superstar-mode 1)
  (evil-org-mode 1))
(add-hook 'org-mode-hook #'xueliang-org-setup)

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
  (define-keymap :keymap magit-mode-map
    "h" #'evil-backward-char
    "j" #'magit-next-line
    "k" #'magit-previous-line
    "l" #'evil-forward-char))

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
  (if-let* ((sidebar (seq-find (lambda (win)
                                 (eq (buffer-local-value 'major-mode (window-buffer win))
                                     'dired-mode))
                               (window-list))))
      (delete-window sidebar)
    (xueliang-cd-current-dir)
    (evil-window-vsplit) (evil-window-move-far-left)
    (dired-jump) (dired-hide-details-mode) (hl-line-mode 1)
    (evil-window-decrease-width (floor (* (window-width) 0.382)))))

(defun xueliang-imenu-or-org-today ()
  "In Org jump to today's timestamp, elsewhere imenu." (interactive)
  (if (derived-mode-p 'org-mode)
      (progn (swiper (format-time-string "<%Y-%m-%d")) (evil-ex-nohighlight))
    (counsel-imenu)))

(defun xueliang-eshell-popup ()
  "Open eshell below, in the current file's directory, ready for input." (interactive)
  (xueliang-cd-current-dir) (xueliang-split-below) (eshell) (evil-append-line 1))

;; Same behaviour as `org-link-open-from-string', without needing Org loaded:
;; web links go to the system default browser on any OS, file links open here
(defun xueliang-open-link (link)
  "Open LINK: http(s) in the default browser, file: links inside Emacs.
  A file link may end in ::LINE or ::TEXT to jump there, as in Org."
  (if (string-match-p "\\`https?://" link)
      ;; Not `browse-url': a buffer may set eww as its browser (the Crafted
      ;; home buffer does), and web pages belong in the real browser.
      (browse-url-default-browser link)
    (let* ((parts (split-string (string-remove-prefix "file:" link) "::"))
           ;; file:///Users/me/a%20b.org -> /Users/me/a b.org
           (path (url-unhex-string
                  (replace-regexp-in-string "\\`//+" "/" (car parts))))
           (target (cadr parts)))
      (find-file (expand-file-name path))
      (when target
        (goto-char (point-min))
        (if (string-match-p "\\`[0-9]+\\'" target)
            (forward-line (1- (string-to-number target)))
          (search-forward (string-remove-prefix "*" target) nil t))))))

;; F12: fast search in my pre-compiled URL list
(defun xueliang-open-knowledge-links ()
  "Pick a line from my links file and open its URL or file link." (interactive)
  (let ((line (xueliang-read-in-order
               "Knowledge Link: "
               (with-temp-buffer (insert-file-contents xl-links-file)
                                 (split-string (buffer-string) "\n" t)))))
    (when (string-match "\\(\\(?:https?://\\|file:\\)[^ \t]+\\)" line)
      (xueliang-open-link (match-string 1 line)))))

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
  (minibuffer-with-setup-hook (lambda () (insert "counsel-"))
    (call-interactively #'execute-extended-command)))

;; My one liner commands
(defun xueliang-find-file-in-dotfiles () (interactive)
  (let ((default-directory xl-dotfiles-dir)) (call-interactively #'find-file)))
(defun Gwrite () (interactive) (evil-ex-execute "!git add %")) ; like vim-fugitive
(defun Gcommit () "Like vim-fugitive :Gcommit." (interactive)
  (xueliang-cd-current-dir)
  (evil-ex-execute (format "!git commit -m \"Update %s\"" (file-name-nondirectory buffer-file-name))))
(defun xueliang-replace-tab-trailing-spaces () (interactive)
  (untabify (point-min) (point-max)) (delete-trailing-whitespace))
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
(define-keymap :keymap global-map
  "<f2>"   #'imenu
  "<f3>"   #'xueliang-open-mindmap      ; my domain learning hot key
  "<f4>"   #'evil-window-delete
  "<f5>"   #'xueliang-eshell-popup
  "<f6>"   #'counsel-yank-pop
  "<f7>"   #'xueliang-just-make
  "<f8>"   #'xueliang-imenu-or-org-today
  "<f9>"   #'xueliang-dired-sidebar
  "<f10>"  #'xueliang-telescope
  "<f11>"  #'xueliang-org-sort-upheading
  "<f12>"  #'xueliang-open-knowledge-links
  "C-<f4>" #'kill-buffer-and-window)

;; ============================================================
;; Leader keys - my overrides on top of the Spacemacs defaults
;; ============================================================
;; Only my own commands here; Spacemacs-position keys are in spacemacs-keys.el
(defun xl-leader (&rest specs)
  "Bind KEY LABEL COMMAND triples from SPECS under the SPC leader."
  (while specs
    (keymap-set xl-leader-map (pop specs) (cons (pop specs) (pop specs)))))

(xl-leader "b s" "scratch window" #'xueliang-open-scratch-buffer-window
           "c c" "just all"       #'xueliang-just-make
           "f p" "dotfiles"       #'xueliang-find-file-in-dotfiles
           "g g" "magit status"   #'xueliang-magit-status-window)

;; ============================================================
;; Personal modules outside this repo
;; ============================================================
;; YI.el binds F3 and SPC y ...; a broken YI.el must not stop Emacs starting
(when (file-exists-p xl-yi-file)
  (condition-case err (load xl-yi-file nil t)
    (error (message "YI.el failed to load: %s" (error-message-string err)))))
