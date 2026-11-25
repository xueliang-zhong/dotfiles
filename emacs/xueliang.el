;;; xueliang.el --- My Emacs habits, framework-neutral -*- lexical-binding: t; -*-

;; ============================================================
;; Platform, Paths and Appearance
;; ============================================================
(defconst xl-mac-p (eq system-type 'darwin))
(defconst xl-win-p (eq system-type 'windows-nt))
;; The same font weight looks different per platform: `semi-bold' is right on
;; macOS, while Windows needs `medium' to get the same visual weight.
(defconst xl-font-weight (if xl-win-p 'medium 'semi-bold))
(defvar xl-dotfiles-dir "~/workspace/dotfiles/")
(defvar xl-links-file "~/workspace/xzhong-links.txt")
(defvar xl-mindmap-dir "~/workspace/mindmap/")
(defvar xl-yi-file "~/workspace/life-notes/YI/YI.el")
;; Machine-specific paths go in a local file that never enters the repo
(let ((local (expand-file-name "~/.emacs-profiles/local.el")))
  (when (file-exists-p local) (load local nil t)))
(require 'xl-utils)                 ; the logic behind these settings and bindings

;; First installed font wins, so the same file works on macOS, Windows, Linux.
;; Every text face uses it: `fixed-pitch', `fixed-pitch-serif' and
;; `variable-pitch' carry families of their own ("Monospace", "Sans Serif"),
;; so the home buffer, Info and prose would otherwise use other fonts.
;; Only `default' gets the height; the others inherit it, so text scaling
;; still applies to them all.
(when (display-graphic-p)
  (when-let* ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                              '("JetBrains Mono" "Menlo" "Consolas" "DejaVu Sans Mono"))))
    (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
      (set-face-attribute face nil :family font :weight xl-font-weight))
    (set-face-attribute 'default nil
                        :height (if (eq system-type 'windows-nt) 120 180)))
  (setq-default line-spacing 0.06)
  ;; Direct Chinese and symbol fallback on MacOS: avoids a slow full
  ;; system-font scan when drawing at the first time
  (when xl-mac-p
    (set-fontset-font t 'han (font-spec :family "PingFang SC"))
    (set-fontset-font t 'cjk-misc (font-spec :family "PingFang SC"))
    (set-fontset-font t '(#x4dc0 . #x4dff) (font-spec :family "Apple Symbols")))
  (set-frame-parameter nil 'fullscreen 'maximized))

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; new frames too

;; Same look as Spacemacs; fall back to tokyo-night if the theme is missing
;; `spacemacs-theme` passes :background nil unless this is non-nil, which
;; Emacs 31 warns about.  Load with highlights on, then clear them below.
(setq spacemacs-theme-org-highlight t)
(or (ignore-errors (load-theme 'spacemacs-dark t))
    (ignore-errors (load-theme 'doom-tokyo-night t)))
;; Remove the muddy background strip behind comments in `spacemacs-dark'
(set-face-attribute 'font-lock-comment-face nil :background 'unspecified)
;; make sure M-x works on MacOS
(when xl-mac-p (keymap-global-set "s-x" #'execute-extended-command))

(when xl-mac-p
  (with-eval-after-load 'evil
    (dolist (map (list evil-insert-state-map
                       evil-replace-state-map
                       evil-emacs-state-map
                       ;; Ex and evil-search command lines do not inherit
                       ;; from `minibuffer-local-map'.
                       evil-command-line-map
                       evil-eval-map))
      (keymap-set map "M-3" #'xueliang-insert-hash)))
  (keymap-set minibuffer-local-map "M-3" #'xueliang-insert-hash)
  (keymap-set isearch-mode-map "M-3" #'xueliang-isearch-insert-hash))

;; Priority Suggestions: 1 = high, 2 = medium, 3 = low
(xl-idle-require 'org        0.5)
(xl-idle-require 'org-indent 0.5)
(xl-idle-require 'magit      1.5)
(xl-idle-require 'eshell     2.0)

;; Other useful things to do when idle
(run-with-idle-timer 300 t 'garbage-collect)

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

;; Performance: while keys are waiting, skip font-lock so typing stays
;; responsive in big or complex buffers (Prelude does the same)
(setq redisplay-skip-fontification-on-input t)

;; ffap must not ping a host name under point to check it exists: that
;; causes network lookups and freezes.  `reject' keeps only known files/URLs.
(setq ffap-machine-p-known 'reject)

;; Better help and discoverability: which-key pops up fast, keys in order
(setq which-key-idle-delay 0.3
      which-key-sort-order 'which-key-key-order
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1)

;; Small defaults from Crafted's newcomers settings, kept here so they are
;; visible and easy to revert.  Column number in the mode line; the compact
;; mode line hides the less useful parts; imenu rescans on every call;
;; compile output scrolls to the first error.
(column-number-mode 1)
(setq mode-line-compact 'long
      imenu-auto-rescan t
      compilation-scroll-output 'first-error)

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

;; Kill ring survives restarts, like savehist keeps search history.  Text
;; properties are stripped before saving: propertized strings bloat the
;; history file and can fail to load.  Duplicate kills are not stored twice.
(require 'savehist)
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (seq-filter #'stringp kill-ring)))))
(setq kill-do-not-save-duplicates t)
(savehist-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode -1)                ; no current-line highlight

;; Long lines stay on one line, like `set nowrap' in my vimrc: no wrapping
;; by default in any buffer, not only dired, org and prog modes.
(setq-default truncate-lines t)

(add-hook 'before-save-hook #'xueliang-create-missing-directories-h)

;; ============================================================
;; Evil Mode Settings - Preserving your vim habits
;; ============================================================
;; Flash yanked text briefly, like Neovim's highlight-on-yank
(defface xueliang-yank-flash '((t (:background "#e0af68" :foreground "#1a1b26")))
  "Face for flashing yanked text." :group 'faces)
;; IME: back to the English layout when leaving insert state (macOS + macism)
(defvar xl-macism (or (executable-find "macism")
                      (car (file-expand-wildcards "~/.local/bin/macism"))))

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
    "TAB"     #'xueliang-tab-or-corfu-next
    "C-x C-f" #'cape-file               ; vim style completions: file names,
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
;; Vertico Stack - modern minibuffer completion
;; ============================================================
;; Crafted's completion module turns on vertico (list UI), marginalia
;; (annotations), orderless (matching; file names use partial-completion)
;; and consult; only my additions to it are here.
(require 'vertico-repeat)                ; `SPC r l': repeat the last list
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(add-hook 'minibuffer-setup-hook #'xueliang-vertico-golden-count)

;; minibuffer may grow past the default 25%
(setq max-mini-window-height 0.5)

;; Same minibuffer habits as the old ivy-mode-map: C-k deletes to end of
;; line, C-q turns the candidates into a buffer (Embark's exporter, the
;; modern `ivy-occur'), M-w does the same as C-q so the result is visible
;; instead of only landing in the kill ring, C-. opens Embark's actions.
;; RET enters a directory and DEL goes up one level, like counsel-find-file.
;; C-. is only bound here: in normal state Evil keeps it for `evil-repeat-pop'.
(define-keymap :keymap vertico-map
  "RET"   #'vertico-directory-enter
  "DEL"   #'vertico-directory-delete-char
  "M-DEL" #'vertico-directory-delete-word
  "C-k"   #'evil-delete-line
  "C-q"   #'embark-export
  "M-w"   #'embark-export
  "C-."   #'embark-act)

;; ============================================================
;; Corfu + Cape - in-buffer completion popup
;; ============================================================
;; Crafted's completion module turns on corfu (auto popup, cycling, docs
;; beside it).  Mine: pop up after two characters, which also keeps
;; org-tempo's `<s' + TAB working.  On Windows querying the backends is
;; slower, so wait for a short pause too; fast typing then never runs
;; completion at all.
(setq corfu-auto-delay (if xl-win-p 0.2 0)
      corfu-auto-prefix 2 ;; 2 works better for org-tempo '<s'
      ;; Corfu stays in buffers; the minibuffer ':' uses vertico
      global-corfu-minibuffer nil
      ;; no auto popup in shells and REPLs (eshell: TAB shows it on demand)
      global-corfu-modes '((not eshell-mode comint-mode inferior-emacs-lisp-mode) t))

;; Text modes (org, markdown, text) put `ispell-completion-at-point' in
;; `completion-at-point-functions'.  Each query starts a spell checker
;; (about 230 ms here), and corfu runs every function on each committed
;; character, so typing Chinese in notes stalled.  Take it out of the automatic
;; completion; `ispell-complete-word' still works on demand.
(add-hook 'text-mode-hook
          (lambda ()
            (remove-hook 'completion-at-point-functions
                         #'ispell-completion-at-point t)))

;; `cape-dabbrev' scans every buffer with the same major mode for candidates
;; by default, on every keystroke.  Cape's own docstring recommends against
;; that for auto completion; it is a major source of input lag, especially
;; on Windows.  Complete from the current buffer only.
(setq cape-dabbrev-buffer-function #'current-buffer)

;; Crafted adds cape-dabbrev and cape-file with `add-to-list', which changes
;; the buffer current at load time: *scratch* (lisp-interaction-mode has its
;; own local value), not the global default.  Add them globally here.
(add-hook 'completion-at-point-functions #'cape-dabbrev t)
(add-hook 'completion-at-point-functions #'cape-file t)

;; TAB moves down the popup via `xueliang-tab-or-corfu-next' (bound in
;; evil insert state above).  Without this entry corfu would insert the
;; selected candidate and close the popup right after the command, so
;; TAB would only work once.
(with-eval-after-load 'corfu
  (add-to-list 'corfu-continue-commands #'xueliang-tab-or-corfu-next))

(add-hook 'enable-theme-functions #'xueliang-corfu-theme-faces)
(with-eval-after-load 'corfu (xueliang-corfu-theme-faces))

(add-hook 'emacs-lisp-mode-hook #'xueliang-elisp-completion-setup)

;; ============================================================
;; Snippets - SPC i s, like the Spacemacs auto-completion layer
;; ============================================================
;; yasnippet loads on first use, so it costs nothing at startup.  Turning
;; on `yas-minor-mode' loads the snippets for the current mode; the
;; completing prompt displays them in Vertico.
(setq yas-prompt-functions '(yas-completing-prompt))

;; ============================================================
;; Dired Settings
;; ============================================================
(setq dired-use-ls-dired nil)           ; BSD ls on macOS has no --dired
(setq-default dired-listing-switches "-alh") ; List file details in human-readable format
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

(add-hook 'eshell-mode-hook #'xueliang-eshell-setup)

;; Shell words that open files the vim way
(defalias 'eshell/e   #'find-file-other-window)
(defalias 'eshell/vi  #'eshell/e)
(defalias 'eshell/vim #'eshell/e)
(defalias 'eshell/fzf (if (executable-find "fd") #'consult-fd #'consult-find))

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
  ;; keep Org's load light, but bring back its structure templates:
  ;; `org-tempo' makes `<s' + TAB expand to a src block, as before.
  (setq org-modules nil)
  (require 'org-tempo)

  ;; org-babel improvements
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  ;; shell blocks run in the background unless a block says `:async no'
  (setq org-babel-default-header-args:bash '((:async . "yes"))
        org-babel-default-header-args:sh   '((:async . "yes")))
  ;; better org-mode CMD key behaviour on MacOS
  (define-keymap :keymap org-mode-map
    "s-<right>"  #'org-metaright
    "s-<left>"   #'org-metaleft
    "s-<return>" #'org-meta-return)
  ;; RET on a link follows it; anywhere else it is a normal RET
  (evil-define-key 'normal org-mode-map
    (kbd "<return>") #'org-open-at-point
    (kbd "RET")      #'org-open-at-point)
  ;; Face settings: headings same size and weight as body text
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                  org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight xl-font-weight))
  ;; `spacemacs-theme-org-highlight' is enabled only to avoid nil face specs;
  ;; keep headings without the theme's background highlight.
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4))
    (set-face-attribute face nil :background 'unspecified))
  (advice-add 'org-open-at-point :around #'xueliang-org-open-at-point-split-advice))

(defun xueliang-org-setup ()
  (org-indent-mode 1)
  (org-superstar-mode 1)
  (evil-org-mode 1))
(add-hook 'org-mode-hook #'xueliang-org-setup)

;; ============================================================
;; Git settings - Improve magit experience
;; ============================================================
(setq magit-display-buffer-function #'display-buffer
      magit-diff-refine-hunk t)         ; refine only the current hunk

;; Vim-style navigation in Magit: h,j,k,l stay for moving around
(with-eval-after-load 'magit
  (define-keymap :keymap magit-mode-map
    "h" #'evil-backward-char
    "j" #'magit-next-line
    "k" #'magit-previous-line
    "l" #'evil-forward-char))

;; ============================================================
;; Git gutter - changed lines in the margin (diff-hl, as in Prelude)
;; ============================================================
;; Added, changed and deleted lines show in the left margin, updated on save
;; and after magit operations.  Loaded when idle, so it never slows startup;
;; the margin appears a moment after the first file opens.
(xl-idle-require 'diff-hl 1.0)
(with-eval-after-load 'diff-hl
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)                 ; changed files
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)) ; refresh after magit
;; SPC g n / p jump between hunks, SPC g h shows the hunk at point
(keymap-set xl-leader-map "g n" '("next hunk" . diff-hl-next-hunk))
(keymap-set xl-leader-map "g p" '("previous hunk" . diff-hl-previous-hunk))
(keymap-set xl-leader-map "g h" '("show hunk" . diff-hl-show-hunk))

(defun xueliang-eshell-popup ()
  "Open eshell below, in the current file's directory, ready for input." (interactive)
  (xueliang-cd-current-dir) (xueliang-split-below) (eshell) (evil-append-line 1))

;; ============================================================
;; Persistent scratch - like Doom's scratch notes
;; ============================================================
;; `persistent-scratch' restores *scratch* at startup and saves it when
;; Emacs is idle and on exit, so notes survive restarts.  C-x C-s in
;; scratch saves it immediately instead of asking for a file name.
(require 'persistent-scratch)
(setq persistent-scratch-save-file
      (expand-file-name "persistent-scratch.eld" user-emacs-directory)
      persistent-scratch-autosave-interval '(idle . 30))
;; Only interactive sessions read/write the save file: `just emacs-test'
;; and other batch runs must not overwrite the real scratch notes.
(unless noninteractive
  (persistent-scratch-setup-default)
  (with-current-buffer (get-scratch-buffer-create)
    (persistent-scratch-mode 1)))

;; My one liner commands
(defun xueliang-find-file-in-dotfiles () (interactive)
  (let ((default-directory xl-dotfiles-dir)) (call-interactively #'find-file)))
(defun xueliang-replace-tab-trailing-spaces () (interactive)
  (untabify (point-min) (point-max)) (delete-trailing-whitespace))
(defun xueliang-dired-jump () (interactive)
  (dired-jump) (dired-hide-details-mode -1) (hl-line-mode 1))
(defun xueliang-just-make ()
  "Run `just all' in the dotfiles, asynchronously."
  (interactive)
  (xueliang-just-run "all"))

(defun xueliang-daily-websites ()
  "Run the `daily-websites' recipe, asynchronously."
  (interactive)
  (xueliang-just-run "daily-websites"))

;; Some useful alias
(defalias 'xueliang-what-face  #'describe-face)
(defalias 'xueliang-cap-region #'capitalize-region)
(defalias 'xueliang-org-sort   #'org-sort)

;; ============================================================
;; Help keys - symbol lookup with live completion
;; ============================================================
;; The stock C-h a (`apropos-command') reads a pattern first, then builds the
;; buffer.  `describe-symbol' offers functions, variables and faces as Vertico
;; candidates, so Orderless filters while typing; C-q exports the matches to
;; an *Apropos* buffer.  This is the modern replacement for `consult-apropos'.
(keymap-set help-map "a" #'describe-symbol)

;; ============================================================
;; Function keys - match with vim/nvim
;; ============================================================
(define-keymap :keymap global-map
  "<f2>"   #'consult-imenu
  "<f3>"   #'consult-imenu
  "<f4>"   #'evil-window-delete
  "<f5>"   #'xueliang-eshell-popup
  "<f6>"   #'consult-yank-pop
  "<f7>"   #'xueliang-just-make
  "<f8>"   #'xueliang-imenu-or-org-today
  "<f9>"   #'xueliang-dired-sidebar
  "<f10>"  #'xl-sk-home-buffer
  "<f11>"  #'xueliang-org-sort-upheading
  "<f12>"  #'xueliang-open-knowledge-links
  "C-<f4>" #'kill-buffer-and-window)

;; ============================================================
;; Leader keys - my overrides on top of the Spacemacs defaults
;; ============================================================
;; Only my own commands here; Spacemacs-position keys are in xl-key-bindings.el
(defun xl-leader (&rest specs)
  "Bind KEY LABEL COMMAND triples from SPECS under the SPC leader."
  (while specs
    (keymap-set xl-leader-map (pop specs) (cons (pop specs) (pop specs)))))

(xl-leader "b s" "scratch window" #'xueliang-open-scratch-buffer-window
           "c c" "just all"       #'xueliang-just-make
           "f p" "dotfiles"       #'xueliang-find-file-in-dotfiles
           "g b" "blame"          #'xl-gblame
           "g g" "git status"     #'xl-git-status-window)

;; ============================================================
;; Personal modules outside this repo
;; ============================================================
(when (file-exists-p xl-yi-file)
  (load xl-yi-file nil t))
