;;; init.el --- Standalone test: my habits without Crafted  -*- lexical-binding: t; -*-
;;
;; A single-file Emacs configuration that tries to reproduce the habits in
;; emacs/xueliang.el and emacs/spacemacs-keys.el without Crafted Emacs.
;;
;; Run it in its own profile directory, never in ~/.emacs.d:
;;
;;   mkdir -p ~/.emacs-profiles/vanilla
;;   ln -sfn ~/workspace/dotfiles/emacs/vanilla/init.el \
;;           ~/.emacs-profiles/vanilla/init.el
;;   emacs --init-directory ~/.emacs-profiles/vanilla
;;
;; Everything Emacs writes (elpa/, custom.el, history, recentf, backups,
;; auto-save-list, eln-cache) stays under that profile directory, because
;; --init-directory changes `user-emacs-directory' before this file loads.
;;
;; This file deliberately avoids Crafted.  Third-party packages are listed
;; once in `xl-packages' and installed on the first start.

;;; Code:

;; ============================================================
;; Early startup: before the first frame is drawn
;; ============================================================
;; Startup speed: no GC during init, back to a moderate threshold afterwards.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 128 1024 1024))))

;; Window chrome off from the beginning.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native compilation: no background compilation of packages on first use.
;; It slows the whole machine down and is not needed for testing a config.
(setq native-comp-jit-compilation nil
      inhibit-automatic-native-compilation t
      package-native-compile nil
      native-comp-async-report-warnings-errors 'silent)

;; Emacs 31 warns about every third-party file without a lexical-binding
;; cookie (spacemacs-theme is one); nothing I can fix from here.
(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

;; The tagged evil release on NonGNU ELPA (1.15.0) breaks on Emacs 30+
;; (`void-variable evil-mode-buffers'); take the evil family from MELPA.
(setq package-pinned-packages '((evil . "melpa")
                                (evil-collection . "melpa")
                                (evil-org . "melpa")))

;; ============================================================
;; Packages: built-in package.el, no use-package needed
;; ============================================================
(require 'package)
(require 'seq)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Keep this profile's package directory inside the profile.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      ;; Emacs' own startup should not read archives; we control activation.
      package-enable-at-startup nil
      ;; For a disposable test profile, do not block on GPG key import.
      ;; Remove this line for a configuration you intend to keep.
      package-check-signature nil)

(defconst xl-packages
  '(evil evil-collection evil-escape evil-surround evil-org
    evil-nerd-commenter winum avy yasnippet yasnippet-snippets
    magit org-superstar golden-ratio spacemacs-theme doom-themes
    just-mode lua-mode graphviz-dot-mode markdown-mode diff-hl
    vertico consult marginalia orderless embark embark-consult
    corfu cape)
  "Third-party packages this configuration needs.")

(dolist (pkg xl-packages)
  (add-to-list 'package-selected-packages pkg))

;; Activate what is already installed, without reading archive caches.
(package-activate-all)

;; Read archives only when something is missing (first start of this profile).
(unless (seq-every-p #'package-installed-p xl-packages)
  (package-refresh-contents)
  (dolist (pkg xl-packages)
    (unless (package-installed-p pkg)
      (package-install pkg)))
  (package-activate-all))

;; Customize writes into its own file, never into this one.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

;; ============================================================
;; Platform, paths and local overrides
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

;; Machine-specific paths go in a local file that never enters the repo.
(let ((local (expand-file-name "~/.emacs-profiles/local.el")))
  (when (file-exists-p local)
    (load local nil t)))

;; ============================================================
;; Appearance
;; ============================================================
;; Every text face uses the same family; `fixed-pitch', `fixed-pitch-serif'
;; and `variable-pitch' otherwise bring their own fonts, so the home buffer,
;; Info and prose would look different.  Only `default' gets the height; the
;; others inherit it, so text scaling still applies to them all.
(when (display-graphic-p)
  (when-let* ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                              '("JetBrains Mono" "Menlo" "Consolas"
                                "DejaVu Sans Mono"))))
    (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
      (set-face-attribute face nil :family font :weight xl-font-weight))
    (set-face-attribute 'default nil
                        :height (if xl-win-p 120 180)))
  (setq-default line-spacing 0.06)
  ;; Direct Chinese and symbol fallback on macOS: avoids a slow full
  ;; system-font scan when drawing for the first time.
  (when xl-mac-p
    (set-fontset-font t 'han (font-spec :family "PingFang SC"))
    (set-fontset-font t 'cjk-misc (font-spec :family "PingFang SC"))
    (set-fontset-font t '(#x4dc0 . #x4dff) (font-spec :family "Apple Symbols")))
  (set-frame-parameter nil 'fullscreen 'maximized))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Same look as Spacemacs; fall back to tokyo-night if the theme is missing.
(setq spacemacs-theme-org-highlight t)
(or (ignore-errors (load-theme 'spacemacs-dark t))
    (ignore-errors (load-theme 'doom-tokyo-night t)))
;; Remove the muddy background strip behind comments in `spacemacs-dark'.
(set-face-attribute 'font-lock-comment-face nil :background 'unspecified)

;; make sure M-x works on macOS
(when xl-mac-p
  (keymap-global-set "s-x" #'execute-extended-command))

;; ============================================================
;; Global defaults
;; ============================================================
(setq inhibit-startup-screen t
      initial-major-mode 'text-mode        ; *scratch* is notes
      initial-scratch-message nil          ; *scratch* starts empty
      ring-bell-function #'ignore
      visible-bell nil
      create-lockfiles nil
      require-final-newline t
      uniquify-buffer-name-style 'forward)

(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2
              fill-column 80)

(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)                    ; on by default since 28, kept explicit
(electric-pair-mode 1)
(delete-selection-mode 1)
(save-place-mode 1)
(winner-mode 1)
(global-hl-line-mode -1)               ; no current-line highlight

;; Recent files: keep a long history and never prune it behind my back.
(setq recentf-max-saved-items 1000
      recentf-max-menu-items 50
      recentf-auto-cleanup 'never)
(recentf-mode 1)

;; Backups: numbered copies in one folder instead of foo~ next to every file.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Killing state survives restarts, like savehist keeps search history.
;; Text properties are stripped before saving: propertized strings bloat
;; the history file and can fail to load.
(require 'savehist)
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (seq-filter #'stringp kill-ring)))))
(setq kill-do-not-save-duplicates t)
(savehist-mode 1)

;; Keep dired and other non-file buffers fresh too.
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t
      dired-dwim-target t                   ; guess target dir in dired
      dired-auto-revert-buffer t)           ; refresh dired on revisit
(global-auto-revert-mode 1)

;; Keep the eshell prompt at the bottom, but only in the active window.
(setq eshell-scroll-to-bottom-on-input 'this)

;; Dedicated buffers and manual switches follow display actions.
(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

;; Prefer the full ibuffer to the plain buffer list.
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(setq ibuffer-movement-cycle nil
      ibuffer-old-time 24)

;; Performance: read more from subprocesses (git, rg, LSP) per chunk.
(setq read-process-output-max (* 1024 1024))

;; Long-line handling and smoother scrolling.
(setq auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-conservatively 101
      scroll-preserve-screen-position t)
(global-so-long-mode 1)

;; Load the newest source or compiled file; useful while editing configs.
(setq load-prefer-newer t
      bookmark-save-flag 1)

;; Make shebang scripts executable when saved.
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Repeat useful keys with `repeat-mode', like Crafted's defaults.
(repeat-mode 1)

;; While keys are waiting, skip font-lock so typing stays responsive in big
;; or complex buffers (Prelude does the same).
(setq redisplay-skip-fontification-on-input t)

;; ffap must not ping a host name under point to check it exists.
(setq ffap-machine-p-known 'reject)

;; Better help and discoverability: which-key pops up fast, keys in order.
(setq which-key-idle-delay 0.3
      which-key-sort-order 'which-key-key-order
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1)
(which-key-mode 1)

;; Small defaults kept visible: compact mode line, imenu rescans on every
;; call, compile output scrolls to the first error.
(setq mode-line-compact 'long
      imenu-auto-rescan t
      compilation-scroll-output 'first-error)

;; Performance: disable bidirectional text for speed.
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              scroll-margin 0)

;; Completion in the minibuffer and at point.
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      xref-show-definitions-function #'xref-show-definitions-completing-read)
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode -1)) ; Corfu owns the in-buffer popup

;; Split side by side only on wide frames; works better for org-timestamp.
(setq-default split-width-threshold 160)

;; Long lines stay on one line, like `set nowrap' in my vimrc.
(setq-default truncate-lines t)

;; Saving a file in a new folder creates the folder, like `mkdir -p'.
(defun xueliang-create-missing-directories-h ()
  "Create missing parent directories before saving current file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook #'xueliang-create-missing-directories-h)

;; Completion in file order: vertico or any sorter would otherwise re-sort
;; lists such as my links file or the lines of the buffer.
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
;; macOS UK keyboard: Option-3 types #
;; ============================================================
;; On a UK Mac keyboard # is Option-3.  Emacs treats Option as Meta, so the
;; key arrives as M-3 (`digit-argument') and # cannot be typed.  Insert it
;; wherever text is entered; normal state keeps M-3 as a numeric prefix.
(defun xueliang-insert-hash ()
  "Insert `#', the Option-3 character on a UK Mac keyboard."
  (interactive)
  (let ((last-command-event ?#))
    (self-insert-command 1)))

(defun xueliang-isearch-insert-hash ()
  "Add `#' to the current isearch string."
  (interactive)
  (isearch-printing-char ?#))

;; ============================================================
;; First-use latency: pay one-time costs while idle
;; ============================================================
;; Emacs cannot load packages asynchronously in-process.  Idle timers are
;; the safe compromise: they run only when I am not typing.
(defun xueliang-require-silently (feature)
  "Require FEATURE without echo-area or *Messages* output."
  (let ((inhibit-message t) (message-log-max nil))
    (ignore-errors (require feature))))

(defun xl-idle-require (package priority)
  "Load PACKAGE once Emacs has been idle for PRIORITY half-seconds.
PRIORITY 0 or below disables the load: no timer is registered, so a
line can be switched off without deleting it."
  ;; Deliberately a plain `when': Emacs treats a zero or negative idle
  ;; delay as \"ripe now\", the opposite of switching the load off.
  (when (> priority 0)
    (run-with-idle-timer
     (* 0.5 priority) nil
     (lambda ()
       ;; Outside the silent `let' in the helper, so a failure still shows.
       (unless (xueliang-require-silently package)
         (message "xl-idle-require: could not load %s" package))))))

;; Priority suggestions: 1 = high, 2 = medium, 3 = low.
(xl-idle-require 'org        0.5)
(xl-idle-require 'org-indent 0.5)
(xl-idle-require 'magit      1.5)
(xl-idle-require 'eshell     2.0)
(xl-idle-require 'diff-hl    1.0)

;; Other useful things to do when idle.
(run-with-idle-timer 300 t 'garbage-collect)

;; Org should stay light, but `org-tempo' brings back `<s' + TAB.
(setq org-modules nil)

;; ============================================================
;; Evil: Vim keys, third-party
;; ============================================================
;; Must be set before Evil loads; evil-collection requires it.
(setq evil-want-keybinding nil
      evil-want-C-u-scroll t
      evil-want-C-h-delete nil          ; C-h stays the help prefix
      evil-undo-system 'undo-redo)

;; evil-collection binds keys in many modes; keep SPC and `,' out of them
;; so both leaders work everywhere.  Must be set before evil-collection-init.
(setq evil-collection-key-blacklist '("SPC" ","))

(require 'evil)
(require 'evil-collection)
(require 'evil-escape)
(require 'evil-surround)
(require 'winum)

;; Every buffer starts in normal state; no mode-specific emacs state.
(setq evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil
      evil-shift-width 2
      evil-want-fine-undo t           ; undo in small steps, not whole inserts
      evil-flash-delay 0.15)
(customize-set-variable 'evil-want-Y-yank-to-eol t) ; Y yanks to end of line
;; * and # take the whole symbol: foo-bar is one word, not foo.
(setq-default evil-symbol-word-search t)

(evil-mode 1)
(evil-collection-init)
(evil-escape-mode 1)
(global-evil-surround-mode 1)
(setq evil-escape-key-sequence "fd"
      evil-escape-delay 0.1)
(setq winum-auto-setup-mode-line t)
(winum-mode 1)

;; Flash yanked text briefly, like Neovim's highlight-on-yank.
(defface xueliang-yank-flash '((t (:background "#e0af68" :foreground "#1a1b26")))
  "Face for flashing yanked text."
  :group 'faces)
(defun xueliang-flash-yanked-text (beg end &rest _)
  (when (called-interactively-p 'any)
    (pulse-momentary-highlight-region beg end 'xueliang-yank-flash)))
(advice-add 'evil-yank :after #'xueliang-flash-yanked-text)

;; Split and move into the new window, like vim's :vs / :sp.
(defun xueliang-split-right () (interactive) (select-window (split-window-right)))
(defun xueliang-split-below () (interactive) (select-window (split-window-below)))

;; TAB inserts two spaces, as in my vimrc; but in an open Corfu popup it
;; moves down like C-n.  Needed because evil's insert-state map (an
;; emulation keymap) shadows `corfu-map', so TAB cannot be bound there.
;; The corfu section below adds this command to `corfu-continue-commands',
;; so the popup stays open instead of accepting the candidate after TAB.
;; In Org it also expands structure templates: `<s' + TAB is a src block.
(defun xueliang-tab-or-corfu-next ()
  (interactive)
  (cond
   ((and (bound-and-true-p corfu-mode) (bound-and-true-p corfu--candidates))
    (corfu-next))
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-tempo-complete-tag)
         (org-tempo-complete-tag))
    nil)
   (t (insert "  "))))

;; IME: back to the English layout when leaving insert state (macOS + macism).
(defvar xl-macism (or (executable-find "macism")
                      (car (file-expand-wildcards "~/.local/bin/macism"))))

(defun xueliang-ime-english ()
  (when xl-macism
    (call-process xl-macism nil 0 nil "com.apple.keylayout.British")))

;; Insert-state keys: modern paste, ESC-like C-g, TAB popup navigation,
;; and vim-style file/line completion.
(define-keymap :keymap evil-insert-state-map
  "C-v"     #'yank
  "C-g"     #'evil-normal-state
  "TAB"     #'xueliang-tab-or-corfu-next
  "C-x C-f" #'cape-file
  "C-x C-l" #'xueliang-complete-line)

(define-keymap :keymap evil-normal-state-map
  "C-x C-l" #'xueliang-complete-line
  ;; vim habit: C-u scrolls up half a page (C-d already scrolls down).
  "C-u"     #'evil-scroll-up)
(keymap-set evil-visual-state-map "C-u" #'evil-scroll-up)

;; Make vs and sp behave like vim.
(dolist (cmd '("vs" "vsplit"))
  (evil-ex-define-cmd cmd #'xueliang-split-right))
(dolist (cmd '("sp" "split"))
  (evil-ex-define-cmd cmd #'xueliang-split-below))

(when xl-macism
  (add-hook 'evil-insert-state-exit-hook #'xueliang-ime-english))

;; UK keyboard: insert # in every evil state that accepts text, plus the
;; minibuffer and isearch.  Normal state keeps M-3 as a numeric prefix.
(when xl-mac-p
  (dolist (map (list evil-insert-state-map
                     evil-replace-state-map
                     evil-emacs-state-map
                     ;; Ex and evil-search command lines do not inherit
                     ;; from `minibuffer-local-map'.
                     evil-command-line-map
                     evil-eval-map))
    (keymap-set map "M-3" #'xueliang-insert-hash))
  (keymap-set minibuffer-local-map "M-3" #'xueliang-insert-hash)
  (keymap-set isearch-mode-map "M-3" #'xueliang-isearch-insert-hash))

;; ============================================================
;; Vertico stack: vertico, marginalia, orderless, consult, embark
;; ============================================================
(require 'vertico)
(require 'vertico-directory)
(require 'vertico-repeat)
(require 'marginalia)
(require 'orderless)
(require 'consult)
(require 'embark)
(require 'embark-consult)

(vertico-mode 1)
(marginalia-mode 1)

;; Crafted starts Vertico with wrapping at the top/bottom.
(setq vertico-cycle t)

;; Orderless for everything except file names: there partial-completion
;; gives the familiar path completion.
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion))))
      completions-detailed t)

;; Consult habits from Crafted: C-s searches the buffer, C-r in the
;; minibuffer shows command history, and region completion uses Consult.
(keymap-global-set "C-s" #'consult-line)
(keymap-set minibuffer-local-map "C-r" #'consult-history)
(setq completion-in-region-function #'consult-completion-in-region)

;; Embark: C-. opens actions; C-h on a prefix lists that prefix's keys.
(keymap-global-set "C-." #'embark-act)
(keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
(setq prefix-help-command #'embark-prefix-help-command)

;; `SPC r l': repeat the last list.
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;; Golden ratio: the candidate list takes 38.2% of the frame height, like
;; the F9 sidebar.  Computed for every minibuffer, so a resized frame gets
;; a resized list.
(defun xueliang-vertico-golden-count ()
  "Size the Vertico list to the golden ratio of the frame height."
  (setq-local vertico-count (max 5 (round (* 0.382 (frame-height))))))
(add-hook 'minibuffer-setup-hook #'xueliang-vertico-golden-count)

;; minibuffer may grow past the default 25%.
(setq max-mini-window-height 0.5)

;; Same minibuffer habits as the old ivy-mode-map: C-k deletes to end of
;; line, C-q turns the candidates into a buffer (Embark's exporter, the
;; modern `ivy-occur'), M-w does the same as C-q so the result is visible
;; instead of only landing in the kill ring, C-. opens Embark's actions.
;; RET enters a directory and DEL goes up one level, like counsel-find-file.
;; C-. is also bound globally for Embark; in normal state Evil keeps it for
;; `evil-repeat-pop'.
(define-keymap :keymap vertico-map
  "RET"   #'vertico-directory-enter
  "DEL"   #'vertico-directory-delete-char
  "M-DEL" #'vertico-directory-delete-word
  "C-k"   #'evil-delete-line
  "C-q"   #'embark-export
  "M-w"   #'embark-export
  "C-."   #'embark-act)

;; `SPC *': like vim/nvim's <leader>*, search the project for the symbol
;; under the cursor.  No symbol under point just opens an empty prompt.
(defun xueliang-search-project-at-point ()
  "Search the project for the symbol at point."
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol t)))

;; ============================================================
;; Corfu + Cape: in-buffer completion popup
;; ============================================================
(setq corfu-auto t
      corfu-cycle t
      corfu-auto-delay (if xl-win-p 0.2 0)
      corfu-auto-prefix 2 ;; 2 works better for org-tempo '<s'
      ;; Corfu stays in buffers; the minibuffer ':' uses vertico.
      global-corfu-minibuffer t
      ;; No auto popup in shells and REPLs (eshell: TAB shows it on demand).
      global-corfu-modes '((not eshell-mode comint-mode
                                 inferior-emacs-lisp-mode)
                           t))

(require 'corfu)
(require 'corfu-popupinfo)
(require 'cape)

(global-corfu-mode 1)
(corfu-popupinfo-mode 1)
(eldoc-add-command #'corfu-insert)
(define-keymap :keymap corfu-map
  "M-p" #'corfu-popupinfo-scroll-down
  "M-n" #'corfu-popupinfo-scroll-up
  "M-d" #'corfu-popupinfo-toggle)

;; Emacs 31 draws Corfu popups in terminals itself; corfu-terminal is not
;; needed and warns if loaded.

;; `cape-dabbrev' scans every buffer with the same major mode for candidates
;; by default, on every keystroke.  Cape's own docstring recommends against
;; that for auto completion; complete from the current buffer only.
(setq cape-dabbrev-buffer-function #'current-buffer)

;; Add the global completion sources.  `emacs-lisp-mode' replaces its own
;; buffer-local CAPF below; every other mode sees these.
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)

;; Silence pcomplete's errors/messages (important for Corfu in Eshell).
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; TAB moves down the popup via `xueliang-tab-or-corfu-next'.  Without this
;; entry corfu would insert the selected candidate and close the popup right
;; after the command, so TAB would only work once.
(add-to-list 'corfu-continue-commands #'xueliang-tab-or-corfu-next)

;; Corfu ships theme-neutral colors: a near-black popup with a teal
;; selection.  Paint it with the active theme instead, so the popup looks
;; like a small floating window.  Recompute after a theme switch.
(defun xueliang-corfu-theme-faces (&rest _)
  "Match the Corfu popup to the colors of the active theme."
  (when (facep 'corfu-default)          ; a theme may load before Corfu
    (set-face-attribute 'corfu-default nil
                        :background (face-attribute 'default :background nil 'default)
                        :foreground (face-attribute 'default :foreground nil 'default))
    (set-face-attribute 'corfu-current nil
                        :background (face-attribute 'highlight :background nil 'default)
                        :foreground (face-attribute 'default :foreground nil 'default))
    (set-face-attribute 'corfu-border nil
                        :background (face-attribute 'vertical-border :foreground nil 'default))
    (set-face-attribute 'corfu-bar nil
                        :background (face-attribute 'shadow :foreground nil 'default))))
(add-hook 'enable-theme-functions #'xueliang-corfu-theme-faces)
(xueliang-corfu-theme-faces)

;; `emacs-lisp-mode' installs its own buffer-local CAPF, so the global
;; `cape-dabbrev' above never runs.  Merge it in: symbols typed in this
;; file (even before they are evaluated) now complete too.
(defun xueliang-elisp-completion-setup ()
  "Merge buffer words into Emacs Lisp completion."
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'elisp-completion-at-point
                                     #'cape-dabbrev
                                     #'cape-file)
                    t)))
(add-hook 'emacs-lisp-mode-hook #'xueliang-elisp-completion-setup)

;; ============================================================
;; Snippets: SPC i s, like the Spacemacs auto-completion layer
;; ============================================================
;; yasnippet loads on first use, so it costs nothing at startup.
(setq yas-prompt-functions '(yas-completing-prompt))

(defun xueliang-insert-snippet ()
  "Insert a yasnippet for the current mode (Spacemacs SPC i s)."
  (interactive)
  (require 'yasnippet)
  (yas-minor-mode 1)
  (yas-insert-snippet))

(defun xueliang-new-snippet ()
  "Create a new yasnippet for the current mode."
  (interactive)
  (require 'yasnippet)
  (yas-new-snippet))

;; ============================================================
;; Dired
;; ============================================================
(setq dired-use-ls-dired nil)                  ; BSD ls on macOS has no --dired
(setq-default dired-listing-switches "-alh")

;; Layout stays  | sidebar | code window |  : open the file in the window
;; that is already there, and split only when dired is the sole window.
(defun xueliang-dired-open ()
  "Open the file at point in the other window, reusing it if there is one."
  (interactive)
  (let ((display-buffer-overriding-action
         (unless (one-window-p)
           '((display-buffer-reuse-window display-buffer-use-some-window)
             (inhibit-same-window . t)))))
    (dired-find-file-other-window)))

(with-eval-after-load 'dired
  (require 'dired-x)
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
;; Eshell
;; ============================================================
(defun xueliang-eshell-setup ()
  "Insert-state keys for eshell, then start ready to type a command."
  (define-keymap :keymap evil-insert-state-local-map
    ;; RET submits the command.  Newer evil-collection binds it to `newline'
    ;; (repl-newline theme), which silently swallows the command.
    "RET"        #'eshell-send-input
    "<return>"   #'eshell-send-input
    "s-<return>" #'eshell-send-input
    ;; Readline habits: C-a line start, C-k kill, C-r history, C-d quit.
    "C-a"        #'eshell-bol
    "C-k"        #'kill-line
    "C-r"        #'consult-history
    "TAB"        #'completion-at-point
    "C-d"        #'kill-buffer-and-window
    "<f1>"       #'evil-force-normal-state) ; F1 acts like ESC, just like tmux
  (evil-insert-state))
(add-hook 'eshell-mode-hook #'xueliang-eshell-setup)

;; Shell words that open files the vim way.
(defalias 'eshell/e   #'find-file-other-window)
(defalias 'eshell/vi  #'eshell/e)
(defalias 'eshell/vim #'eshell/e)
(defalias 'eshell/fzf (if (executable-find "fd") #'consult-fd #'consult-find))

;; ============================================================
;; Org: built in, just settings and the standard global keys
;; ============================================================
(setq org-directory "~/org"
      org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-startup-indented t
      org-log-done nil)

(defun xueliang-org-open-at-point-split-advice (orig-fn &optional arg)
  "Open Org links with RET in a split, except web URLs and files."
  (let* ((ret-pressed (member (key-description (this-single-command-keys))
                              '("RET" "<return>")))
         (context (and (derived-mode-p 'org-mode) (org-element-context)))
         (linkp (and context (eq (org-element-type context) 'link)))
         (web-linkp (member (and linkp (org-element-property :type context))
                            '("http" "https" "file"))))
    (when (and (null arg) ret-pressed linkp (not web-linkp))
      (select-window (or (ignore-errors (split-window-right))
                         (split-window-below))))
    (funcall orig-fn arg)))

;; Set once when org loads (not on every org buffer); only buffer-local work
;; happens in `xueliang-org-setup'.
(with-eval-after-load 'org
  (setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        ;; changes +/- symbols in item lists
        org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓))
        org-ellipsis " ▶"
        org-hide-emphasis-markers nil   ; show *bold* markers
        ;; [[SC:VWRL.L]] and [[piano:Cmaj7]] style links
        org-link-abbrev-alist '(("SC"    . "https://stockcharts.com/h-sc/ui?s=%s")
                                ("piano" . "https://www.scales-chords.com/chord/piano/%s"))
        org-todo-keywords '((sequence "FOCUS(f)" "TODO(t)" "STORY(s)"
                                      "PROG(p)" "|" "DONE(d)"))
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-pretty-entities nil
        org-confirm-babel-evaluate nil  ; run shell blocks without asking
        ;; SVG links open in the system viewer, everything else in Emacs.
        org-file-apps '(("\\.svg\\'" . default) (auto-mode . emacs)
                        (directory . emacs) (remote . emacs)))
  ;; keep Org's load light, but bring back its structure templates:
  ;; `org-tempo' makes `<s' + TAB expand to a src block, as before.
  (setq org-modules nil)
  (require 'org-tempo)

  ;; org-babel improvements
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

  ;; better org-mode CMD key behaviour on macOS
  (define-keymap :keymap org-mode-map
    "s-<right>"  #'org-metaright
    "s-<left>"   #'org-metaleft
    "s-<return>" #'org-meta-return)

  ;; RET on a link follows it; anywhere else it is a normal RET.
  (evil-define-key 'normal org-mode-map
    (kbd "<return>") #'org-open-at-point
    (kbd "RET")      #'org-open-at-point)

  ;; Face settings: headings same size and weight as body text.
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                  org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight xl-font-weight))
  ;; `spacemacs-theme-org-highlight' is enabled only to avoid nil face specs;
  ;; keep headings without the theme's background highlight.
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4))
    (set-face-attribute face nil :background 'unspecified))
  (advice-add 'org-open-at-point :around
              #'xueliang-org-open-at-point-split-advice))

(defun xueliang-org-setup ()
  (require 'org-superstar)
  (require 'evil-org)
  (org-indent-mode 1)
  (org-superstar-mode 1)
  (evil-org-mode 1))
(add-hook 'org-mode-hook #'xueliang-org-setup)

(defun xueliang-org-sort-upheading ()
  "Sort org tree entries by todo order."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (evil-next-line 1)
    (outline-up-heading 1 t)
    (org-sort-entries nil ?o)))

;; Org's standard global keys, in addition to the SPC tree.
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;; ============================================================
;; Git: magit
;; ============================================================
(setq magit-display-buffer-function #'display-buffer
      magit-diff-refine-hunk t)         ; refine only the current hunk

;; Vim-style navigation in Magit: h,j,k,l stay for moving around.
(with-eval-after-load 'magit
  (define-keymap :keymap magit-mode-map
    "h" #'evil-backward-char
    "j" #'magit-next-line
    "k" #'magit-previous-line
    "l" #'evil-forward-char))

;; Fast status: every status section and header starts extra git processes.
;; Drop the expensive ones; logs are still one key away (`l'), stashes
;; another (`z').  Kept: upstream state, the head branch and the working tree.
(with-eval-after-load 'magit-status
  (dolist (fn '(magit-insert-tags-header
                magit-insert-diff-filter-header
                magit-insert-upstream-branch-header
                magit-insert-push-branch-header))
    (remove-hook 'magit-status-headers-hook fn))
  (setq magit-status-sections-hook
        (seq-remove (lambda (fn)
                      (memq fn '(
                                 ;; magit-insert-unpushed-to-upstream-or-recent
                                 ;; magit-insert-unpushed-to-upstream
                                 magit-insert-unpushed-to-pushremote
                                 magit-insert-unpulled-from-upstream
                                 magit-insert-unpulled-from-pushremote
                                 magit-insert-merge-log
                                 magit-insert-stashes
                                 magit-insert-rebase-sequence
                                 magit-insert-am-sequence
                                 magit-insert-sequencer-sequence
                                 magit-insert-bisect-output
                                 magit-insert-bisect-rest
                                 magit-insert-bisect-log)))
                    magit-status-sections-hook)))

;; ============================================================
;; Git gutter: changed lines in the margin (diff-hl)
;; ============================================================
(with-eval-after-load 'diff-hl
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; ============================================================
;; Spacemacs keys: the SPC tree, M-m, `,' major-mode leader
;; ============================================================
(require 'seq)
(autoload 'flymake-show-buffer-diagnostics "flymake" nil t) ; SPC e l

(defun xl-sk-bind (map &rest specs)
  "In MAP bind KEY LABEL DEF triples; a nil DEF only names a prefix."
  (while specs
    (let ((key (pop specs)) (label (pop specs)) (def (pop specs)))
      (if def
          (keymap-set map key (cons label def))
        (with-eval-after-load 'which-key
          (which-key-add-keymap-based-replacements map key label))))))

;;; Small commands Spacemacs provides and Emacs lacks
(defun xl-sk-copy-file-path ()
  "Copy this buffer's file path."
  (interactive)
  (let ((path (or buffer-file-name default-directory)))
    (kill-new path)
    (message "Copied %s" path)))

(defun xl-sk-delete-this-file ()
  "Delete this buffer's file and kill the buffer."
  (interactive)
  (when (and buffer-file-name
             (yes-or-no-p (format "Delete %s? " buffer-file-name)))
    (delete-file buffer-file-name t)
    (kill-buffer)))

(defun xl-sk-copy-whole-buffer ()
  "Copy the whole buffer."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer copied"))

(defun xl-sk-open-dotfile ()
  "Open this standalone init.el."
  (interactive)
  (find-file (or (and user-init-file (file-truename user-init-file))
                 (expand-file-name "emacs/vanilla/init.el" xl-dotfiles-dir))))

(defun xl-sk-relative-line-numbers ()
  "Toggle relative line numbers."
  (interactive)
  (setq-local display-line-numbers
              (if (eq display-line-numbers 'relative) nil 'relative)))

(defvar xl-sk-themes '(spacemacs-dark doom-tokyo-night doom-spacegrey)
  "Themes cycled by SPC T n, like `dotspacemacs-themes'.")

(defun xl-sk-cycle-theme ()
  "Switch to the next theme in `xl-sk-themes'."
  (interactive)
  (let* ((now (car custom-enabled-themes))
         (next (or (cadr (memq now xl-sk-themes)) (car xl-sk-themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next t)
    (message "Theme: %s" next)))

;;; SPC tree
(defvar-keymap xl-leader-map
  :doc "Keys under SPC (and M-m outside normal state).")
(evil-define-key '(normal visual motion) 'global (kbd "SPC") xl-leader-map)
(evil-define-key '(insert emacs) 'global (kbd "M-m") xl-leader-map)

(xl-sk-bind xl-leader-map
  "SPC" "switch buffer"  #'consult-buffer ; Spacemacs default is M-x
  "TAB" "last buffer"    #'mode-line-other-buffer
  "!"   "shell command"  #'shell-command
  "'"   "eshell"         #'eshell
  ";"   "comment"        #'evilnc-comment-operator
  "u"   "universal arg"  #'universal-argument
  "1" "window 1" #'winum-select-window-1   "2" "window 2" #'winum-select-window-2
  "3" "window 3" #'winum-select-window-3   "4" "window 4" #'winum-select-window-4
  "5" "window 5" #'winum-select-window-5   "6" "window 6" #'winum-select-window-6
  "7" "window 7" #'winum-select-window-7   "8" "window 8" #'winum-select-window-8
  "9" "window 9" #'winum-select-window-9

  "a" "apps" nil
  "a c" "calc" #'calc  "a d" "dired" #'dired  "a s e" "eshell" #'eshell
  "a o a" "org agenda" #'org-agenda  "a o c" "org capture" #'org-capture

  "b" "buffers" nil
  "b b" "switch buffer" #'consult-buffer
  "b h" "home"          #'xl-sk-home-buffer
  "b d" "kill buffer"   #'kill-current-buffer
  "b x" "kill buffer & window" #'kill-buffer-and-window
  "b n" "next buffer"   #'next-buffer
  "b p" "previous buffer" #'previous-buffer
  "b R" "revert"        #'revert-buffer-quick
  "b m" "messages"      #'view-echo-area-messages
  "b I" "ibuffer"       #'ibuffer
  "b Y" "copy buffer"   #'xl-sk-copy-whole-buffer

  "c" "comments/compile" nil
  "c l" "comment lines"     #'evilnc-comment-or-uncomment-lines
  "c p" "comment paragraph" #'evilnc-comment-or-uncomment-paragraphs
  "c y" "copy & comment"    #'evilnc-copy-and-comment-lines
  "c C" "compile"           #'compile

  "e" "errors" nil
  "e n" "next error" #'next-error  "e p" "previous error" #'previous-error
  "e l" "list errors" #'flymake-show-buffer-diagnostics

  "f" "files" nil
  "f f" "find file"     #'find-file
  "f r" "recent files"  #'consult-recent-file
  "f s" "save"          #'save-buffer
  "f S" "save all"      #'save-some-buffers
  "f R" "rename file"   #'rename-visited-file
  "f D" "delete file"   #'xl-sk-delete-this-file
  "f c" "copy file"     #'copy-file
  "f j" "dired jump"    #'dired-jump
  "f y" "yank" nil
  "f y y" "copy path"   #'xl-sk-copy-file-path
  "f e" "emacs config" nil
  "f e d" "open config" #'xl-sk-open-dotfile

  "g" "git" nil
  "g s" "status" #'magit-status  "g b" "blame" #'magit-blame
  "g l" "file log" #'magit-log-buffer-file  "g d" "file diff" #'magit-diff-buffer-file
  "g m" "dispatch" #'magit-dispatch

  "h" "help" nil
  "h i" "info" #'info  "h m" "man" #'man
  "h d" "describe" nil
  "h d f" "function" #'describe-function  "h d v" "variable" #'describe-variable
  "h d k" "key" #'describe-key  "h d m" "mode" #'describe-mode
  "h d b" "bindings" #'describe-bindings  "h d p" "package" #'describe-package
  "h d a" "apropos" #'apropos-command

  "i" "insert" nil
  "i s" "snippet" #'xueliang-insert-snippet
  "i S" "new snippet" #'xueliang-new-snippet

  "j" "jump" nil
  "j j" "to chars" #'avy-goto-char-timer  "j l" "to line" #'avy-goto-line
  "j w" "to word" #'avy-goto-word-1  "j i" "imenu" #'imenu
  "j d" "dired jump" #'dired-jump  "j D" "dired jump other" #'dired-jump-other-window

  "n" "narrow" nil
  "n r" "region" #'narrow-to-region  "n f" "defun" #'narrow-to-defun
  "n s" "org subtree" #'org-narrow-to-subtree  "n w" "widen" #'widen

  "p" "project" nil
  "p f" "find file" #'project-find-file  "p p" "switch project" #'project-switch-project
  "p b" "project buffer" #'project-switch-to-buffer  "p d" "project dired" #'project-dired
  "p k" "kill buffers" #'project-kill-buffers  "p c" "compile" #'project-compile
  "p '" "eshell" #'project-eshell
  "p s" "search" #'consult-ripgrep

  "q" "quit" nil
  "q q" "quit" #'save-buffers-kill-terminal  "q Q" "kill emacs" #'kill-emacs
  "q r" "restart" #'restart-emacs  "q z" "close frame" #'delete-frame

  "r" "resume/registers" nil
  "r l" "resume list" #'vertico-repeat
  "r y" "kill ring" #'consult-yank-pop

  "s" "search" nil
  "s s" "this buffer" #'consult-line
  "s p" "project" #'consult-ripgrep
  "s j" "imenu" #'imenu
  "s c" "clear highlight" #'evil-ex-nohighlight

  "t" "toggles" nil
  "t n" "line numbers" #'display-line-numbers-mode
  "t r" "relative numbers" #'xl-sk-relative-line-numbers
  "t l" "truncate lines" #'toggle-truncate-lines
  "t w" "whitespace" #'whitespace-mode
  "t f" "fill column" #'display-fill-column-indicator-mode
  "t s" "syntax check" #'flymake-mode
  "t S" "spelling" #'flyspell-mode
  "t h" "highlight" nil
  "t h h" "current line" #'hl-line-mode

  "T" "UI" nil
  "T n" "next theme" #'xl-sk-cycle-theme  "T s" "choose theme" #'load-theme
  "T F" "fullscreen" #'toggle-frame-fullscreen  "T M" "maximize" #'toggle-frame-maximized

  "w" "windows" nil
  "w h" "left" #'evil-window-left  "w j" "down" #'evil-window-down
  "w k" "up" #'evil-window-up  "w l" "right" #'evil-window-right
  "w H" "move far left" #'evil-window-move-far-left  "w J" "move to bottom" #'evil-window-move-very-bottom
  "w K" "move to top" #'evil-window-move-very-top  "w L" "move far right" #'evil-window-move-far-right
  "w /" "split right" #'xueliang-split-right  "w -" "split below" #'xueliang-split-below
  "w v" "split right" #'xueliang-split-right  "w s" "split below" #'xueliang-split-below
  "w d" "close" #'delete-window  "w m" "maximize" #'delete-other-windows
  "w =" "balance" #'balance-windows  "w w" "other window" #'other-window
  "w u" "undo layout" #'winner-undo  "w U" "redo layout" #'winner-redo)

;;; Major-mode leader: `,' in normal state and `SPC m' resolve per mode.
;;  A mode without a map keeps evil's `,' (repeat f/t backwards).
(defvar xl-major-leader-maps nil "Alist of (MODE . KEYMAP) for `,' and SPC m.")

(defun xl-sk--major-map (&rest _)
  (cdr (seq-find (lambda (entry) (derived-mode-p (car entry)))
                 xl-major-leader-maps)))

(defun xl-sk--comma (&rest _)
  (or (xl-sk--major-map) #'evil-repeat-find-char-reverse))

(evil-define-key '(normal visual motion) 'global (kbd ",")
  '(menu-item "major mode" nil :filter xl-sk--comma))
(keymap-set xl-leader-map "m" '(menu-item "major mode" nil :filter xl-sk--major-map))

(defvar-keymap xl-org-leader-map :doc "Spacemacs org layer keys.")
(xl-sk-bind xl-org-leader-map
  "," "C-c C-c" #'org-ctrl-c-ctrl-c  "'" "edit block" #'org-edit-special
  "*" "toggle heading" #'org-ctrl-c-star  "-" "toggle item" #'org-ctrl-c-minus
  "RET" "C-c RET" #'org-ctrl-c-ret  "a" "agenda" #'org-agenda  "c" "capture" #'org-capture
  "l" "open link" #'org-open-at-point  "p" "priority" #'org-priority
  "A" "archive subtree" #'org-archive-subtree
  "b" "babel" nil
  "b b" "execute buffer" #'org-babel-execute-buffer  "b e" "execute block" #'org-babel-execute-src-block
  "C" "clock" nil
  "C i" "clock in" #'org-clock-in  "C o" "clock out" #'org-clock-out
  "C g" "goto clock" #'org-clock-goto
  "d" "dates" nil
  "d d" "deadline" #'org-deadline  "d s" "schedule" #'org-schedule
  "d t" "timestamp" #'org-time-stamp  "d T" "inactive timestamp" #'org-time-stamp-inactive
  "e" "export" nil
  "e e" "export" #'org-export-dispatch
  "i" "insert" nil
  "i l" "link" #'org-insert-link  "i h" "heading" #'org-insert-heading
  "i s" "subheading" #'org-insert-subheading  "i t" "tags" #'org-set-tags-command
  "i p" "property" #'org-set-property
  "s" "subtrees" nil
  "s r" "refile" #'org-refile  "s n" "narrow" #'org-narrow-to-subtree  "s N" "widen" #'widen
  "s s" "sparse tree" #'org-sparse-tree  "s h" "promote" #'org-promote-subtree
  "s l" "demote" #'org-demote-subtree  "s j" "move down" #'org-move-subtree-down
  "s k" "move up" #'org-move-subtree-up  "s S" "sort" #'org-sort
  "t" "tables" nil
  "t a" "align" #'org-table-align  "t c" "convert region" #'org-table-convert-region
  "t r" "recalculate" #'org-table-recalculate  "t s" "sort" #'org-table-sort-lines
  "T" "toggles" nil
  "T c" "checkbox" #'org-toggle-checkbox  "T i" "inline images" #'org-toggle-inline-images
  "T l" "link display" #'org-toggle-link-display  "T t" "todo" #'org-todo)
(push (cons 'org-mode xl-org-leader-map) xl-major-leader-maps)

(defvar-keymap xl-elisp-leader-map :doc "Spacemacs emacs-lisp layer keys.")
(xl-sk-bind xl-elisp-leader-map
  "e" "eval" nil
  "e e" "last sexp" #'eval-last-sexp  "e f" "defun" #'eval-defun
  "e b" "buffer" #'eval-buffer  "e r" "region" #'eval-region
  "g" "goto" nil
  "g g" "definition" #'xref-find-definitions  "g G" "references" #'xref-find-references
  "h" "help" nil
  "h h" "describe symbol" #'describe-symbol)
(push (cons 'emacs-lisp-mode xl-elisp-leader-map) xl-major-leader-maps)

;;; Home buffer: SPC b h / F10, recent files 1-9, startup time
(defvar-local xl-sk--home-files nil "Recent files listed on the home buffer.")

(defun xl-sk--home-button (label action)
  (insert-button label 'action (lambda (_) (funcall action)) 'follow-link t))

(defun xl-sk--home-build ()
  "Build and return the *Home* buffer."
  (let ((buf (get-buffer-create "*Home*"))
        ;; One spelling per file: /Users/me/x and ~/x are the same entry.
        (files (seq-take (seq-uniq (mapcar #'abbreviate-file-name recentf-list)) 9)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Emacs %s — standalone init.el test\n\n" emacs-version))
        (if files
            (progn
              (insert "Recent files:\n")
              (seq-do-indexed
               (lambda (file i)
                 (insert (format "[%d]  " (1+ i)))
                 (insert-button (abbreviate-file-name file)
                                'action (lambda (_) (find-file file))
                                'follow-link t)
                 (insert "\n"))
               files))
          (insert "No recent files yet.\n"))
        (insert "\nTo start:     ")
        (xl-sk--home-button "Open a File"
                            (lambda () (call-interactively #'find-file)))
        (insert "     ")
        (xl-sk--home-button "Open Home Directory" (lambda () (dired "~")))
        (insert "     ")
        (xl-sk--home-button "Open Dotfiles"
                            (lambda () (dired xl-dotfiles-dir)))
        (insert "\n")
        (let ((seconds (if (and before-init-time after-init-time)
                           (float-time (time-subtract after-init-time
                                                      before-init-time))
                         0)))
          (insert (format "\nEmacs loaded in %.2f seconds\n" seconds))))
      ;; `special-mode' kills buffer-local variables, so store the list
      ;; and install the digit keys afterwards.
      (special-mode)
      (setq xl-sk--home-files files)
      ;; Defined below; at runtime it is already available.
      (when (fboundp 'xl-sk--home-keys)
        (xl-sk--home-keys)))
    buf))

(defun xl-sk-home-buffer ()
  "Show the home buffer; also the `initial-buffer-choice'."
  (interactive)
  (let ((buf (xl-sk--home-build)))
    (when (called-interactively-p 'any)
      (switch-to-buffer buf))
    buf))

;; Digits 1-9 open the listed recent files, in normal and motion state.
(defun xl-sk--home-keys ()
  "In the home buffer, digits 1-9 open the listed recent files."
  (when (eq major-mode 'special-mode)
    (dotimes (i 9)
      (let ((n i))
        (dolist (state '(normal motion))
          (evil-local-set-key
           state (kbd (number-to-string (1+ n)))
           (lambda () (interactive)
             (when-let* ((file (nth n xl-sk--home-files)))
               (find-file file)))))))))

;;; Leader keys that sit on Spacemacs' own positions, with the commands I use
(xl-sk-bind xl-leader-map
  "RET" "switch buffer"  #'consult-buffer
  "?"   "all keys"       #'which-key-show-top-level
  "*"   "project symbol" #'xueliang-search-project-at-point
  "/"   "search buffer"  #'consult-line
  "x"   "M-x"            #'execute-extended-command
  "b w" "read-only"      #'read-only-mode
  "f T" "imenu"          #'imenu
  "g f" "git files"      #'project-find-file
  "s i" "imenu"          #'imenu
  "w c" "close window"   #'delete-window
  "w g" "golden ratio"   #'golden-ratio)

;; ============================================================
;; My own functions
;; ============================================================
(defun xueliang-cd-current-dir ()
  "cd to directory of current buffer/file."
  (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" default-directory)))

(defun xueliang-complete-line ()
  "Like vim's Blines completion."
  (interactive)
  (let ((line (xueliang-read-in-order
               "Complete Line > "
               (split-string (buffer-string) "\n" t)
               (string-trim-right (or (thing-at-point 'line) "")))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert line)))

;; F9: my file tree, a narrow dired window on the left (golden-ratio width).
(defun xueliang-dired-sidebar ()
  "Toggle a dired sidebar on the far left."
  (interactive)
  (if-let* ((sidebar (seq-find
                      (lambda (win)
                        (eq (buffer-local-value 'major-mode (window-buffer win))
                            'dired-mode))
                      (window-list))))
      (delete-window sidebar)
    (xueliang-cd-current-dir)
    (evil-window-vsplit)
    (evil-window-move-far-left)
    (dired-jump)
    (dired-hide-details-mode)
    (hl-line-mode 1)
    (evil-window-decrease-width (floor (* (window-width) 0.382)))))

(defun xueliang-imenu-or-org-today ()
  "In Org jump to today's timestamp, elsewhere imenu."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (consult-line (format-time-string "<%Y-%m-%d"))
    (consult-imenu)))

(defun xueliang-eshell-popup ()
  "Open eshell below, in the current file's directory, ready for input."
  (interactive)
  (xueliang-cd-current-dir)
  (xueliang-split-below)
  (eshell)
  (evil-append-line 1))

;; F12: fast search in my pre-compiled URL list.
(defun xueliang-open-knowledge-links ()
  "Pick a line from my links file and open its URL or file link."
  (interactive)
  (unless (file-readable-p xl-links-file)
    (user-error "No links file: %s" xl-links-file))
  (let ((line (xueliang-read-in-order
               "Knowledge Link: "
               (with-temp-buffer
                 (insert-file-contents xl-links-file)
                 (split-string (buffer-string) "\n" t)))))
    (when (string-match "\\(\\(?:https?://\\|file:\\)[^ \t]+\\)" line)
      (require 'org)
      (org-link-open-from-string (match-string 1 line)))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s"
           (apply #'+ (mapcar #'string-to-number
                              (split-string (buffer-substring start end))))))

(defun xueliang-open-scratch-buffer-window ()
  "Open the scratch buffer in a right split."
  (interactive)
  (xueliang-split-right)
  (switch-to-buffer (get-scratch-buffer-create)))

(defun xueliang-magit-status-window ()
  "Open magit status, preferring a right split."
  (interactive)
  (setq-local split-width-threshold 10) ; prefer vsplit
  (xueliang-cd-current-dir)
  (magit-status))

;; My one-liner commands.
(defun xueliang-find-file-in-dotfiles ()
  (interactive)
  (let ((default-directory xl-dotfiles-dir))
    (call-interactively #'find-file)))

(defun Gwrite ()
  "Like vim-fugitive :Gwrite."
  (interactive)
  (evil-ex-execute "!git add %"))

(defun Gcommit ()
  "Like vim-fugitive :Gcommit."
  (interactive)
  (xueliang-cd-current-dir)
  (evil-ex-execute
   (format "!git commit -m \"Update %s\""
           (file-name-nondirectory buffer-file-name))))

(defun xueliang-replace-tab-trailing-spaces ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun xueliang-dired-jump ()
  (interactive)
  (dired-jump)
  (dired-hide-details-mode -1)
  (hl-line-mode 1))

(defun xueliang-just-make ()
  (interactive)
  (evil-ex-execute "!just all &"))

(defun xueliang-daily-websites ()
  (interactive)
  (evil-ex-execute "!just daily-websites &"))

;; Some useful aliases.
(defalias 'xueliang-what-face #'describe-face)
(defalias 'xueliang-cap-region #'capitalize-region)
(defalias 'xueliang-org-sort #'org-sort)

;; ============================================================
;; Function keys: match my vim/nvim setup
;; ============================================================
(define-keymap :keymap global-map
  "<f2>"   #'imenu
  "<f3>"   #'imenu
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
;; Leader keys: my overrides on top of the Spacemacs defaults
;; ============================================================
;; Only my own commands here; Spacemacs-position keys are above.
(defun xl-leader (&rest specs)
  "Bind KEY LABEL COMMAND triples from SPECS under the SPC leader."
  (while specs
    (keymap-set xl-leader-map (pop specs) (cons (pop specs) (pop specs)))))

(xl-leader "b s" "scratch window" #'xueliang-open-scratch-buffer-window
           "c c" "just all"       #'xueliang-just-make
           "f p" "dotfiles"       #'xueliang-find-file-in-dotfiles
           "g g" "magit status"   #'xueliang-magit-status-window
           "g n" "next hunk"      #'diff-hl-next-hunk
           "g p" "previous hunk"  #'diff-hl-previous-hunk
           "g h" "show hunk"      #'diff-hl-show-hunk)

;; ============================================================
;; Startup buffer and personal modules
;; ============================================================
;; Show the home buffer when Emacs starts without a file argument.
(setq initial-buffer-choice #'xl-sk-home-buffer)

;; Personal modules outside this repo.
(when (file-exists-p xl-yi-file)
  (load xl-yi-file nil t))

(provide 'init)
;;; init.el ends here
