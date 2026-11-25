;;; xl-utils.el --- my helper functions, behind the short bindings in xueliang.el -*- lexical-binding: t; -*-

;; Commentary:
;; The logic that backs my bindings and settings.  xueliang.el keeps the
;; settings, key bindings and short combinations of functions; anything with
;; real logic (branches, loops, process calls, buffers) lives here.  Every
;; function keeps its name, so bindings in xueliang.el and tests do not change.

;;; Code:

;; Set by xueliang.el; declared here so the byte compiler knows them.
(defvar xl-dotfiles-dir)
(defvar xl-links-file)
(defvar xl-macism)

;; ============================================================
;; macOS UK Keyboard - Option-3 types #
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
;; First-use latency - pay one-time costs while idle
;; ============================================================
;; Emacs cannot load packages asynchronously in-process.
;; Idle timers are the safe compromise: they run only when you are not typing.
(defun xueliang-require-silently (feature)
  "Require FEATURE without echo-area or *Messages* output."
  (let ((inhibit-message t) (message-log-max nil)) (ignore-errors (require feature))))

(defun xl-idle-require (package priority)
  "Load PACKAGE once Emacs has been idle for PRIORITY half-seconds.
PRIORITY 0 or below disables the load: no timer is registered, so a
line can be switched off without deleting it."
  ;; Deliberately a plain `when': Emacs treats a zero or negative idle
  ;; delay as "ripe now", the opposite of switching the load off.
  (when (> priority 0)
    (run-with-idle-timer
     (* 0.5 priority) nil
     (lambda ()
       ;; Outside the silent `let' in the helper, so a failure still shows.
       (unless (xueliang-require-silently package)
         (message "xl-idle-require: could not load %s" package))))))

;; Saving a file in a new folder creates the folder, like `mkdir -p'
(defun xueliang-create-missing-directories-h ()
  "Create missing parent directories before saving current file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (make-directory (file-name-directory buffer-file-name) t)))

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

(defun xueliang-flash-yanked-text (beg end &rest _)
  (when (called-interactively-p 'any)
    (pulse-momentary-highlight-region beg end 'xueliang-yank-flash)))

;; Split and move into the new window, like vim's :vs / :sp
(defun xueliang-split-right () (interactive) (select-window (split-window-right)))

(defun xueliang-split-below () (interactive) (select-window (split-window-below)))

;; TAB inserts two spaces, as in my vimrc; but in an open Corfu popup it
;; moves down like C-n. Needed because evil's insert-state map (an
;; emulation keymap) shadows `corfu-map', so TAB cannot be bound there.
;; The corfu section below adds this command to `corfu-continue-commands',
;; so the popup stays open instead of accepting the candidate after TAB.
;; In Org it also expands structure templates: `<s' + TAB is a src block,
;; which `org-cycle' does in normal state but this command would hide.
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

(defun xueliang-ime-english ()
  (call-process xl-macism nil 0 nil "com.apple.keylayout.British"))

;; Golden ratio: the candidate list takes 38.2% of the frame height, like
;; the F9 sidebar.  Computed for every minibuffer, so a resized frame gets
;; a resized list, exactly like the old `ivy-height-alist' function.
(defun xueliang-vertico-golden-count ()
  "Size the Vertico list to the golden ratio of the frame height."
  (setq-local vertico-count (max 5 (round (* 0.382 (frame-height))))))

;; Project search: ripgrep where it is installed, grep where it is not (on
;; Windows, by default).  consult-grep is asynchronous too, so the same
;; indicator animates.  `SPC p s', `SPC s p' and `SPC *' all go through it.
(defun xueliang-search-project (&optional initial)
  "Search the project with ripgrep, or with grep where ripgrep is not installed.
INITIAL is the text the search starts with."
  (interactive)
  (if (executable-find "rg")
      (consult-ripgrep nil initial)
    (consult-grep nil initial)))

;; `SPC *': like vim/nvim's <leader>*, search the project for the symbol
;; under the cursor.  No symbol under point just opens an empty prompt.
(defun xueliang-search-project-at-point ()
  "Search the project for the symbol at point."
  (interactive)
  (xueliang-search-project (thing-at-point 'symbol t)))

;; Corfu ships theme-neutral colors: a near-black popup (#191a1b) with a
;; teal selection.  Paint it with the active theme instead, so the popup
;; looks like a small floating window: the buffer background, the region
;; color for the current candidate, and the window-divider purple as its
;; border.  Recompute after a theme switch (SPC T n).
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

;; `emacs-lisp-mode' installs its own buffer-local CAPF, so the global
;; `cape-dabbrev' above never runs. Merge it in: symbols typed in this
;; file (even before they are evaluated) now complete too.
(defun xueliang-elisp-completion-setup ()
  "Merge buffer words into Emacs Lisp completion."
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'elisp-completion-at-point
                                     #'cape-dabbrev
                                     #'cape-file)
                    t)))

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
    "C-r"        #'consult-history
    "TAB"        #'completion-at-point
    "C-d"        #'kill-buffer-and-window
    "<f1>"       #'evil-force-normal-state) ; F1 acts like ESC, just like tmux
  (evil-insert-state))

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
      (consult-line (format-time-string "<%Y-%m-%d"))
    (consult-imenu)))

;; F12: fast search in my pre-compiled URL list
(defun xueliang-open-knowledge-links ()
  "Pick a line from my links file and open its URL or file link." (interactive)
  (let ((line (xueliang-read-in-order
               "Knowledge Link: "
               (with-temp-buffer (insert-file-contents xl-links-file)
                                 (split-string (buffer-string) "\n" t)))))
    (when (string-match "\\(\\(?:https?://\\|file:\\)[^ \t]+\\)" line)
      (require 'org) (org-link-open-from-string (match-string 1 line)))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (apply #'+ (mapcar #'string-to-number
                                        (split-string (buffer-substring start end))))))

(defun xueliang-open-scratch-buffer-window ()
  "Show the persistent scratch buffer on the right, or jump to it."
  (interactive)
  (let ((buffer (get-scratch-buffer-create)))
    (if-let* ((window (get-buffer-window buffer t)))
        (select-window window)
      (xueliang-split-right)
      (switch-to-buffer buffer))))

(defun xl-git-status-window ()
  "Open my Git status, preferring a right split." (interactive)
  (setq-local split-width-threshold 10) ; prefer vsplit
  (xueliang-cd-current-dir)
  (turn-on-diff-hl-mode) (xl-git-status))

(defun xueliang-just-run (&rest args)
  "Run `just' with ARGS in the dotfiles, asynchronously.
ARGS are the recipe and its arguments.  Output streams into
`*compilation*', so `next-error' jumps to the failing file:line, the
buffer keeps the log, and a failed recipe stays visible instead of
disappearing with the shell command's temporary buffer."
  (let ((default-directory (expand-file-name xl-dotfiles-dir)))
    (compile (mapconcat #'identity (cons "just" args) " "))))

(provide 'xl-utils)

;;; xl-utils.el ends here
