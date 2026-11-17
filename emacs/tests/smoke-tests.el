;;; smoke-tests.el --- ERT checks for the Emacs profile -*- lexical-binding: t; -*-
;;
;; Run against a fully loaded profile:
;;   just emacs-test
;; The profile is loaded by the recipe before this file; every test here
;; inspects the state it leaves behind: keymaps, hooks, variables and the
;; files in emacs/.  Nothing here starts a GUI.

(require 'ert)
(require 'seq)
(require 'cl-lib)

(defconst xl-test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root, so tests can read the config files themselves.")

(defmacro xl-test-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory and run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "xl-test-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun xl-test--read-file (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun xl-test--code-lines (path)
  "Return non-comment lines of PATH, each as (LINE-NUMBER . TEXT)."
  (let ((n 0) lines)
    (dolist (line (split-string (xl-test--read-file path) "\n"))
      (setq n (1+ n))
      (unless (string-match-p "\\`[[:space:]]*;" line)
        (push (cons n line) lines)))
    (nreverse lines)))

(defun xl-test--git-repo ()
  "Create a temporary git repo with commits, edits and an untracked file."
  (let ((dir (make-temp-file "xl-test-git-" t))
        (git (executable-find "git")))
    (unless git
      (error "git is not installed"))
    (cl-flet ((run (&rest args)
                (apply #'call-process git nil nil nil
                       "-C" dir "-c" "user.email=test@example.com"
                       "-c" "user.name=Test" args)))
      (run "init" "-q")
      (dotimes (i 8)
        (with-temp-file (expand-file-name (format "file-%d.txt" i) dir)
          (insert (format "line %d\n" i))))
      (run "add" ".")
      (run "commit" "-qm" "initial")
      (dotimes (i 3)
        (with-temp-file (expand-file-name (format "file-%d.txt" i) dir)
          (insert "changed\n")))
      (with-temp-file (expand-file-name "untracked.txt" dir)
        (insert "new\n")))
    dir))

;;; ============================================================
;;; Profile and file structure
;;; ============================================================

(ert-deftest xl-profile-is-loaded ()
  "The stubs handed control to the dotfiles, and Emacs is new enough."
  (should (>= emacs-major-version 30))
  (should (bound-and-true-p xl-dotfiles))
  (should (bound-and-true-p xl-crafted-home))
  (should (file-directory-p xl-dotfiles))
  (should (file-directory-p xl-crafted-home)))

(ert-deftest xl-packages-are-listed-in-init ()
  "`xl-packages' lives in `init.el'; `xueliang.el' does not define it."
  (should (memq 'magit xl-packages))
  (should (memq 'org-superstar xl-packages))
  (should (memq 'evil-escape xl-packages))   ; Spacemacs habits are listed too
  ;; The completion stack comes from Crafted, not from `xl-packages'.
  (should-not (memq 'corfu xl-packages))
  (should (equal (symbol-file 'xl-packages 'defvar)
                 (expand-file-name "emacs/init.el" xl-dotfiles)))
  (dolist (entry (xl-test--code-lines (expand-file-name "emacs/xueliang.el" xl-dotfiles)))
    (should-not (string-match-p "defconst xl-packages" (cdr entry)))))

(ert-deftest xl-packages-are-selected-and-installed ()
  "Everything in `xl-packages' is requested by `init.el' and present on disk."
  (dolist (pkg xl-packages)
    (should (memq pkg package-selected-packages))
    (should (package-installed-p pkg))))

(ert-deftest xl-completion-packages-come-from-crafted ()
  "crafted-completion selects the completion stack; it stays selected and
installed after the theme load, so `package-autoremove' keeps it."
  (require 'package)
  (let ((removable (package--removable-packages)))
    (dolist (pkg '(vertico consult marginalia orderless embark embark-consult
                   corfu cape))
      (should (memq pkg package-selected-packages))
      (should (package-installed-p pkg))
      (should-not (memq pkg removable)))))

(ert-deftest xl-personal-file-is-framework-neutral ()
  "`xueliang.el' uses plain Emacs APIs, not Spacemacs/Doom/Crafted internals.
Comments are ignored (the file explains the rule in its header).  Theme
names (`spacemacs-dark', `doom-tokyo-night', `doom-themes') are allowed."
  (dolist (entry (xl-test--code-lines (expand-file-name "emacs/xueliang.el" xl-dotfiles)))
    (let ((line (cdr entry)))
      (should-not (string-match-p "crafted-" line))
      (should-not (string-match-p "spacemacs/" line))
      (let ((start 0))
        (while (string-match "\\_<doom-[[:alnum:]-]+" line start)
          (should (member (match-string 0 line)
                          '("doom-themes" "doom-tokyo-night")))
          (setq start (match-end 0)))))))

(ert-deftest xl-init-loads-keys-before-personal-config ()
  "Personal bindings must win, so `xueliang.el' has to load last."
  (let ((init (expand-file-name "emacs/init.el" xl-dotfiles))
        keys-line personal-line)
    (dolist (entry (xl-test--code-lines init))
      (when (string-match-p "spacemacs-keys\\.el" (cdr entry))
        (setq keys-line (car entry)))
      (when (string-match-p "emacs/xueliang\\.el" (cdr entry))
        (setq personal-line (car entry))))
    (should keys-line)
    (should personal-line)
    (should (< keys-line personal-line))))

(ert-deftest xl-early-init-settings ()
  "Startup-time settings from `early-init.el' are in effect."
  (should-not crafted-package-perform-stale-archive-check)
  (should-not crafted-init-auto-save-customized)
  (should-not crafted-init-auto-save-selected-packages)
  (should (equal package-user-dir
                 (expand-file-name (format "elpa-%d.%d"
                                           emacs-major-version emacs-minor-version)
                                   user-emacs-directory)))
  (should (equal (alist-get 'evil package-pinned-packages) "melpa"))
  (should (equal (alist-get 'evil-collection package-pinned-packages) "melpa"))
  (should (equal (cdr (assq 'menu-bar-lines default-frame-alist)) 0))
  (should (equal (cdr (assq 'tool-bar-lines default-frame-alist)) 0)))

;;; ============================================================
;;; Leader keys and function keys
;;; ============================================================

(ert-deftest xl-leader-is-reachable ()
  "SPC in normal state and M-m in insert state open `xl-leader-map'."
  (should (eq (keymap-lookup evil-normal-state-map "SPC") xl-leader-map))
  (should (eq (keymap-lookup evil-motion-state-map "SPC") xl-leader-map))
  (should (eq (keymap-lookup evil-visual-state-map "SPC") xl-leader-map))
  (should (eq (keymap-lookup evil-insert-state-map "M-m") xl-leader-map))
  (should (eq (keymap-lookup evil-emacs-state-map "M-m") xl-leader-map)))

(defun xl-test--collect-commands (map)
  "Return every command bound in MAP, descending into prefix keymaps."
  (let (commands)
    (map-keymap
     (lambda (_event def)
       (cond
        ((keymapp def)
         (setq commands (append (xl-test--collect-commands def) commands)))
        ;; Prefix keys are often stored as ("label" keymap ...).
        ((and (consp def) (keymapp (cdr def)))
         (setq commands (append (xl-test--collect-commands (cdr def)) commands)))
        ((and (consp def) (eq (car def) 'menu-item))
         (let ((cmd (nth 2 def)))
           (cond ((keymapp cmd)
                  (setq commands (append (xl-test--collect-commands cmd) commands)))
                 ((commandp cmd) (push cmd commands)))))
        ((and (consp def) (commandp (cdr def)))
         (push (cdr def) commands))
        ((commandp def) (push def commands))))
     map)
    commands))

(ert-deftest xl-leader-commands-exist ()
  "Every command reachable from the SPC tree is a real command.
A typo in `xl-sk-bind' or `xl-leader' then fails here instead of at the
keypress.  Prefix keymaps are walked recursively."
  (let ((commands (xl-test--collect-commands xl-leader-map)))
    (should (> (length commands) 80))
    (dolist (cmd (delete-dups commands))
      (should (commandp cmd))
      (should (fboundp cmd)))))

(ert-deftest xl-leader-bindings ()
  "The Spacemacs positions and my personal overrides are both present."
  (dolist (spec '(("SPC" . consult-buffer)
                  ("RET" . consult-buffer)
                  ("*" . xueliang-search-project-at-point)
                  ("/" . xueliang-search-buffer)
                  ("x" . execute-extended-command)
                  ("f f" . find-file)
                  ("f r" . consult-recent-file)
                  ("f p" . xueliang-find-file-in-dotfiles)
                  ("b b" . consult-buffer)
                  ("b h" . xl-sk-home-buffer)
                  ("b s" . xueliang-open-scratch-buffer-window)
                  ("g s" . magit-status)
                  ("g g" . xueliang-magit-status-window)
                  ("g f" . project-find-file)
                  ("j j" . avy-goto-char-timer)
                  ("p s" . consult-ripgrep)
                  ("r l" . vertico-repeat)
                  ("r y" . consult-yank-pop)
                  ("s s" . xueliang-search-buffer)
                  ("w v" . xueliang-split-right)
                  ("w s" . xueliang-split-below)
                  ("T n" . xl-sk-cycle-theme)
                  ("1" . winum-select-window-1)
                  ("9" . winum-select-window-9)))
    (should (eq (keymap-lookup xl-leader-map (car spec)) (cdr spec)))))

(ert-deftest xl-search-project-at-point ()
  "SPC * passes the whole symbol at point to `consult-ripgrep'."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert "foo-bar baz ")
    (let (args)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (&rest a) (setq args a))))
        (goto-char (point-min))
        (xueliang-search-project-at-point)
        (should (equal args '(nil "foo-bar")))
        ;; No symbol under point still opens the prompt, with no input.
        (goto-char (point-max))
        (xueliang-search-project-at-point)
        (should (equal args '(nil nil)))))))

(ert-deftest xl-function-keys ()
  "F2-F12 keep the vim/Neovim habits from `vimrc'."
  (dolist (spec '(("<f2>" . imenu)
                  ("<f4>" . evil-window-delete)
                  ("<f5>" . xueliang-eshell-popup)
                  ("<f6>" . consult-yank-pop)
                  ("<f7>" . xueliang-just-make)
                  ("<f8>" . xueliang-imenu-or-org-today)
                  ("<f9>" . xueliang-dired-sidebar)
                  ("<f10>" . xl-sk-home-buffer)
                  ("<f11>" . xueliang-org-sort-upheading)
                  ("<f12>" . xueliang-open-knowledge-links)
                  ("C-<f4>" . kill-buffer-and-window)))
    (should (eq (keymap-lookup global-map (car spec)) (cdr spec)))))

(ert-deftest xl-mac-option-3-types-hash ()
  "On macOS UK layouts Option-3 (M-3) inserts # where text is entered.
The normal state keeps M-3 as `digit-argument'."
  (skip-unless xl-mac-p)
  (dolist (map (list evil-insert-state-map evil-replace-state-map
                     evil-emacs-state-map minibuffer-local-map
                     evil-command-line-map evil-ex-completion-map
                     evil-ex-search-keymap evil-eval-map))
    (should (eq (keymap-lookup map "M-3") #'xueliang-insert-hash)))
  (should (eq (keymap-lookup isearch-mode-map "M-3")
              #'xueliang-isearch-insert-hash))
  (should (eq (keymap-lookup global-map "M-3") #'digit-argument))
  ;; Typing in replace state must overwrite, not insert before the character.
  (with-temp-buffer
    (insert "ab")
    (goto-char (point-min))
    (overwrite-mode 1)
    (xueliang-insert-hash)
    (should (equal (buffer-string) "#b")))
  ;; isearch gets the literal character, not a numeric argument.
  (let (char)
    (cl-letf (((symbol-function 'isearch-printing-char)
               (lambda (c &optional _count) (setq char c))))
      (xueliang-isearch-insert-hash))
    (should (equal char ?#))))

(ert-deftest xl-major-mode-leaders ()
  "`,` and SPC m resolve by major mode; unknown modes keep evil's `,'."
  (with-temp-buffer
    (org-mode)
    (should (eq (xl-sk--major-map) xl-org-leader-map)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (eq (xl-sk--major-map) xl-elisp-leader-map)))
  (with-temp-buffer
    (fundamental-mode)
    (should-not (xl-sk--major-map))
    (should (eq (xl-sk--comma) #'evil-repeat-find-char-reverse)))
  (with-temp-buffer
    (org-mode)
    (should (eq (xl-sk--comma) xl-org-leader-map))))

;;; ============================================================
;;; Org: settings, faces and buffer setup
;;; ============================================================

(ert-deftest xl-org-settings ()
  "Org options that differ from Crafted's defaults."
  (require 'org)
  (should (equal org-ellipsis " ▶"))
  (should (eq org-startup-folded 'content))
  (should (equal org-cycle-separator-lines 2))
  (should-not org-hide-emphasis-markers)
  (should-not org-pretty-entities)
  (should-not org-confirm-babel-evaluate)
  (should-not org-modules)
  (should (alist-get 'shell org-babel-load-languages))
  (should (equal (alist-get "SC" org-link-abbrev-alist nil nil #'equal)
                 "https://stockcharts.com/h-sc/ui?s=%s"))
  (should (equal (alist-get "piano" org-link-abbrev-alist nil nil #'equal)
                 "https://www.scales-chords.com/chord/piano/%s"))
  (should (member "FOCUS(f)" (cdr (car org-todo-keywords))))
  (should (equal (face-attribute 'org-level-1 :height) 1.0))
  (should (eq (face-attribute 'org-level-1 :weight) 'regular)))

(ert-deftest xl-org-buffer-setup ()
  "Opening an Org file runs local setup and keeps long lines unwrapped."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "note.org" dir)))
      (with-temp-file file
        (insert "* Heading\nSome text\n** Child\n- item\n"))
      (let ((buf (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buf
              (should (derived-mode-p 'org-mode))
              (should org-indent-mode)
              (should org-superstar-mode)
              (should evil-org-mode)
              (should truncate-lines)
              (should (equal (keymap-lookup org-mode-map "s-<return>")
                             #'org-meta-return))
              (should (equal (keymap-lookup
                              (evil-get-auxiliary-keymap org-mode-map 'normal t)
                              "RET")
                             #'org-open-at-point)))
          (kill-buffer buf))))))

;;; ============================================================
;;; Magit
;;; ============================================================

(ert-deftest xl-magit-status-hooks-are-pruned ()
  "The expensive status sections stay off; upstream state stays on."
  (require 'magit-status)
  (dolist (fn '(magit-insert-stashes
                magit-insert-merge-log
                magit-insert-unpulled-from-upstream
                magit-insert-unpulled-from-pushremote
                magit-insert-unpushed-to-pushremote
                magit-insert-rebase-sequence))
    (should-not (memq fn magit-status-sections-hook)))
  ;; Deliberately kept: one reason to open status is to see what is ahead.
  (should (memq 'magit-insert-unpushed-to-upstream-or-recent
                magit-status-sections-hook))
  (dolist (fn '(magit-insert-tags-header
                magit-insert-diff-filter-header
                magit-insert-upstream-branch-header
                magit-insert-push-branch-header))
    (should-not (memq fn magit-status-headers-hook)))
  (should (memq 'magit-insert-head-branch-header magit-status-headers-hook)))

(ert-deftest xl-magit-movement-keys ()
  "h/j/k/l move in Magit without losing their vim meaning."
  (require 'magit)
  (should (eq (keymap-lookup magit-mode-map "h") #'evil-backward-char))
  (should (eq (keymap-lookup magit-mode-map "j") #'magit-next-line))
  (should (eq (keymap-lookup magit-mode-map "k") #'magit-previous-line))
  (should (eq (keymap-lookup magit-mode-map "l") #'evil-forward-char)))

(ert-deftest xl-magit-status-buffer ()
  "Magit status opens and shows the head branch even without log sections."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo)))
    (unwind-protect
        (progn
          (require 'magit-status)
          (let ((buf (magit-status-setup-buffer repo)))
            (unwind-protect
                (with-current-buffer buf
                  (should (derived-mode-p 'magit-status-mode))
                  (should (save-excursion
                            (goto-char (point-min))
                            (search-forward "Head:" nil t)))
                  (should-not (save-excursion
                                (goto-char (point-min))
                                (search-forward "Stashes" nil t))))
              (kill-buffer buf))))
      (delete-directory repo t))))

;;; ============================================================
;;; Dired and Eshell
;;; ============================================================

(ert-deftest xl-dired-buffer-setup ()
  "Dired uses human-readable listings and vim-style h/l/RET."
  (xl-test-with-temp-dir dir
    (let ((buf (dired-noselect dir)))
      (unwind-protect
          (with-current-buffer buf
            (should (derived-mode-p 'dired-mode))
            (should (equal dired-listing-switches "-alh"))
            (should (equal (keymap-lookup dired-mode-map "TAB")
                           #'dired-display-file))
            (let ((normal (evil-get-auxiliary-keymap dired-mode-map 'normal t)))
              (should (eq (keymap-lookup normal "h") #'dired-up-directory))
              (should (eq (keymap-lookup normal "l") #'xueliang-dired-open))
              (should (eq (keymap-lookup normal "RET") #'xueliang-dired-open))))
        (kill-buffer buf)))))

(ert-deftest xl-eshell-setup ()
  "Eshell starts in insert state with terminal-like keys."
  (let ((buf (eshell)))
    (unwind-protect
        (with-current-buffer buf
          (should (derived-mode-p 'eshell-mode))
          (should (memq #'xueliang-eshell-setup (default-value 'eshell-mode-hook)))
          ;; Eshell may reset the state after its mode hook in batch; calling
          ;; the setup explicitly proves that "ready to type" works.
          (xueliang-eshell-setup)
          (should (eq evil-state 'insert))
          (dolist (spec '(("RET" . eshell-send-input)
                          ("C-a" . eshell-bol)
                          ("C-k" . kill-line)
                          ("C-r" . consult-history)
                          ("TAB" . completion-at-point)
                          ("C-d" . kill-buffer-and-window)))
            (should (eq (keymap-lookup evil-insert-state-local-map (car spec))
                        (cdr spec)))))
      (kill-buffer buf)))
  (should (fboundp 'eshell/e))
  (should (fboundp 'eshell/vi))
  (should (fboundp 'eshell/fzf)))

;;; ============================================================
;;; Completion and global behaviour
;;; ============================================================

(ert-deftest xl-completion-stack ()
  "The Vertico completion stack is active and configured."
  (should (featurep 'vertico))
  (should vertico-mode)
  (should (featurep 'marginalia))
  (should marginalia-mode)
  (should (assq 'orderless completion-styles-alist))
  (should (equal completion-styles '(orderless basic)))
  (should (equal completion-category-overrides
                 '((file (styles . (partial-completion))))))
  ;; Consult commands are autoloaded, the package itself loads on first use.
  (dolist (cmd '(consult-buffer consult-line consult-ripgrep consult-imenu
                 consult-recent-file consult-yank-pop consult-history))
    (should (fboundp cmd)))
  (should (eq (keymap-lookup vertico-map "RET") #'vertico-directory-enter))
  (should (eq (keymap-lookup vertico-map "C-k") #'evil-delete-line))
  (should (eq (keymap-lookup vertico-map "C-q") #'embark-export))
  (should (eq (keymap-lookup vertico-map "C-.") #'embark-act))
  ;; Normal state keeps Evil's own C-. (`evil-repeat-pop').
  (should (eq (keymap-lookup evil-normal-state-map "C-.") #'evil-repeat-pop))
  (should (memq #'vertico-repeat-save minibuffer-setup-hook))
  (should (memq #'xueliang-vertico-golden-count minibuffer-setup-hook))
  (with-temp-buffer
    (xueliang-vertico-golden-count)
    (should (integerp vertico-count))
    (should (>= vertico-count 5)))
  (should global-corfu-mode)
  (should corfu-auto)
  ;; Windows waits for a pause and a second character to keep typing smooth.
  (should (equal corfu-auto-delay (if (eq system-type 'windows-nt) 0.2 0)))
  (should (= corfu-auto-prefix (if (eq system-type 'windows-nt) 2 1)))
  (with-temp-buffer
    (should (memq 'cape-dabbrev completion-at-point-functions))
    (should (memq 'cape-file completion-at-point-functions)))
  ;; Dabbrev must not scan every buffer of the same mode on each keystroke.
  (should (eq cape-dabbrev-buffer-function #'current-buffer)))

(ert-deftest xl-embark-collect-after-consult-3-4 ()
  "Embark Collect works despite Crafted's stale Consult hook.
`embark-export' falls back to Collect for types without a dedicated
exporter (imenu), and Consult 3.4 removed `consult-preview-at-point-mode',
which Crafted still puts on `embark-collect-mode-hook'."
  (require 'embark-consult)
  (require 'consult-imenu)
  (should-not (memq 'consult-preview-at-point-mode embark-collect-mode-hook))
  (let ((transformed '(:orig-type imenu :type imenu
                       :candidates ("alpha" "beta")
                       :orig-candidates ("alpha" "beta")
                       :bounds (nil nil))))
    (cl-letf (((symbol-function 'embark--maybe-transform-candidates)
               (lambda () transformed)))
      ;; Run from a displayed buffer: Marginalia needs a window to compute
      ;; its annotation width, and ERT leaves an undisplayed temp buffer current.
      (with-current-buffer (get-buffer-create "*scratch*")
        (embark-export))
      (let ((buf (seq-find (lambda (b)
                             (string-prefix-p "*Embark Collect"
                                              (buffer-name b)))
                           (buffer-list))))
        (unwind-protect
            (progn
              (should buf)
              (with-current-buffer buf
                (should (eq major-mode 'embark-collect-mode))
                (should (eq embark--type 'imenu))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest xl-ivy-stack-is-retired ()
  "The old Ivy stack is gone from the package list, the UI and the selection."
  (dolist (pkg '(ivy counsel swiper ivy-rich))
    (should-not (memq pkg xl-packages))
    (should-not (memq pkg package-selected-packages)))
  (should-not (featurep 'ivy))
  (should-not (featurep 'counsel)))

(ert-deftest xl-global-editing-settings ()
  "Indentation, wrapping and process settings shared with Neovim."
  (should-not indent-tabs-mode)
  (should (= tab-width 2))
  (should (= standard-indent 2))
  (should (= split-width-threshold 160))
  (should (= read-process-output-max (* 1024 1024)))
  (should (eq ring-bell-function #'ignore))
  (should-not bidi-display-reordering)
  (should (= recentf-max-saved-items 1000))
  (should (= which-key-idle-delay 0.3))
  (with-temp-buffer
    (should truncate-lines))            ; `set nowrap' in every buffer
  (should (equal backup-directory-alist
                 `(("." . ,(expand-file-name "backups" user-emacs-directory))))))

(ert-deftest xl-theme-is-loaded ()
  "One of the two configured dark themes is active."
  (should (seq-intersection custom-enabled-themes
                            '(spacemacs-dark doom-tokyo-night))))

(ert-deftest xl-idle-require-disabled-for-nonpositive-priority ()
  "Priority 0 or below leaves no timer and returns nil."
  (let ((before (length timer-idle-list)))
    (should-not (xl-idle-require 'org 0))
    (should-not (xl-idle-require 'org -1))
    (should (= before (length timer-idle-list)))))

(ert-deftest xl-idle-require-registers-one-shot-timer ()
  "A positive priority registers a cancellable idle timer."
  (let* ((before (length timer-idle-list))
         (timer (xl-idle-require 'seq 5)))
    (should (timerp timer))
    (should (= (1+ before) (length timer-idle-list)))
    (cancel-timer timer)
    (should (= before (length timer-idle-list)))))

(ert-deftest xl-tab-inserts-two-spaces-without-corfu ()
  "TAB keeps my two-space indent when no completion popup is open."
  (with-temp-buffer
    (xueliang-tab-or-corfu-next)
    (should (equal (buffer-string) "  "))))

(ert-deftest xl-create-missing-directories-before-save ()
  "Saving into a new folder creates the parents, like `mkdir -p'."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "a/b/c.txt" dir)))
      (with-temp-buffer
        (setq buffer-file-name file)
        (xueliang-create-missing-directories-h)
        (should (file-directory-p (file-name-directory file)))))))

(ert-deftest xl-read-in-order-keeps-input-order ()
  "My link/line pickers opt out of the completion UI's sorting."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _)
               (funcall collection "" nil 'metadata))))
    (should (equal (xueliang-read-in-order "Pick" '("b" "a"))
                   '(metadata (display-sort-function . identity)
                              (cycle-sort-function . identity))))))

(ert-deftest xl-relative-line-numbers-toggle ()
  "SPC t r toggles relative line numbers in the current buffer."
  (with-temp-buffer
    (xl-sk-relative-line-numbers)
    (should (eq display-line-numbers 'relative))
    (xl-sk-relative-line-numbers)
    (should-not display-line-numbers)))

(ert-deftest xl-home-buffer ()
  "The home buffer exists and lists at most nine recent files."
  (xl-sk-home-buffer)
  (should (get-buffer "*Crafted Emacs*"))
  (should (<= (length xl-sk--home-files) 9))
  (should (listp xl-sk--home-files)))

(provide 'smoke-tests)
;;; smoke-tests.el ends here
