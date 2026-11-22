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

(defun xl-test--git-output (dir &rest args)
  "Run git ARGS in DIR and return its trimmed output."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil t nil "-C" dir args)))
      (should (zerop code))
      (string-trim (buffer-string)))))

(defun xl-test--git-repo-hunks ()
  "Create a repository whose file.txt has two separate unstaged hunks."
  (let ((dir (make-temp-file "xl-test-git-hunks-" t))
        (git (executable-find "git")))
    (unless git
      (error "git is not installed"))
    (cl-flet ((run (&rest args)
                (apply #'call-process git nil nil nil
                       "-C" dir "-c" "user.email=test@example.com"
                       "-c" "user.name=Test" args)))
      (run "init" "-q")
      (with-temp-file (expand-file-name "file.txt" dir)
        (dotimes (i 20)
          (insert (format "line %d\n" i))))
      (run "add" "file.txt")
      (run "commit" "-qm" "initial")
      ;; Lines 1 and 18 are far enough apart to stay in separate hunks.
      (with-temp-file (expand-file-name "file.txt" dir)
        (dotimes (i 20)
          (insert (if (memq i '(1 18))
                      (format "changed %d\n" i)
                    (format "line %d\n" i))))))
    dir))

(defun xl-test--wait-for-git-status (buffer)
  "Wait until the Git status BUFFER has finished rendering."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (not buffer-read-only) (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should buffer-read-only)))

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
  "`xueliang.el' and `git-status.el' use plain Emacs APIs.
Comments are ignored (the files explain the rule in their headers).
Theme names (`spacemacs-dark', `doom-tokyo-night', `doom-themes') are
allowed."
  (dolist (file '("emacs/xueliang.el" "emacs/git-status.el"))
    (dolist (entry (xl-test--code-lines (expand-file-name file xl-dotfiles)))
      (let ((line (cdr entry)))
        (should-not (string-match-p "crafted-" line))
        (should-not (string-match-p "spacemacs/" line))
        (let ((start 0))
          (while (string-match "\\_<doom-[[:alnum:]-]+" line start)
            (should (member (match-string 0 line)
                            '("doom-themes" "doom-tokyo-night")))
            (setq start (match-end 0))))))))

(ert-deftest xl-init-loads-keys-before-personal-config ()
  "Personal bindings must win, so `xueliang.el' has to load last."
  (let ((init (expand-file-name "emacs/init.el" xl-dotfiles))
        keys-line status-line personal-line)
    (dolist (entry (xl-test--code-lines init))
      (when (string-match-p "spacemacs-keys\\.el" (cdr entry))
        (setq keys-line (car entry)))
      (when (string-match-p "git-status\\.el" (cdr entry))
        (setq status-line (car entry)))
      (when (string-match-p "emacs/xueliang\\.el" (cdr entry))
        (setq personal-line (car entry))))
    (should keys-line)
    (should status-line)
    (should personal-line)
    (should (< keys-line status-line))
    (should (< status-line personal-line))))

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

(ert-deftest xl-native-comp-jit-is-off ()
  "Packages run as byte code: no JIT, and no native compile when installed."
  (when (boundp 'native-comp-jit-compilation)
    (should-not native-comp-jit-compilation))
  (should-not package-native-compile))

(ert-deftest xl-packages-load-as-byte-code ()
  "A package function is byte code, not native: no package .eln is loaded."
  (require 'consult)
  (when (fboundp 'subr-native-elisp-p)
    (should-not (subr-native-elisp-p (symbol-function 'consult-line))))
  (should (byte-code-function-p (symbol-function 'consult-line))))

(ert-deftest xl-frame-starts-maximized-from-early-init ()
  "The maximized size is a frame parameter from `early-init.el', so the first
frame is created maximized; `init.el' must not set it after the frame exists."
  (should (eq (cdr (assq 'fullscreen default-frame-alist)) 'maximized))
  (dolist (entry (xl-test--code-lines (expand-file-name "emacs/init.el" xl-dotfiles)))
    (should-not (string-match-p "set-frame-parameter.*fullscreen" (cdr entry))))
  (should (cl-some (lambda (entry)
                     (string-match-p "push '(fullscreen . maximized) default-frame-alist"
                                     (cdr entry)))
                   (xl-test--code-lines
                    (expand-file-name "emacs/early-init.el" xl-dotfiles)))))

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
                  ("/" . consult-line)
                  ("x" . execute-extended-command)
                  ("f f" . find-file)
                  ("f r" . consult-recent-file)
                  ("f p" . xueliang-find-file-in-dotfiles)
                  ("h d a" . describe-symbol)
                  ("i s" . xueliang-insert-snippet)
                  ("i S" . xueliang-new-snippet)
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
                  ("s s" . consult-line)
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

(ert-deftest xl-help-key-uses-describe-symbol ()
  "C-h a completes symbols instead of asking for an apropos pattern."
  (should (eq (keymap-lookup help-map "a") #'describe-symbol))
  (should (eq (keymap-lookup global-map "C-h a") #'describe-symbol)))

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
  ;; `org-modules' stays empty, but org-tempo is required explicitly for
  ;; the `<s' + TAB structure templates.
  (should (featurep 'org-tempo))
  (should (alist-get 'shell org-babel-load-languages))
  (should (equal (alist-get "SC" org-link-abbrev-alist nil nil #'equal)
                 "https://stockcharts.com/h-sc/ui?s=%s"))
  (should (equal (alist-get "piano" org-link-abbrev-alist nil nil #'equal)
                 "https://www.scales-chords.com/chord/piano/%s"))
  (should (member "FOCUS(f)" (cdr (car org-todo-keywords))))
  (should (equal (face-attribute 'org-level-1 :height) 1.0))
  (should (eq (face-attribute 'org-level-1 :weight) xl-font-weight)))

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

(ert-deftest xl-org-structure-template-expands ()
  "`<s' + TAB inserts a source block, also from insert state.
The other TAB command would insert two spaces, so it delegates to
`org-tempo' before falling back, like `org-cycle' in normal state."
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "<s")
    (xueliang-tab-or-corfu-next)
    (should (string-match-p "#\\+begin_src" (buffer-string)))
    (should (string-match-p "#\\+end_src" (buffer-string)))))

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

(ert-deftest xl-git-status-lists-files-and-log ()
  "The status buffer shows the head, changed files and recent commits."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (expand-file-name "sub/" repo))))
          (make-directory default-directory t)
          ;; Keep one change staged so all three sections are present.
          (should (zerop (call-process "git" nil nil nil
                                       "-C" repo "add" "file-0.txt")))
          ;; A remote ref and origin/HEAD exercise the decoration filtering.
          (dolist (ref '("refs/remotes/origin/master" "refs/remotes/origin/HEAD"))
            (should (zerop (call-process "git" nil nil nil
                                         "-C" repo "update-ref" ref "HEAD"))))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (b &rest _) (setq buffer b) b)))
            (xueliang-git-status)
            (should (buffer-live-p buffer))
            (with-current-buffer buffer
              ;; The renderer turns the buffer read-only when both commands exit.
              (let ((end (+ (float-time) 10)))
                (while (and (not buffer-read-only) (< (float-time) end))
                  (accept-process-output nil 0.05)))
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (should (equal default-directory
                             (file-name-as-directory repo)))
              (should (save-excursion
                        (goto-char (point-min))
                        (looking-at "Head: +[^ \n]+ initial")))
              ;; Same section order as `magit-status': untracked, unstaged,
              ;; staged, then the recent commits.  Match headings at line
              ;; starts so "Staged changes" cannot match inside "Unstaged".
              (let ((positions
                     (mapcar (lambda (heading)
                               (save-excursion
                                 (goto-char (point-min))
                                 (let ((case-fold-search nil))
                                   (re-search-forward
                                    (concat "^" (regexp-quote heading)) nil t))))
                             '("Untracked files" "Unstaged changes"
                               "Staged changes" "Recent commits"))))
                (should (cl-every #'identity positions))
                (should (apply #'< positions)))
              ;; Faces: Head label, section heading, status label, file path,
              ;; hash and ref.
              (should (eq (get-text-property (point-min) 'face) 'bold))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "Unstaged changes" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'xueliang-git-status-heading)))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "modified" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'xueliang-git-status-status))
                (should (search-forward "file-1.txt" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'xueliang-git-status-file)))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "Recent commits" nil t))
                (should (re-search-forward
                         "^\\([0-9a-f]+\\) origin/master initial$" nil t))
                (let ((hash-start (match-beginning 1)))
                  (should (eq (get-text-property hash-start 'face)
                              'xueliang-git-status-hash))
                  (goto-char hash-start)
                  (should (search-forward "origin/master" nil t))
                  (should (eq (get-text-property (1- (point)) 'face)
                              'xueliang-git-status-ref))))
              ;; The newest commit appears again below the log heading.
              (should (= 2 (how-many "initial" (point-min) (point-max))))
              (should (= (point) (point-min)))
              (let ((window (get-buffer-window buffer t)))
                (when window
                  (should (= (window-point window) (point-min))))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-keys ()
  "Folding, operations and RET are bound in the status mode and Evil."
  (dolist (spec '(("TAB" . xueliang-git-status-toggle)
                  ("<backtab>" . xueliang-git-status-toggle-files)
                  ("s" . xueliang-git-status-stage)
                  ("u" . xueliang-git-status-unstage)
                  ("X" . xueliang-git-status-discard)
                  ("S" . xueliang-git-status-stage-all)
                  ("U" . xueliang-git-status-unstage-all)
                  ("RET" . xueliang-git-status-visit)))
    (should (eq (keymap-lookup xueliang-git-status-mode-map (car spec))
                (cdr spec))))
  (require 'evil)
  (let ((normal (evil-get-auxiliary-keymap
                 xueliang-git-status-mode-map 'normal t)))
    (dolist (spec '(("TAB" . xueliang-git-status-toggle)
                    ("<backtab>" . xueliang-git-status-toggle-files)
                    ("s" . xueliang-git-status-stage)
                    ("u" . xueliang-git-status-unstage)
                    ("X" . xueliang-git-status-discard)
                    ("S" . xueliang-git-status-stage-all)
                    ("U" . xueliang-git-status-unstage-all)
                    ("g" . revert-buffer)
                    ("RET" . xueliang-git-status-visit)))
      (should (eq (keymap-lookup normal (car spec)) (cdr spec))))))

(ert-deftest xl-git-status-stages-and-unstages ()
  "`s' stages the file at point and `u' puts it back in the index."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xueliang-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              ;; file-0.txt is unstaged: stage it.
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xueliang-git-status-stage)
              (xl-test--wait-for-git-status buffer)
              (should (equal "file-0.txt"
                             (xl-test--git-output repo "diff" "--cached"
                                                  "--name-only")))
              ;; It is under Staged changes now: unstage it again.
              (goto-char (point-min))
              (should (search-forward "Staged changes" nil t))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xueliang-git-status-unstage)
              (xl-test--wait-for-git-status buffer)
              (should (equal "" (xl-test--git-output repo "diff" "--cached"
                                                      "--name-only"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-visits-files-and-commits ()
  "`RET' opens the file at point and shows the commit at point."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil)
        (opened nil)
        (commit nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) b))
                  ((symbol-function 'find-file)
                   (lambda (file) (setq opened file)))
                  ((symbol-function 'xueliang-git-status--show-commit)
                   (lambda (hash) (setq commit hash))))
          (let ((default-directory (file-name-as-directory repo)))
            (xueliang-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xueliang-git-status-visit)
              (should (equal opened (expand-file-name "file-0.txt" repo)))
              (goto-char (point-min))
              (should (search-forward "Recent commits" nil t))
              (should (re-search-forward "^[0-9a-f]+ " nil t))
              (xueliang-git-status-visit)
              (should (string-match-p "\\`[0-9a-f]+\\'" commit)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-visits-a-hunk-line ()
  "`RET' on a hunk opens its file at the line the hunk line maps to."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-hunks))
         (buffer nil)
         (path (expand-file-name "file.txt" repo)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory repo)))
          ;; Stub only the status window: the visits below use `find-file',
          ;; which itself goes through `display-buffer'.
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (b &rest _) (setq buffer b) b)))
            (xueliang-git-status))
          (xl-test--wait-for-git-status buffer)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "modified   file.txt")
            (beginning-of-line)
            (xueliang-git-status-toggle))
          (let ((visit-line
                 (lambda (regexp nth)
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (dotimes (_ nth) (re-search-forward regexp))
                     (beginning-of-line)
                     (xueliang-git-status-visit))
                   (with-current-buffer (get-file-buffer path)
                     (line-number-at-pos)))))
            ;; A hunk heading opens at the first line of the hunk.
            (should (= 1 (funcall visit-line "^@@" 1)))
            ;; An added line opens at the line it becomes in the file.
            (should (= 2 (funcall visit-line "^\\+changed 1" 1)))
            ;; The second hunk starts at file line 16.
            (should (= 16 (funcall visit-line "^@@" 2)))
            (should (= 19 (funcall visit-line "^\\+changed 18" 1)))
            ;; A removed line maps to where the replacement now sits.
            (should (= 19 (funcall visit-line "^-line 18" 1)))))
      (when (get-file-buffer path) (kill-buffer (get-file-buffer path)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))


(ert-deftest xl-git-status-expands-an-abbreviated-root ()
  "A dired-style ~/... root is expanded before `git -C' sees it."
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (&rest _) "~/workspace/dotfiles/")))
    (let ((default-directory temporary-file-directory))
      (should (equal (xueliang-git-status--root)
                     (file-name-as-directory
                      (expand-file-name "~/workspace/dotfiles/")))))))

(ert-deftest xl-git-status-rejects-a-non-repo ()
  "Calling the status outside a Git repository is a clear user error."
  (xl-test-with-temp-dir dir
    (let ((default-directory (file-name-as-directory dir)))
      (should-error (xueliang-git-status) :type 'user-error))))

(ert-deftest xl-git-status-folds-sections ()
  "File diffs start folded, TAB toggles hunks, and refreshes keep the state."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xueliang-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (let* ((hunk-heading
                      (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^@@")
                          (match-beginning 0))))
                     (hunk-body
                      (lambda ()
                        (save-excursion
                          (goto-char (funcall hunk-heading))
                          (forward-line 1)
                          (point))))
                     (goto-file
                      (lambda ()
                        (goto-char (point-min))
                        (search-forward "modified   file.txt")
                        (beginning-of-line))))
                ;; The whole file diff starts folded, hunk heading included.
                (should (invisible-p (funcall hunk-heading)))
                (should (eq (get-text-property (funcall hunk-heading) 'face)
                            'xueliang-git-status-hunk-heading))
                (funcall goto-file)
                (xueliang-git-status-toggle)
                (should-not (invisible-p (funcall hunk-heading)))
                (should-not (invisible-p (funcall hunk-body)))
                ;; Added and removed lines use the diff faces.
                (save-excursion
                  (goto-char (point-min))
                  (should (re-search-forward "^-line 1\\b" nil t))
                  (should (eq (get-text-property (match-beginning 0) 'face)
                              'diff-removed))
                  (should (re-search-forward "^\\+changed 1\\b" nil t))
                  (should (eq (get-text-property (match-beginning 0) 'face)
                              'diff-added)))
                ;; TAB on the hunk heading folds only that hunk.
                (goto-char (funcall hunk-heading))
                (xueliang-git-status-toggle)
                (should-not (invisible-p (funcall hunk-heading)))
                (should (invisible-p (funcall hunk-body)))
                ;; A refresh keeps the file open and the hunk folded.
                (xueliang-git-status)
                (xl-test--wait-for-git-status buffer)
                (should-not (invisible-p (funcall hunk-heading)))
                (should (invisible-p (funcall hunk-body)))
                ;; S-TAB folds every file diff, then unfolds it again.
                (xueliang-git-status-toggle-files)
                (should (invisible-p (funcall hunk-heading)))
                (xueliang-git-status-toggle-files)
                (should-not (invisible-p (funcall hunk-heading)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-stages-one-hunk ()
  "`s' on a hunk stages that hunk and leaves the other one unstaged."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xueliang-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "modified   file.txt")
              (beginning-of-line)
              (xueliang-git-status-toggle)
              (goto-char (point-min))
              (re-search-forward "^@@")
              (beginning-of-line)
              (xueliang-git-status-stage))
            (xl-test--wait-for-git-status buffer)
            (let ((cached (xl-test--git-output repo "diff" "--cached"
                                               "--unified=0"))
                  (unstaged (xl-test--git-output repo "diff" "--unified=0"))
                  (worktree (xl-test--read-file (expand-file-name "file.txt" repo))))
              (should (string-match-p "changed 1\\b" cached))
              (should-not (string-match-p "changed 18" cached))
              (should (string-match-p "changed 18" unstaged))
              (should-not (string-match-p "changed 1\\b" unstaged))
              ;; Staging a hunk touches the index, not the working tree.
              (should (string-match-p "changed 1\\b" worktree))
              (should (string-match-p "changed 18" worktree)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-unstages-one-hunk ()
  "`u' on a staged hunk takes just that hunk out of the index."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil
                                       "-C" repo "add" "file.txt")))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (b &rest _) (setq buffer b) b)))
            (let ((default-directory (file-name-as-directory repo)))
              (xueliang-git-status)
              (xl-test--wait-for-git-status buffer)
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Staged changes")
                (search-forward "modified   file.txt")
                (beginning-of-line)
                (xueliang-git-status-toggle)
                (goto-char (point-min))
                (re-search-forward "^@@")
                (beginning-of-line)
                (xueliang-git-status-unstage))
              (xl-test--wait-for-git-status buffer)
              (let ((cached (xl-test--git-output repo "diff" "--cached"
                                                 "--unified=0"))
                    (unstaged (xl-test--git-output repo "diff" "--unified=0")))
                (should-not (string-match-p "changed 1\\b" cached))
                (should (string-match-p "changed 18" cached))
                (should (string-match-p "changed 1\\b" unstaged))
                (should-not (string-match-p "changed 18" unstaged))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-discards-a-hunk ()
  "`X' reverts one hunk in the working tree after confirmation."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) b))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) t)))
          (let ((default-directory (file-name-as-directory repo)))
            (xueliang-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "modified   file.txt")
              (beginning-of-line)
              (xueliang-git-status-toggle)
              ;; The second hunk reverts line 18 and nothing else.
              (goto-char (point-min))
              (re-search-forward "^@@")
              (re-search-forward "^@@")
              (beginning-of-line)
              (xueliang-git-status-discard))
            (xl-test--wait-for-git-status buffer)
            (let ((worktree (xl-test--read-file (expand-file-name "file.txt" repo))))
              (should (string-match-p "changed 1\\b" worktree))
              (should-not (string-match-p "changed 18" worktree))
              (should (equal "" (xl-test--git-output repo "diff" "--cached"
                                                     "--name-only")))
              (let ((unstaged (xl-test--git-output repo "diff" "--unified=0")))
                (should (string-match-p "changed 1\\b" unstaged))
                (should-not (string-match-p "changed 18" unstaged))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-stages-and-unstages-a-section ()
  "`s' on a section heading stages all its files; S and U act globally."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xueliang-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Unstaged changes")
              (beginning-of-line)
              (xueliang-git-status-stage))
            (xl-test--wait-for-git-status buffer)
            (should (equal "file-0.txt\nfile-1.txt\nfile-2.txt"
                           (xl-test--git-output repo "diff" "--cached"
                                                "--name-only")))
            (with-current-buffer buffer
              (xueliang-git-status-stage-all))
            (xl-test--wait-for-git-status buffer)
            (should (string-match-p "untracked\\.txt"
                                    (xl-test--git-output repo "diff" "--cached"
                                                         "--name-only")))
            (with-current-buffer buffer
              (xueliang-git-status-unstage-all))
            (xl-test--wait-for-git-status buffer)
            (should (equal "" (xl-test--git-output repo "diff" "--cached"
                                                    "--name-only")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
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
  (should (eq (keymap-lookup vertico-map "M-w") #'embark-export))
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
  ;; Windows waits for a pause; both platforms wait for a second character.
  (should (equal corfu-auto-delay (if (eq system-type 'windows-nt) 0.2 0)))
  (should (= corfu-auto-prefix 2))
  (with-temp-buffer
    (should (memq 'cape-dabbrev completion-at-point-functions))
    (should (memq 'cape-file completion-at-point-functions)))
  ;; Dabbrev must not scan every buffer of the same mode on each keystroke.
  (should (eq cape-dabbrev-buffer-function #'current-buffer)))

(ert-deftest xl-corfu-popup-follows-the-theme ()
  "Corfu's stock near-black popup is repainted with the active theme.
Batch mode has no display, so feed the function concrete theme colors and
check where each one lands."
  (should (memq #'xueliang-corfu-theme-faces enable-theme-functions))
  (cl-letf (((symbol-function 'face-attribute)
             (lambda (face attribute &optional _frame _inherit)
               (or (alist-get (cons face attribute)
                              '(((default . :background) . "#292b2e")
                                ((default . :foreground) . "#b2b2b2")
                                ((highlight . :background) . "#444155")
                                ((vertical-border . :foreground) . "#5d4d7a")
                                ((shadow . :foreground) . "#686868"))
                              nil nil #'equal)
                   (error "Unexpected face lookup: %S %S" face attribute)))))
    (xueliang-corfu-theme-faces))
  (should (equal (face-attribute 'corfu-default :background) "#292b2e"))
  (should (equal (face-attribute 'corfu-default :foreground) "#b2b2b2"))
  (should (equal (face-attribute 'corfu-current :background) "#444155"))
  (should (equal (face-attribute 'corfu-border :background) "#5d4d7a"))
  (should (equal (face-attribute 'corfu-bar :background) "#686868")))

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

(ert-deftest xl-snippets ()
  "`SPC i s' lists yasnippets for the current mode in Vertico."
  (should (memq 'yasnippet xl-packages))
  (should (memq 'yasnippet-snippets xl-packages))
  (should (package-installed-p 'yasnippet))
  (should (package-installed-p 'yasnippet-snippets))
  (should (equal yas-prompt-functions '(yas-completing-prompt)))
  (should (commandp 'xueliang-insert-snippet))
  (should (commandp 'xueliang-new-snippet))
  (require 'yasnippet)
  (let (inserted)
    (cl-letf (((symbol-function 'yas-insert-snippet)
               (lambda () (interactive) (setq inserted t))))
      (xueliang-insert-snippet))
    (should inserted)))

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
  "The home buffer lists at most nine recent files and ends with startup time."
  (xl-sk-home-buffer)
  (should (get-buffer "*Crafted Emacs*"))
  (with-current-buffer "*Crafted Emacs*"
    (should (string-match-p
             "Open Dotfiles\n\n\n\nEmacs loaded in [0-9]+\\.[0-9][0-9] seconds[ \t\n]*\\'"
             (buffer-string))))
  (should (<= (length xl-sk--home-files) 9))
  (should (listp xl-sk--home-files)))

;;; ============================================================
;;; Kill ring, redisplay, ffap and the small Crafted defaults
;;; ============================================================

(ert-deftest xl-kill-ring-is-saved-by-savehist ()
  "The kill ring is in savehist's list and saved without text properties."
  (require 'savehist)
  (should (memq 'kill-ring savehist-additional-variables))
  (should kill-do-not-save-duplicates)
  (let ((kill-ring (list (propertize "kept" 'face 'bold) "plain" 42)))
    (run-hooks 'savehist-save-hook)
    (should (equal kill-ring '("kept" "plain")))
    (should-not (get-text-property 0 'face (car kill-ring)))))

(ert-deftest xl-redisplay-and-ffap-settings ()
  "Typing skips fontification under input; ffap does no host lookups."
  (should redisplay-skip-fontification-on-input)
  (should (eq ffap-machine-p-known 'reject)))

(ert-deftest xl-crafted-small-defaults ()
  "Mode line column, compact mode line, imenu rescan and compile scrolling."
  (should column-number-mode)
  (should (eq mode-line-compact 'long))
  (should imenu-auto-rescan)
  (should (eq compilation-scroll-output 'first-error)))

(ert-deftest xl-git-gutter-is-wired ()
  "diff-hl is selected, its keys sit under SPC g, and its hooks are set."
  (should (memq 'diff-hl xl-packages))
  (should (package-installed-p 'diff-hl))
  (should (eq (keymap-lookup xl-leader-map "g n") 'diff-hl-next-hunk))
  (should (eq (keymap-lookup xl-leader-map "g p") 'diff-hl-previous-hunk))
  (should (eq (keymap-lookup xl-leader-map "g h") 'diff-hl-show-hunk)))

;;; ============================================================
;;; Coverage: functions in xueliang.el and spacemacs-keys.el
;;; ============================================================

(defun xl-test--capture-message (thunk)
  "Run THUNK and return the last message it printed, or nil."
  (let (last)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq last (apply #'format fmt args)))))
      (funcall thunk))
    last))

(ert-deftest xl-sum-numbers-in-region ()
  "`xueliang-sum-numbers-in-region' adds whitespace-separated numbers."
  (with-temp-buffer
    (insert "1 2 3\n")
    (should (equal (xl-test--capture-message
                    (lambda () (xueliang-sum-numbers-in-region (point-min) (point-max))))
                   "Sum: 6"))))

(ert-deftest xl-replace-tab-trailing-spaces ()
  "Tabs become spaces and trailing blanks are removed from the whole buffer."
  (with-temp-buffer
    (insert "a\tb   \nc\t\n")
    (let ((tab-width 2))
      (xueliang-replace-tab-trailing-spaces))
    (should-not (string-match-p "\t" (buffer-string)))
    (should-not (string-match-p "[ \t]+$" (buffer-string)))
    (should (string-match-p "\\`a b\nc" (buffer-string)))))

(ert-deftest xl-complete-line-replaces-with-picked-line ()
  "`xueliang-complete-line' swaps the current line for the one picked."
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'xueliang-read-in-order)
               (lambda (_prompt candidates &rest _) (nth 1 candidates))))
      (xueliang-complete-line))
    (should (equal (buffer-string) "beta\nbeta\n"))))

(ert-deftest xl-cd-current-dir-follows-the-file ()
  "`xueliang-cd-current-dir' makes the file's folder the default directory."
  (xl-test-with-temp-dir dir
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "note.txt" dir))
      (let ((default-directory "/"))
        (xueliang-cd-current-dir)
        (should (file-equal-p default-directory dir))))))

(ert-deftest xl-cycle-theme-moves-to-next-and-wraps ()
  "`SPC T n' loads the next theme in `xl-sk-themes', wrapping at the end."
  (let (loaded)
    (cl-letf (((symbol-function 'load-theme)
               (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) #'ignore))
      (let ((custom-enabled-themes '(spacemacs-dark)))
        (xl-sk-cycle-theme)
        (should (eq loaded 'doom-tokyo-night)))
      (let ((custom-enabled-themes (last xl-sk-themes)))
        (xl-sk-cycle-theme)
        (should (eq loaded (car xl-sk-themes)))))))

(ert-deftest xl-copy-whole-buffer-and-file-path ()
  "`SPC y' style copies: whole buffer text, and the file path."
  (with-temp-buffer
    (insert "hello buffer")
    (xl-sk-copy-whole-buffer)
    (should (equal (current-kill 0) "hello buffer")))
  (xl-test-with-temp-dir dir
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "path.txt" dir))
      (xl-sk-copy-file-path)
      (should (equal (current-kill 0) (expand-file-name "path.txt" dir))))))

(ert-deftest xl-home-recentf-lists-nine-unique-files ()
  "The home buffer shows at most nine distinct recent files."
  (let* ((files (append (mapcar (lambda (i) (format "/tmp/xl-test-%02d.txt" i))
                                (number-sequence 1 11))
                        '("/tmp/xl-test-01.txt")))
         (recentf-list files))
    (with-temp-buffer
      (xl-sk-home-recentf)
      (should (= (length xl-sk--home-files) 9))
      (should (= (length (delete-dups (copy-sequence xl-sk--home-files)))
                 (length xl-sk--home-files))))))

(ert-deftest xl-split-windows-open-the-new-window ()
  "`SPC w v' and `SPC w s' split the window and move into the new one."
  (save-window-excursion
    (let ((before (length (window-list))))
      (xueliang-split-right)
      (should (= (length (window-list)) (1+ before)))
      (delete-other-windows)
      (xueliang-split-below)
      (should (= (length (window-list)) (1+ before))))))

(ert-deftest xl-flash-yank-is-advised ()
  "Yanking in evil flashes the region (interactive yanks only)."
  (should (advice-member-p #'xueliang-flash-yanked-text 'evil-yank))
  ;; Non-interactive calls (from code) must not pulse.
  (should-not (xueliang-flash-yanked-text (point-min) (point-min))))

(ert-deftest xl-org-open-at-point-is-advised ()
  "Org links open in a split through the RET advice."
  (should (advice-member-p #'xueliang-org-open-at-point-split-advice 'org-open-at-point)))

;;; ============================================================
;;; Coverage: hooks and modes the config relies on
;;; ============================================================

(ert-deftest xl-global-modes-are-on ()
  "Persistence and housekeeping modes from `xueliang.el' and `spacemacs-keys.el'."
  (should (bound-and-true-p savehist-mode))
  (should (bound-and-true-p recentf-mode))
  (should (bound-and-true-p save-place-mode))
  (should (bound-and-true-p global-auto-revert-mode))
  (should (bound-and-true-p winum-mode))
  (should (bound-and-true-p evil-escape-mode))
  (should (bound-and-true-p global-evil-surround-mode))
  (should-not (bound-and-true-p global-hl-line-mode)))

(ert-deftest xl-evil-habit-settings ()
  "Vim-like evil options that differ from the defaults."
  (should (equal evil-escape-key-sequence "fd"))
  (should evil-want-fine-undo)
  (should evil-want-Y-yank-to-eol))

(ert-deftest xl-save-hooks-create-folders ()
  "The folder-creating save hook is installed globally, not only in some modes."
  (should (memq #'xueliang-create-missing-directories-h
                (default-value 'before-save-hook))))

(ert-deftest xl-mode-hooks-are-installed ()
  "Major-mode setup hooks point at my setup functions."
  (should (memq #'xueliang-org-setup (default-value 'org-mode-hook)))
  (should (memq #'xueliang-eshell-setup (default-value 'eshell-mode-hook)))
  (should (memq #'xueliang-elisp-completion-setup
                (default-value 'emacs-lisp-mode-hook))))

(ert-deftest xl-diff-hl-hooks-after-load ()
  "Loading diff-hl turns the global gutter on and hooks dired and magit."
  (require 'diff-hl)
  (should (bound-and-true-p global-diff-hl-mode))
  (should (memq #'diff-hl-dired-mode (default-value 'dired-mode-hook)))
  (should (memq #'diff-hl-magit-post-refresh (default-value 'magit-post-refresh-hook))))

(ert-deftest xl-magit-display-settings ()
  "Magit splits in the current window's direction and refines the current hunk."
  (require 'magit)
  (should (eq magit-display-buffer-function #'display-buffer))
  (should magit-diff-refine-hunk))

;;; ============================================================
;;; Coverage: the SPC tree and its prefix groups
;;; ============================================================

(ert-deftest xl-leader-prefix-groups-exist ()
  "Each Spacemacs group under SPC is a keymap, so which-key can show it."
  (dolist (group '("b" "c" "f" "g" "h" "i" "j" "p" "q" "r" "s" "t" "T" "w"))
    (should (keymapp (keymap-lookup xl-leader-map group)))))

(ert-deftest xl-leader-window-and-golden-ratio-keys ()
  "Window keys and the golden ratio toggle sit where Spacemacs puts them."
  (should (eq (keymap-lookup xl-leader-map "w g") 'golden-ratio))
  (should (eq (keymap-lookup xl-leader-map "w v") 'xueliang-split-right))
  (should (eq (keymap-lookup xl-leader-map "w s") 'xueliang-split-below)))

;;; ============================================================
;;; Coverage: commands that shell out, open files or split windows
;;; ============================================================

(defun xl-test--capture-ex (thunk)
  "Run THUNK with `evil-ex-execute' stubbed; return the commands it received."
  (let (commands)
    (cl-letf (((symbol-function 'evil-ex-execute)
               (lambda (cmd &rest _) (push cmd commands))))
      (funcall thunk))
    (nreverse commands)))

(ert-deftest xl-git-helpers-run-the-right-command ()
  "`Gwrite' stages the file and `Gcommit' commits it with a fixed message."
  (should (equal (xl-test--capture-ex #'Gwrite) '("!git add %")))
  (xl-test-with-temp-dir dir
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "note.txt" dir))
      (let ((default-directory "/"))
        (should (equal (xl-test--capture-ex #'Gcommit)
                       '("!git commit -m \"Update note.txt\"")))
        ;; Gcommit cds to the file's folder before committing.
        (should (file-equal-p default-directory dir))))))

(ert-deftest xl-just-commands-run-in-the-background ()
  "`SPC c c' and the daily job send `just' to the shell with `&'."
  (should (equal (xl-test--capture-ex #'xueliang-just-make) '("!just all &")))
  (should (equal (xl-test--capture-ex #'xueliang-daily-websites)
                 '("!just daily-websites &"))))

(ert-deftest xl-ime-switches-to-english-layout ()
  "Leaving insert state calls macism with the British layout."
  (let ((xl-macism "macism") args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest a) (setq args a) 0)))
      (xueliang-ime-english))
    (should (equal (car args) "macism"))
    (should (member "com.apple.keylayout.British" args))))

(ert-deftest xl-knowledge-links-open-the-picked-url ()
  "`F12' opens the first URL in the picked line of the links file."
  (require 'org)
  (xl-test-with-temp-dir dir
    (let* ((links (expand-file-name "links.txt" dir))
           (xl-links-file links)
           opened)
      (with-temp-file links
        (insert "plain note\nhttps://example.com/a\n"))
      (cl-letf (((symbol-function 'xueliang-read-in-order)
                 (lambda (_prompt candidates &rest _) (nth 1 candidates)))
                ((symbol-function 'org-link-open-from-string)
                 (lambda (s &rest _) (setq opened s))))
        (xueliang-open-knowledge-links))
      (should (equal opened "https://example.com/a")))))

(ert-deftest xl-eshell-popup-splits-then-opens-eshell ()
  "`F5' cds to the file's folder, splits below and starts eshell there."
  (let (steps)
    (cl-letf (((symbol-function 'xueliang-cd-current-dir)
               (lambda () (push 'cd steps)))
              ((symbol-function 'xueliang-split-below)
               (lambda () (push 'split steps)))
              ((symbol-function 'eshell)
               (lambda (&rest _) (push 'eshell steps)))
              ((symbol-function 'evil-append-line)
               (lambda (&rest _) (push 'append steps))))
      (xueliang-eshell-popup))
    (should (equal (nreverse steps) '(cd split eshell append)))))

(ert-deftest xl-dired-jump-calls-dired-jump ()
  "`xueliang-dired-jump' jumps to the file's folder and shows the details."
  (let (jumped)
    (cl-letf (((symbol-function 'dired-jump)
               (lambda (&rest _) (setq jumped t)))
              ((symbol-function 'dired-hide-details-mode) #'ignore))
      (xueliang-dired-jump))
    (should jumped)))

(ert-deftest xl-dired-sidebar-toggle-closes-existing-sidebar ()
  "Calling the sidebar command again removes the dired window on the left."
  (xl-test-with-temp-dir dir
    (save-window-excursion
      (delete-other-windows)
      (let ((left (selected-window)))
        (set-window-buffer left (dired-noselect dir))
        (let ((right (split-window-right)))
          (set-window-buffer right (get-buffer-create "xl-test-other"))
          (select-window right)
          (let ((before (length (window-list))))
            (xueliang-dired-sidebar)
            (should (= (length (window-list)) (1- before)))))))
    (kill-buffer (get-buffer-create "xl-test-other"))))

(ert-deftest xl-dired-open-uses-the-other-window ()
  "`l' and RET in dired open the file in the other window."
  (let (called)
    (cl-letf (((symbol-function 'dired-find-file-other-window)
               (lambda (&rest _) (setq called t))))
      (with-temp-buffer
        (xueliang-dired-open)))
    (should called)))

(ert-deftest xl-scratch-window-opens-right-split ()
  "`SPC b s' shows the scratch buffer in a new window on the right."
  (save-window-excursion
    (delete-other-windows)
    (let ((before (length (window-list))))
      (xueliang-open-scratch-buffer-window)
      (should (= (length (window-list)) (1+ before)))
      (should (equal (buffer-name) "*scratch*")))))

(ert-deftest xl-magit-status-window-prefers-vertical-split ()
  "`SPC g g' sets a low split threshold locally, then opens Git status."
  (let (opened)
    (cl-letf (((symbol-function 'xueliang-git-status)
               (lambda () (setq opened t))))
      (with-temp-buffer
        (xueliang-magit-status-window)
        (should (= split-width-threshold 10))))
    (should opened)))

(ert-deftest xl-delete-this-file-needs-confirmation ()
  "`SPC f D' deletes the file only after a yes; a no keeps it."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "gone.txt" dir)))
      (with-temp-file file (insert "x"))
      (with-current-buffer (find-file-noselect file)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
          (xl-sk-delete-this-file))
        (should (file-exists-p file))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (xl-sk-delete-this-file))
        (should-not (file-exists-p file))))))

(ert-deftest xl-open-dotfile-opens-the-personal-config ()
  "`SPC f e d' opens emacs/xueliang.el."
  (let (opened)
    (cl-letf (((symbol-function 'find-file) (lambda (f &rest _) (setq opened f))))
      (xl-sk-open-dotfile))
    (should (string-suffix-p "emacs/xueliang.el" opened))))

(ert-deftest xl-find-file-in-dotfiles-starts-in-the-repo ()
  "`SPC f p' searches from the dotfiles folder."
  (let (seen)
    (cl-letf (((symbol-function 'find-file)
               (lambda (&rest _) (interactive) (setq seen default-directory))))
      (xueliang-find-file-in-dotfiles))
    (should (file-equal-p seen xl-dotfiles-dir))))

(ert-deftest xl-org-enter-falls-back-to-evil-ret ()
  "RET on plain Org text is evil's RET, not an Org link open."
  (let (ret)
    (cl-letf (((symbol-function 'evil-ret) (lambda (&rest _) (setq ret t))))
      (with-temp-buffer
        (org-mode)
        (insert "plain text")
        (goto-char (point-min))
        (xueliang-org-open-at-point)))
    (should ret)))

(ert-deftest xl-tab-completes-when-corfu-popup-is-open ()
  "TAB moves down the Corfu list instead of inserting spaces."
  (require 'corfu)
  (let (moved)
    (cl-letf (((symbol-function 'corfu-next) (lambda (&rest _) (setq moved t))))
      (let ((corfu-mode t) (corfu--candidates '("one" "two")))
        (with-temp-buffer
          (xueliang-tab-or-corfu-next)
          (should moved)
          (should (equal (buffer-string) "")))))))

;;; ============================================================
;;; Coverage: small helpers, macros and aliases
;;; ============================================================

(ert-deftest xl-insert-hash-types-a-hash ()
  "The Option-3 helper inserts `#' in any platform's buffer."
  (with-temp-buffer
    (xueliang-insert-hash)
    (should (equal (buffer-string) "#"))))

(ert-deftest xl-require-silently-reports-missing-features ()
  "`xueliang-require-silently' is quiet and returns nil for a missing feature."
  (should (xueliang-require-silently 'seq))
  (should-not (xueliang-require-silently 'xl-no-such-feature-for-tests)))

(ert-deftest xl-sk-bind-binds-keys-and-skips-prefix-only ()
  "`xl-sk-bind' binds KEY LABEL DEF triples and ignores a nil DEF."
  (let ((map (make-sparse-keymap)))
    (xl-sk-bind map "a b" "label" #'ignore "c" "prefix" nil)
    (should (eq (keymap-lookup map "a b") #'ignore))
    ;; The prefix-only "c" may get a which-key label, but never a command.
    (should-not (eq (keymap-lookup map "c") #'ignore))
    (should-not (functionp (keymap-lookup map "c")))))

(ert-deftest xl-elisp-completion-merges-sources ()
  "Emacs Lisp buffers get one merged completion function, not the bare list."
  (with-temp-buffer
    (xueliang-elisp-completion-setup)
    (should (functionp (car completion-at-point-functions)))
    (should (eq (cadr completion-at-point-functions) t))))

(ert-deftest xl-command-aliases-point-at-the-right-commands ()
  "The personal one-word aliases resolve to their Emacs commands."
  (should (eq (indirect-function 'xueliang-what-face) (indirect-function 'describe-face)))
  (should (eq (indirect-function 'xueliang-cap-region) (indirect-function 'capitalize-region)))
  (should (eq (indirect-function 'xueliang-org-sort) (indirect-function 'org-sort))))

;;; ============================================================
;;; Startup trace (white-screen diagnosis)
;;; ============================================================

(ert-deftest xl-startup-trace-writes-phases-outside-batch ()
  "`xl-startup-mark' appends timed phases in a real start, nothing in batch."
  (xl-test-with-temp-dir dir
    (let ((xl-startup-log (expand-file-name "startup.log" dir)))
      ;; Batch runs (tests, just recipes) must not touch the GUI's trace.
      (xl-startup-mark "batch phase")
      (should-not (file-exists-p xl-startup-log))
      (let ((noninteractive nil))
        (xl-startup-mark "phase one")
        (xl-startup-mark "phase two"))
      (let ((lines (split-string (xl-test--read-file xl-startup-log) "\n" t)))
        (should (= (length lines) 2))
        (should (string-match-p "\\`[ 0-9.]+s  phase one\\'" (nth 0 lines)))
        (should (string-match-p "phase two\\'" (nth 1 lines)))))))

(ert-deftest xl-startup-trace-marks-every-phase-in-init ()
  "init.el marks each slow phase, including the network package install."
  (let ((init (xl-test--read-file (expand-file-name "emacs/init.el" xl-dotfiles))))
    (dolist (phase '("activating packages" "installing missing packages"
                     "Crafted modules" "spacemacs-keys.el" "git-status.el"
                     "xueliang.el" "init: done"))
      (should (string-search phase init))))
  (should (fboundp 'xueliang-startup-mark))
  ;; Outside a real start, C-g stays a plain quit.
  (should-not debug-on-quit))

(provide 'smoke-tests)
;;; smoke-tests.el ends here
