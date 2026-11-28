;;; smoke-tests.el --- ERT checks for the Emacs profile -*- lexical-binding: t; -*-
;;
;; Run against a fully loaded profile:
;;   just emacs-test
;; The profile is loaded by the recipe before this file; every test here
;; inspects the state it leaves behind: keymaps, hooks, variables and the
;; files in tuned-emacs/.  Nothing here starts a GUI.

(require 'ert)
(require 'seq)
(require 'cl-lib)

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

(defun xl-test--git-repo-blame ()
  "Create a repository where file.txt spans two commits plus one edit."
  (let ((dir (make-temp-file "xl-test-git-blame-" t))
        (git (executable-find "git")))
    (unless git
      (error "git is not installed"))
    (cl-flet ((run (&rest args)
                (apply #'call-process git nil nil nil
                       "-C" dir "-c" "user.email=test@example.com"
                       "-c" "user.name=Test" args)))
      (run "init" "-q")
      (with-temp-file (expand-file-name "file.txt" dir)
        (insert "one\ntwo\nthree\nfour\n"))
      (run "add" "file.txt")
      (run "commit" "-qm" "initial")
      ;; A second commit owns the last two lines.
      (with-temp-file (expand-file-name "file.txt" dir)
        (insert "one\ntwo\nTHREE\nFOUR\n"))
      (run "commit" "-qam" "second")
      ;; The first line is then edited but not committed.
      (with-temp-file (expand-file-name "file.txt" dir)
        (insert "ONE\ntwo\nTHREE\nFOUR\n")))
    dir))

(defun xl-test--wait-for-git-status (buffer)
  "Wait until the Git status BUFFER has rendered and its queue is empty.
A file-watch refresh waits `xl-git-status-refresh-delay' after the last
event, so a pending one counts as work: otherwise the test looks before it
has moved the buffer."
  (with-current-buffer buffer
    (let ((key (xl-git-status--queue-key))
          (end (+ (float-time) 10)))
      (while (and (or (not buffer-read-only)
                      (xl-async-queue-busy-p key)
                      (xl-async-busy-p nil buffer)
                      (timerp (bound-and-true-p xl-git-status--watch-timer)))
                  (< (float-time) end))
        (accept-process-output nil 0.05))
      (should buffer-read-only)
      (should-not (xl-async-queue-busy-p key))
      (should-not (xl-async-busy-p nil buffer)))))

(defun xl-test--slot-show-now (buffer)
  "Make BUFFER's waiting mode-line loader appear now.
It is what happens when the operation outlives `xl-spinner-show-delay'."
  (with-current-buffer buffer
    (let ((timer (plist-get xl-spinner--slot :show-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (xl-spinner--slot-show buffer)))

(defun xl-test--wait-for-slot (buffer)
  "Wait until BUFFER's mode-line loader has gone, minimum time included."
  (let ((end (+ (float-time) 10)))
    (while (and (xl-spinner-mode-line-busy-p buffer)
                (< (float-time) end))
      (accept-process-output nil 0.05)))
  (should-not (xl-spinner-mode-line-busy-p buffer)))

(defun xl-test--wait-for-gblame (buffer)
  "Wait until the GBlame BUFFER has finished loading."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (xl-async-busy-p 'blame buffer)
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should-not (xl-async-busy-p 'blame buffer))))

(defun xl-test--wait-for-git-show (buffer)
  "Wait until the commit BUFFER has finished loading."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (xl-async-busy-p 'show buffer)
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should-not (xl-async-busy-p 'show buffer))))

(defun xl-test--wait-for-xl-dired (buffer)
  "Wait until the `xl-dired' BUFFER has its asynchronous listing."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (xl-async-busy-p 'listing buffer)
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should-not (xl-async-busy-p 'listing buffer))))

(defun xl-test--wait-for-xl-preview (buffer)
  "Wait until the `xl-dired' preview BUFFER has rendered."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (xl-async-busy-p 'preview buffer)
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should-not (xl-async-busy-p 'preview buffer))))

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
  "`xl-packages' lives in `init.el'; `personal.el' does not define it."
  (should (memq 'magit xl-packages))
  (should (memq 'org-superstar xl-packages))
  (should (memq 'evil-escape xl-packages))   ; Spacemacs habits are listed too
  ;; The completion stack comes from Crafted, not from `xl-packages'.
  (should-not (memq 'corfu xl-packages))
  (should (equal (symbol-file 'xl-packages 'defvar)
                 (expand-file-name "tuned-emacs/init.el" xl-dotfiles)))
  (dolist (entry (xl-test--code-lines (expand-file-name "tuned-emacs/personal.el" xl-dotfiles)))
    (should-not (string-match-p "defconst xl-packages" (cdr entry)))))

(ert-deftest xl-org-packages-come-from-crafted ()
  "Crafted's Org packages, including Denote, stay selected and installed."
  (dolist (pkg '(denote org-appear))
    (should (memq pkg package-selected-packages))
    (should (package-installed-p pkg))))

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

(ert-deftest xl-package-install-refreshes-a-stale-cache ()
  "The install path refreshes the archives before installing.
`package-install-selected-packages' only refreshes an empty cache; an old
one made transient's dependency compat 31.1 look unavailable."
  (let ((package-selected-packages '(xl-test-pkg))
        (installed nil)
        events)
    (cl-letf (((symbol-function 'package-installed-p)
               (lambda (pkg) (and (eq pkg 'xl-test-pkg) installed)))
              ((symbol-function 'package-refresh-contents)
               (lambda (&rest _) (push 'refresh events)))
              ((symbol-function 'package-install)
               (lambda (pkg &rest _)
                 (push (list 'install pkg) events)
                 (setq installed t))))
      (xl-install-missing-packages))
    ;; Pushed newest first: the refresh ran before the install.
    (should (equal '((install xl-test-pkg) refresh) events))))

(ert-deftest xl-package-install-reports-what-is-still-missing ()
  "A package that stays unavailable stops startup with its name."
  (let ((package-selected-packages '(xl-test-missing)))
    (cl-letf (((symbol-function 'package-installed-p) #'ignore)
              ((symbol-function 'package-refresh-contents) #'ignore)
              ((symbol-function 'package-install) #'ignore)
              ((symbol-function 'xl-refresh-package-keyring) #'ignore))
      (should-error (xl-install-missing-packages) :type 'error))))

(ert-deftest xl-package-keyring-is-rebuilt ()
  "A stale package.el GPG home is deleted and the bundled keyring imported."
  (xl-test-with-temp-dir dir
    (let ((package-gnupghome-dir (expand-file-name "gnupg" dir))
          (package-check-signature t)
          imported)
      (make-directory package-gnupghome-dir t)
      (cl-letf (((symbol-function 'package-import-keyring)
                 (lambda (file) (setq imported file))))
        (xl-refresh-package-keyring))
      (should-not (file-directory-p package-gnupghome-dir))
      (should (string-suffix-p "package-keyring.gpg" imported)))))

(ert-deftest xl-autoloads-point-to-real-definitions ()
  "Every entry in lisp/xl-autoloads.el names a definition in its library.
A stale generated file, or a cookie on a definition that was renamed,
fails here."
  (let ((file (expand-file-name "tuned-emacs/lisp/xl-autoloads.el" xl-dotfiles))
        (count 0))
    (should (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "^(autoload '\\([^ \n]+\\) \"\\([^\"]+\\)\"" nil t)
        (let* ((name (match-string 1))
               (library (match-string 2))
               (source (locate-library library)))
          (setq count (1+ count))
          (should source)
          ;; Prefer the source: an .elc is binary and hides the definition.
          (when (and source (string-suffix-p ".elc" source))
            (setq source (string-remove-suffix "c" source)))
          (should (file-exists-p source))
          (with-temp-buffer
            (insert-file-contents source)
            (goto-char (point-min))
            (should (re-search-forward
                     (concat "(\\(?:defun\\|defmacro\\|defsubst\\|"
                             "define-derived-mode\\|define-minor-mode\\) "
                             (regexp-quote name) "[ \n)]")
                     nil t))))))
    (should (> count 0))))

;;; ============================================================
;;; Package Menu loader



(ert-deftest xl-search-project-at-point-uses-grep-without-ripgrep ()
  "`SPC *' searches with ripgrep when it is installed, with grep when not."
  (require 'consult)
  (let (called)
    (cl-letf (((symbol-function 'consult-ripgrep)
               (lambda (_dir initial) (push (list 'rg initial) called)))
              ((symbol-function 'consult-grep)
               (lambda (_dir initial) (push (list 'grep initial) called)))
              ((symbol-function 'thing-at-point)
               (lambda (&rest _) "symbol-x")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (program) (and (equal program "rg") "/usr/bin/rg"))))
        (xueliang-search-project-at-point))
      (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
        (xueliang-search-project-at-point)))
    (should (equal '((grep "symbol-x") (rg "symbol-x")) called))))

(ert-deftest xl-personal-file-is-framework-neutral ()
  "`personal.el', `xl-utils.el', `xl-git-status.el' and `xl-git-commands.el' use plain Emacs APIs.
Comments are ignored (the files explain the rule in their headers).
Theme names (`spacemacs-dark', `doom-tokyo-night', `doom-themes') are
allowed."
  (dolist (file '("tuned-emacs/personal.el" "tuned-emacs/example/xl-utils.el"
                  "tuned-emacs/lisp/xl-git-status.el" "tuned-emacs/lisp/xl-git-commands.el"
                  "tuned-emacs/lisp/xl-perfetto-trace.el"))
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
  "Personal bindings must win, so `personal.el' has to load last."
  (let ((init (expand-file-name "tuned-emacs/init.el" xl-dotfiles))
        keys-line status-line commands-line personal-line)
    (dolist (entry (xl-test--code-lines init))
      (when (string-match-p "require 'xl-key-bindings" (cdr entry))
        (setq keys-line (car entry)))
      (when (string-match-p "require 'xl-git-status" (cdr entry))
        (setq status-line (car entry)))
      (when (string-match-p "require 'xl-git-commands" (cdr entry))
        (setq commands-line (car entry)))
      (when (string-match-p "tuned-emacs/personal\"" (cdr entry))
        (setq personal-line (car entry))))
    (should keys-line)
    (should status-line)
    (should commands-line)
    (should personal-line)
    (should (< keys-line status-line))
    (should (< status-line commands-line))
    (should (< commands-line personal-line))))

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
  "A package function is byte code, unless its native compile built an eln.  Emacs's own
JIT is off, so a native package function must come from the eln for consult's
current source.  Names are compared, not folders: `ert-run-tests-batch-and-exit'
points the first eln folder at a temporary cache."
  (require 'consult)
  (let* ((fn (symbol-function 'consult-line))
         (native (and (fboundp 'native-comp-function-p) (native-comp-function-p fn))))
    (should (or native (byte-code-function-p fn)))))

(ert-deftest xl-frame-starts-maximized-from-early-init ()
  "The maximized size is a frame parameter from `early-init.el', so the first
frame is created maximized; `init.el' must not set it after the frame exists."
  (should (eq (cdr (assq 'fullscreen default-frame-alist)) 'maximized))
  (dolist (entry (xl-test--code-lines (expand-file-name "tuned-emacs/init.el" xl-dotfiles)))
    (should-not (string-match-p "set-frame-parameter.*fullscreen" (cdr entry))))
  (should (cl-some (lambda (entry)
                     (string-match-p "push '(fullscreen . maximized) default-frame-alist"
                                     (cdr entry)))
                   (xl-test--code-lines
                    (expand-file-name "tuned-emacs/early-init.el" xl-dotfiles)))))

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
                  ("a d" . xl-dired)
                  ("f f" . find-file)
                  ("f r" . consult-recent-file)
                  ("f j" . xl-dired-jump)
                  ("f p" . xueliang-find-file-in-dotfiles)
                  ("h d a" . describe-symbol)
                  ("i s" . xueliang-insert-snippet)
                  ("i S" . xueliang-new-snippet)
                  ("b b" . consult-buffer)
                  ("b h" . xl-sk-home-buffer)
                  ("b s" . xueliang-open-scratch-buffer-window)
                  ("g s" . magit-status)
                  ("g g" . xl-git-status-window)
                  ("g b" . xl-gblame)
                  ("g f" . project-find-file)
                  ("j j" . avy-goto-char-timer)
                  ("j d" . xl-dired-jump)
                  ("p s" . xueliang-search-project)
                  ("q r" . xl-sk-restart-emacs)
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
                 (lambda (&rest a) (setq args a)))
                ((symbol-function 'executable-find)
                 (lambda (program) (and (equal program "rg") "/usr/bin/rg"))))
        (goto-char (point-min))
        (xueliang-search-project-at-point)
        (should (equal args '(nil "foo-bar")))
        ;; No symbol under point still opens the prompt, with no input.
        (goto-char (point-max))
        (xueliang-search-project-at-point)
        (should (equal args '(nil nil)))))))

(ert-deftest xl-function-keys ()
  "F2-F12 keep the vim/Neovim habits; F2 is the Chinese input toggle.
F3 is left out on purpose: YI.el, loaded later, takes it for 八卦."
  (dolist (spec '(("<f2>" . xueliang-toggle-chinese-input)
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

;; Text modes add ispell to completion; corfu would run it on every committed
;; character (about 230 ms a query), so the automatic list must not have it.
(ert-deftest xl-text-modes-skip-ispell-in-automatic-completion ()
  "No text mode lists `ispell-completion-at-point' among the completion functions."
  (dolist (mode '(text-mode markdown-mode org-mode))
    (with-temp-buffer
      (funcall mode)
      (should-not (memq 'ispell-completion-at-point completion-at-point-functions)))))

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

(ert-deftest xl-dired-mode-setup-and-keys ()
  "`xl-dired' looks like Dired and uses the async preview keys."
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (xl-dired-mode)
    (should (derived-mode-p 'xl-dired-mode))
    (should (eq (keymap-lookup xl-dired-mode-map "TAB")
                #'xl-dired-preview))
    (should (eq (keymap-lookup xl-dired-mode-map "RET")
                #'xl-dired-open))
    (let ((normal (evil-get-auxiliary-keymap xl-dired-mode-map 'normal t)))
      (should (eq (keymap-lookup normal "h") #'xl-dired-up-directory))
      (should (eq (keymap-lookup normal "l") #'xl-dired-open))
      (should (eq (keymap-lookup normal "RET") #'xl-dired-open))
      (should (eq (lookup-key normal (kbd "TAB")) #'xl-dired-preview)))))

(ert-deftest xl-dired-lists-a-directory-asynchronously ()
  "`xl-dired' starts `ls' in the background and shows the loader first."
  (xl-test-with-temp-dir dir
    (with-temp-file (expand-file-name "hello.txt" dir)
      (insert "hello\n"))
    (let ((buffer (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (should (xl-async-busy-p 'listing buffer))
            (with-current-buffer buffer
              (should (xl-spinner-mode-line-busy-p))
              (should (zerop (buffer-size))))
            (xl-test--wait-for-xl-dired buffer)
            (with-current-buffer buffer
              (should (derived-mode-p 'xl-dired-mode))
              (should (save-excursion
                        (goto-char (point-min))
                        (search-forward "hello.txt" nil t)))
              (should (equal (file-name-as-directory
                              (expand-file-name default-directory))
                             (file-name-as-directory dir)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest xl-dired-refresh-keeps-the-old-listing-until-ls-answers ()
  "`g' starts a new `ls' and does not empty the window while it runs."
  (xl-test-with-temp-dir dir
    (with-temp-file (expand-file-name "hello.txt" dir)
      (insert "hello\n"))
    (let ((buffer (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-test--wait-for-xl-dired buffer)
            (with-current-buffer buffer
              (let ((before (buffer-string)))
                (revert-buffer)
                (should (xl-async-busy-p 'listing buffer))
                (should (equal before (buffer-string)))
                (xl-test--wait-for-xl-dired buffer))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest xl-dired-cd-uses-the-current-window ()
  "Opening a directory replaces the listing in the selected window."
  (xl-test-with-temp-dir dir
    (let ((sub (expand-file-name "sub" dir)))
      (make-directory sub)
      (with-temp-file (expand-file-name "hello.txt" sub)
        (insert "hello\n"))
      (save-window-excursion
        (delete-other-windows)
        (let ((buffer (xl-dired-noselect dir)))
          (unwind-protect
              (progn
                (switch-to-buffer buffer)
                (xl-test--wait-for-xl-dired buffer)
                (goto-char (point-min))
                (should (re-search-forward " sub$" nil t))
                (goto-char (match-beginning 0))
                (let ((window (selected-window))
                      (count (length (window-list))))
                  (xl-dired-open)
                  (should (eq (selected-window) window))
                  (should (= (length (window-list)) count))
                  (should (derived-mode-p 'xl-dired-mode))
                  (should (equal (file-name-as-directory
                                  (expand-file-name default-directory))
                                 (file-name-as-directory sub)))
                  (xl-test--wait-for-xl-dired buffer)
                  (goto-char (point-min))
                  (should (search-forward "hello.txt" nil t))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest xl-dired-opens-a-file-in-the-other-window ()
  "RET/l in `xl-dired' selects the file in the other window."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "hello.txt" dir)))
      (with-temp-file file (insert "hello\n"))
      (save-window-excursion
        (delete-other-windows)
        (let ((buffer (xl-dired-noselect dir)))
          (unwind-protect
              (progn
                (switch-to-buffer buffer)
                (xl-test--wait-for-xl-dired buffer)
                (goto-char (point-min))
                (should (search-forward "hello.txt" nil t))
                (goto-char (match-beginning 0))
                (let ((dired-window (selected-window)))
                  (xl-dired-open)
                  (should-not (eq (selected-window) dired-window))
                  (should (equal (expand-file-name (buffer-file-name))
                                 (expand-file-name file)))))
            (when (buffer-live-p buffer) (kill-buffer buffer))
            (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))))))))

(ert-deftest xl-dired-preview-loads-a-text-file-asynchronously ()
  "TAB previews a text file through the async `head' pipeline."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "hello.txt" dir)))
      (with-temp-file file (insert "alpha\nbeta\n"))
      (let ((buffer (xl-dired-noselect dir)))
        (unwind-protect
            (progn
              (switch-to-buffer buffer)
              (xl-test--wait-for-xl-dired buffer)
              (goto-char (point-min))
              (should (search-forward "hello.txt" nil t))
              (goto-char (match-beginning 0))
              (cl-letf (((symbol-function 'display-buffer)
                         (lambda (candidate &rest _) candidate)))
                (xl-dired-preview))
              (let ((preview (get-buffer xl-dired-preview-buffer-name)))
                (xl-test--wait-for-xl-preview preview)
                (with-current-buffer preview
                  (should buffer-read-only)
                  (should (string-match-p "alpha" (buffer-string)))
                  (should (string-match-p "beta" (buffer-string))))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (when (get-buffer xl-dired-preview-buffer-name)
            (kill-buffer xl-dired-preview-buffer-name)))))))

(ert-deftest xl-dired-folders-have-no-preview ()
  "TAB on a folder shows no preview: the windows stay as they are."
  (xl-test-with-temp-dir dir
    (let ((sub (expand-file-name "sub" dir)))
      (make-directory sub)
      (let ((buffer (xl-dired-noselect dir)))
        (unwind-protect
            (progn
              (switch-to-buffer buffer)
              (xl-test--wait-for-xl-dired buffer)
              (when (get-buffer xl-dired-preview-buffer-name)
                (kill-buffer xl-dired-preview-buffer-name))
              (goto-char (point-min))
              (should (re-search-forward " sub$" nil t))
              (goto-char (match-beginning 0))
              (xl-dired-preview)
              (should-not (get-buffer xl-dired-preview-buffer-name)))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (when (get-buffer xl-dired-preview-buffer-name)
            (kill-buffer xl-dired-preview-buffer-name)))))))

(ert-deftest xl-dired-preview-says-when-a-file-is-binary ()
  "A binary or data file gets a hex dump with a note, not raw bytes."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "data.bin" dir)))
      (with-temp-file file (insert "\0\1\2data"))
      (let ((buffer (xl-dired-noselect dir)))
        (unwind-protect
            (progn
              (switch-to-buffer buffer)
              (xl-test--wait-for-xl-dired buffer)
              (goto-char (point-min))
              (should (search-forward "data.bin" nil t))
              (goto-char (match-beginning 0))
              (cl-letf (((symbol-function 'display-buffer)
                         (lambda (candidate &rest _) candidate)))
                (xl-dired-preview))
              (let ((preview (get-buffer xl-dired-preview-buffer-name)))
                (xl-test--wait-for-xl-preview preview)
                (with-current-buffer preview
                  (should (string-match-p "Binary file" (buffer-string)))
                  (should (string-match-p "^00000000  " (buffer-string)))
                  (should-not (string-match-p "\0" (buffer-string))))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (when (get-buffer xl-dired-preview-buffer-name)
            (kill-buffer xl-dired-preview-buffer-name)))))))

(ert-deftest xl-dired-schedules-a-preview-after-the-idle-delay ()
  "Point resting for `xl-dired-preview-idle' starts a preview."
  (should (= xl-dired-preview-idle 0.3))
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (xl-dired-mode)
    (xl-dired--schedule-preview)
    (should (timerp xl-dired--hover-timer))
    (cancel-timer xl-dired--hover-timer)
    (setq xl-dired--hover-timer nil)))

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
  ;; Orderless only in the minibuffer: matching every in-buffer candidate
  ;; with it tripled the cost of a completion query.
  (should (equal completion-styles '(basic)))
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
  ;; Both platforms wait for a pause and for a second character.
  (should (equal corfu-auto-delay (if (eq system-type 'windows-nt) 0.2 0.15)))
  (should (= corfu-auto-prefix 2))
  (with-temp-buffer
    (should (memq 'xueliang-cape-dabbrev completion-at-point-functions))
    ;; The guarded wrapper replaces Crafted's plain entry, which would
    ;; otherwise still scan the whole buffer on every query.
    (should-not (memq 'cape-dabbrev completion-at-point-functions))
    (should (memq 'cape-file completion-at-point-functions)))
  ;; Dabbrev must not scan every buffer of the same mode on each keystroke.
  (should (eq cape-dabbrev-buffer-function #'current-buffer)))

(ert-deftest xl-minibuffer-keeps-orderless ()
  "Buffers match by prefix; the minibuffer still gets orderless matching."
  (should (memq #'xueliang-minibuffer-orderless minibuffer-setup-hook))
  (with-temp-buffer
    (xueliang-minibuffer-orderless)
    (should (equal completion-styles '(orderless basic)))))

;; Chinese input runs inside Emacs (pyim), so the macOS IME never sits
;; between a key press and the command loop.
(ert-deftest xl-chinese-input-is-pyim ()
  "pyim is the input method, with the dictionary and popup configured."
  (should (equal default-input-method "pyim"))
  (should (assoc "pyim" input-method-alist))
  (should (eq pyim-default-scheme 'quanpin))
  (should (= pyim-page-length 9))
  (should (equal pyim-page-tooltip '(posframe minibuffer)))
  ;; Scanning the buffer for words stalls on large files.
  (should-not pyim-candidates-search-buffer-p)
  ;; Two word lists: pyim's own and THUOCL, fetched from Git by init.el.
  (dolist (name '("Basedict-elpa" "tsinghua-dict-elpa"))
    (should (seq-find (lambda (dict) (equal (plist-get dict :name) name))
                      pyim-extra-dicts)))
  (should (package-installed-p 'pyim-tsinghua-dict))
  (should (memq 'xueliang-pyim-english-p
                pyim-english-input-switch-functions)))

(ert-deftest xl-chinese-input-shows-in-the-frame-title ()
  "The frame title says (中文拼音) while pyim is the buffer's input method."
  (should (equal frame-title-format
                 '((:eval (xueliang-chinese-input-title)) "%b")))
  (with-temp-buffer
    (should (equal (xueliang-chinese-input-title) ""))
    (setq-local current-input-method "pyim")
    (should (equal (xueliang-chinese-input-title) "(中文拼音) ")))
  ;; The title is re-evaluated when the input method changes.
  (should (memq #'xueliang-refresh-frame-title input-method-activate-hook))
  (should (memq #'xueliang-refresh-frame-title input-method-deactivate-hook)))

(ert-deftest xl-pyim-keeps-commands-english ()
  "pyim types Chinese only where text is typed.
The minibuffer, isearch and Evil normal/visual state must keep their
single-key commands."
  (with-temp-buffer
    ;; No Evil insert state: commands and searches are English.
    (should (xueliang-pyim-english-p))
    (cl-letf (((symbol-function 'evil-insert-state-p) (lambda () t))
              ((symbol-function 'evil-replace-state-p) (lambda () nil)))
      (should-not (xueliang-pyim-english-p))
      ;; isearch started from insert state still searches in English.
      (let ((isearch-mode t))
        (should (xueliang-pyim-english-p))))
    (cl-letf (((symbol-function 'evil-insert-state-p) (lambda () nil))
              ((symbol-function 'evil-replace-state-p) (lambda () t)))
      (should-not (xueliang-pyim-english-p)))))

(ert-deftest xl-chinese-input-toggle-says-which ()
  "F2 toggles pyim, forces the OS layout to English, and reports the input."
  (with-temp-buffer
    (let (messages (os-switched nil))
      (cl-letf (((symbol-function 'toggle-input-method)
                 (lambda (&optional _) (setq-local current-input-method "pyim")))
                ((symbol-function 'xueliang-ime-english)
                 (lambda (&rest _) (setq os-switched t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (xueliang-toggle-chinese-input))
      (should (equal (car messages) "中文输入 (pyim)"))
      ;; The macOS IME must not eat the keys pyim is about to read.
      (should os-switched))
    (let (messages)
      (cl-letf (((symbol-function 'toggle-input-method)
                 (lambda (&optional _) (setq-local current-input-method nil)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (xueliang-toggle-chinese-input))
      (should (equal (car messages) "English")))))

(ert-deftest xl-dabbrev-skips-chinese-and-large-buffers ()
  "Automatic dabbrev would scan the buffer and offer whole Chinese sentences."
  (require 'cape)
  (with-temp-buffer
    (insert "alpha beta alpha")
    (goto-char (point-max))
    (let ((cape-dabbrev-buffer-function #'current-buffer))
      ;; Latin words complete.
      (should (xueliang-cape-dabbrev))
      ;; After a Chinese character there is no word boundary: skip.
      (insert "中文")
      (should-not (xueliang-cape-dabbrev))
      ;; Punctuation ends the Chinese run, so English after it completes.
      (insert "。")
      (should (xueliang-cape-dabbrev))
      ;; A large buffer is skipped: the scan is linear in its size.
      (insert (make-string xueliang-cape-dabbrev-max-buffer-size ?x))
      (should-not (xueliang-cape-dabbrev)))))

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

(ert-deftest xl-restart-comes-back-to-the-same-file ()
  "Restarting saves the file and point, and the next start restores them."
  (let* ((file (make-temp-file "xl-restart-file-" nil ".txt"))
         (state (make-temp-file "xl-restart-state-" nil ".eld"))
         (xl-sk-restart-file state)
         (xl-sk-restart--pending nil)
         (xl-sk-restart--buffer nil)
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "one\ntwo\nthree\nfour\nfive\n"))
          (setq buffer (find-file-noselect file))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (goto-char (point-min))
            (forward-line 4)               ; line 5
            (let ((place (xl-sk--save-restart-place)))
              (should (equal file (plist-get place :file)))
              (should (= 5 (line-number-at-pos (plist-get place :point))))))
          (should (file-exists-p state))
          ;; The next start picks the file and consumes the saved place.
          (let ((restored (xl-sk-initial-buffer-choice)))
            (should (eq restored buffer))
            (should-not (file-exists-p state))
            (set-window-buffer (selected-window) restored)
            (xl-sk--restore-restart-place)
            (with-current-buffer restored
              (should (= 5 (line-number-at-pos (point)))))
            (should (= (with-current-buffer restored (point))
                       (window-point (selected-window))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file state)))))

(ert-deftest xl-restart-from-a-temporary-buffer-goes-home ()
  "A buffer without a file saves nothing, and cancelling cleans up."
  (let* ((state (make-temp-file "xl-restart-state-" nil ".eld"))
         (xl-sk-restart-file state))
    (unwind-protect
        (progn
          (with-temp-file state (insert "(:file \"/tmp/gone\" :point 1)"))
          (with-temp-buffer
            (should-not (xl-sk--save-restart-place)))
          (should-not (file-exists-p state))
          ;; A restart that is cancelled must not leave a place behind.
          (with-temp-buffer
            (cl-letf (((symbol-function 'restart-emacs)
                       (lambda () (interactive))))
              (xl-sk-restart-emacs)))
          (should-not (file-exists-p state))
          ;; With nothing saved the home screen is the initial buffer.
          (should (eq initial-buffer-choice #'xl-sk-initial-buffer-choice))
          (cl-letf (((symbol-function 'crafted-startup-screen)
                     (lambda () 'home)))
            (should (eq 'home (xl-sk-initial-buffer-choice)))))
      (ignore-errors (delete-file state)))))

(ert-deftest xl-restart-remembers-a-dired-directory ()
  "A dired buffer is a real place, not a temporary one."
  (let* ((dir (make-temp-file "xl-restart-dired-" t))
         (state (make-temp-file "xl-restart-state-" nil ".eld"))
         (xl-sk-restart-file state)
         (buffer nil))
    (unwind-protect
        (progn
          (setq buffer (dired-noselect dir))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (let ((place (xl-sk--save-restart-place)))
              (should (plist-get place :dired)))
            (let ((restored (xl-sk-initial-buffer-choice)))
              (should (eq restored buffer))
              (with-current-buffer restored
                (should (derived-mode-p 'dired-mode))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (ignore-errors (delete-file state))
      (ignore-errors (delete-directory dir t)))))


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
;;; Coverage: functions in xueliang.el and xl-key-bindings.el
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
  "Persistence and housekeeping modes from `xueliang.el' and `xl-key-bindings.el'."
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

(ert-deftest xl-git-helpers-run-the-right-command ()
  "`Gwrite' stages the file and `Gcommit' commits it with a fixed message."
  (xl-test-with-temp-dir dir
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "note.txt" dir))
      (let ((default-directory "/")
            calls)
        (cl-letf (((symbol-function 'xl-git-commands--run)
                   (lambda (args action) (push (list args action) calls))))
          (Gwrite)
          (Gcommit))
        (setq calls (nreverse calls))
        (should (equal (caar calls) (list "add" "--" buffer-file-name)))
        (should (equal (cadar calls) "Staged note.txt"))
        (should (equal (caadr calls) (list "commit" "-m" "Update note.txt")))
        (should (equal (cadadr calls) "Committed note.txt"))
        ;; Neither command cd's the buffer.
        (should (equal default-directory "/"))))))

(ert-deftest xl-git-write-commands-join-the-repository-queue ()
  "`Gwrite' and `Gcommit' queue their command instead of running it alone.
The queue key is the repository, so they serialize with a pull."
  (xl-test-with-temp-dir dir
    (let ((file (expand-file-name "note.txt" dir))
          jobs)
      (cl-letf (((symbol-function 'xl-git-core-write)
                 (lambda (job) (push job jobs)))
                ((symbol-function 'xl-async-run)
                 (lambda (&rest _)
                   (error "A G command must not run git alone"))))
        (with-temp-buffer
          (setq buffer-file-name file
                default-directory dir)
          (Gwrite))
        (with-temp-buffer
          (setq buffer-file-name file
                default-directory dir)
          (Gcommit)))
      (setq jobs (nreverse jobs))
      (should (= (length jobs) 2))
      (should (equal (plist-get (nth 0 jobs) :attempts)
                     (list (list "add" "--" file))))
      (should (equal (plist-get (nth 0 jobs) :action) "Staged note.txt"))
      (should (equal (plist-get (nth 1 jobs) :attempts)
                     (list (list "commit" "-m" "Update note.txt"))))
      (should (equal (plist-get (nth 1 jobs) :action) "Committed note.txt"))
      (dolist (job jobs)
        (should (equal (plist-get job :root)
                       (file-name-as-directory (expand-file-name dir))))))))

(ert-deftest xl-git-write-commands-run-in-order ()
  "A Gwrite followed by a Gcommit commits the staged file.
Both run on the repository queue, so the add finishes before the commit
instead of the two racing over `.git/index.lock'."
  (skip-unless (executable-find "git"))
  (let ((dir (xl-test--git-repo)))
    (unwind-protect
        (let ((file (expand-file-name "file-0.txt" dir))
              (key (xl-git-status--queue-key
                    (file-name-as-directory dir))))
          (xl-test--git-output dir "config" "user.email" "test@example.com")
          (xl-test--git-output dir "config" "user.name" "Test")
          (with-temp-file file
            (insert "changed by the test\n"))
          (with-temp-buffer
            (setq buffer-file-name file
                  default-directory dir)
            (Gwrite)
            (Gcommit))
          (let ((deadline (+ (float-time) 30)))
            (while (and (xl-async-queue-busy-p key)
                        (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should-not (xl-async-queue-busy-p key))
          (should (equal (xl-test--git-output dir "log" "-1" "--format=%s")
                         "Update file-0.txt"))
          ;; The staged file was committed; the other edits are untouched.
          (should-not (string-match-p
                       "file-0\\.txt"
                       (xl-test--git-output dir "status" "--porcelain"))))
      (delete-directory dir t))))

(ert-deftest xl-git-status-queue-key-is-per-repository ()
  "The write queue key names the repository, however its path is written."
  (let ((one (make-temp-file "xl-queue-key-one-" t))
        (other (make-temp-file "xl-queue-key-two-" t)))
    (unwind-protect
        (let ((key (xl-git-status--queue-key one)))
          (should (equal key (xl-git-status--queue-key
                              (file-name-as-directory one))))
          (should (equal key (xl-git-status--queue-key
                              (expand-file-name "." one))))
          (should-not (equal key (xl-git-status--queue-key other))))
      (delete-directory one t)
      (delete-directory other t))))

(ert-deftest xl-just-commands-go-through-compile ()
  "`SPC c c' and the daily job run `just' asynchronously in a compile buffer.
`&' in an evil ex command hid the output; `compile' streams it and keeps
the exit status."
  (let (calls)
    (cl-letf (((symbol-function 'compile)
               (lambda (command &optional _comint)
                 (push (cons command default-directory) calls))))
      (xueliang-just-make)
      (xueliang-daily-websites))
    (setq calls (nreverse calls))
    (should (equal (mapcar #'car calls)
                   '("just all" "just daily-websites")))
    (dolist (call calls)
      (should (file-equal-p (cdr call) (expand-file-name xl-dotfiles-dir))))))

(ert-deftest xl-ime-switches-to-english-layout ()
  "Leaving insert state starts macism with the British layout, without waiting.
The switch takes about 75 ms; blocking ESC on it was the lag after typing
Chinese."
  (let ((xl-macism "macism") args)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest a) (setq args a) nil)))
      (xueliang-ime-english))
    (should (equal (plist-get args :name) "xueliang-macism"))
    (should (equal (plist-get args :command)
                   '("macism" "com.apple.keylayout.British")))
    (should (plist-get args :noquery)))
  ;; No macism installed (Windows): nothing is started.
  (let ((xl-macism nil)
        (started nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (setq started t))))
      (xueliang-ime-english))
    (should-not started)))

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

(ert-deftest xl-dired-jump-calls-xl-dired ()
  "`xueliang-dired-jump' opens the file's folder with `xl-dired'."
  (let (jumped)
    (cl-letf (((symbol-function 'xl-dired)
               (lambda (&rest _) (setq jumped t)))
              ((symbol-function 'dired-hide-details-mode) #'ignore))
      (xueliang-dired-jump))
    (should jumped)))

(ert-deftest xl-dired-sidebar-toggle-closes-existing-sidebar ()
  "Calling the sidebar command again removes the async dired window on the left."
  (xl-test-with-temp-dir dir
    (save-window-excursion
      (delete-other-windows)
      (let ((left (selected-window)))
        (set-window-buffer left (xl-dired-noselect dir))
        (let ((right (split-window-right)))
          (set-window-buffer right (get-buffer-create "xl-test-other"))
          (select-window right)
          (let ((before (length (window-list))))
            (xueliang-dired-sidebar)
            (should (= (length (window-list)) (1- before)))))))
    (dolist (buffer (buffer-list))
      (when (string-prefix-p "*xl-dired:" (buffer-name buffer))
        (kill-buffer buffer)))
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
  "`SPC b s' shows the scratch buffer on the right and reuses its window."
  (let ((source (get-buffer-create "*xl-scratch-source*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((before (length (window-list))))
            (xueliang-open-scratch-buffer-window)
            (should (= (length (window-list)) (1+ before)))
            (should (equal (buffer-name) "*scratch*"))
            (let ((scratch-window (selected-window)))
              ;; From the source buffer, a second call jumps to the existing
              ;; scratch window instead of splitting again.
              (select-window (get-buffer-window source t))
              (xueliang-open-scratch-buffer-window)
              (should (= (length (window-list)) (1+ before)))
              (should (eq (selected-window) scratch-window)))))
      (kill-buffer source))))

(ert-deftest xl-persistent-scratch-is-configured ()
  "Scratch notes persist in interactive sessions, never in batch runs."
  (should (featurep 'persistent-scratch))
  (should (equal persistent-scratch-save-file
                 (expand-file-name "persistent-scratch.eld"
                                   user-emacs-directory)))
  (should (equal persistent-scratch-autosave-interval '(idle . 30)))
  ;; The tests and setup scripts run in batch and must not touch the notes.
  (should-not persistent-scratch-autosave-mode)
  (with-current-buffer (get-scratch-buffer-create)
    (should-not persistent-scratch-mode)))

(ert-deftest xl-git-status-window-prefers-vertical-split ()
  "`SPC g g' sets a low split threshold locally, then opens Git status."
  (let (opened)
    (cl-letf (((symbol-function 'xl-git-status)
               (lambda () (setq opened t)))
              ((symbol-function 'turn-on-diff-hl-mode)
               (lambda () nil)))
      (with-temp-buffer
        (xl-git-status-window)
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
  "`SPC f e d' opens tuned-emacs/personal.el."
  (let (opened)
    (cl-letf (((symbol-function 'find-file) (lambda (f &rest _) (setq opened f))))
      (xl-sk-open-dotfile))
    (should (string-suffix-p "tuned-emacs/personal.el" opened))))

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

;; personal.el is never compiled, so it cannot have a stale .elc: its header says so.
(ert-deftest xl-personal-file-has-no-byte-compile-cookie ()
  "`personal.el' carries no-byte-compile, so compile-lisp never makes an .elc."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "tuned-emacs/personal.el" xl-dotfiles) nil 0 200)
    (should (string-match-p "no-byte-compile:[ \t]*t" (buffer-string)))))

;; Our lisp runs as byte code exactly when a current .elc exists.
(ert-deftest xl-own-lisp-is-compiled-when-built ()
  "Our lisp is compiled (byte code or native) when its .elc is current, else not."
  (let ((root (expand-file-name "tuned-emacs" xl-dotfiles)))
    (dolist (case '((xl-git-status--entries "lisp/xl-git-core.el")))
      (let* ((src (expand-file-name (cadr case) root))
             (elc (concat src "c"))
             (current (and (file-exists-p elc)
                           (not (file-newer-than-file-p src elc)))))
        ;; Compiled means byte code, or native once tier 2 has built an eln.
        (let ((f (symbol-function (car case))))
          (should (eq (and (or (byte-code-function-p f) (native-comp-function-p f)) t)
                      (and current t))))))))

;; Startup must not run compilers: ffap computes include folders with gcc,
;; clang and g++ unless its variables are already set (12 s once on macOS).
(ert-deftest xl-startup-ffap-runs-no-compiler ()
  "Init loads ffap (through Embark) with the preset include folders."
  (should (featurep 'ffap))
  (should (equal ffap-c-path '("/usr/include" "/usr/local/include")))
  (should (equal ffap-c++-path ffap-c-path)))

;; The slow-start recorder writes its log only when init was slow.
(ert-deftest xl-slow-start-report-only-when-slow ()
  "A start over the threshold writes the log with the longest stretch first;
a fast one writes nothing."
  (let* ((log (make-temp-file "startup-slow" nil ".log"))
         (before-init-time (time-subtract nil 10))
         (after-init-time nil))
    (unwind-protect
        (progn
          (delete-file log)
          (let ((xl-slow-start-seconds 100)
                (xl--start-loads (list (cons (float-time) "/x/b.elc"))))
            (setq after-init-time (current-time))
            (should-not (xl--slow-start-report log))
            (should-not (file-exists-p log)))
          (let* ((t0 (float-time before-init-time))
                 (xl-slow-start-seconds 1)
                 (xl--start-loads (list (cons (+ t0 9) "/x/slow.elc")
                                        (cons (+ t0 1) "/x/fast.elc"))))
            (should (equal (xl--slow-start-report log) log))
            (with-temp-buffer
              (insert-file-contents log)
              (should (re-search-forward "stretch, at, file):\n *8\\.000 s .*slow\\.elc" nil t)))))
      (when (file-exists-p log) (delete-file log))
      ;; The report removes the recorder; the test leaves it as it found it.
      (add-hook 'after-load-functions #'xl--note-load))))

(provide 'smoke-tests)
;;; smoke-tests.el ends here
