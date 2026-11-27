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
    (let ((end (+ (float-time) 10)))
      (while (and (or (not buffer-read-only)
                      (xl-async-queue-busy-p 'queue buffer)
                      (xl-async-busy-p nil buffer)
                      (timerp (bound-and-true-p xl-git-status--watch-timer)))
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should buffer-read-only)
    (should-not (xl-async-queue-busy-p 'queue buffer))
    (should-not (xl-async-busy-p nil buffer))))

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

;;; ============================================================
;;; Package Menu loader
(ert-deftest xl-spinner-frames-ticks-and-stops ()
  "The shared loader draws a frame at once, advances, limits and clears."
  (with-temp-buffer
    (let ((xl-spinner-style 'ascii)
          drawn)
      (xl-spinner-start (lambda (text) (push text drawn)) :limit 1)
      ;; The first frame is on screen before the caller gets control back.
      (should (xl-spinner-running-p))
      (should (equal (car xl-spinner-frames) (car drawn)))
      (xl-spinner-tick (current-buffer))
      (should (equal (cadr xl-spinner-frames) (car drawn)))
      ;; The limit stops the loader on the next tick and clears it.
      (xl-spinner-tick (current-buffer))
      (should-not (xl-spinner-running-p))
      (should (equal '(nil "[/]" "[|]") drawn)))
    ;; Starting again resets the frames; stopping clears the drawing.
    (let ((xl-spinner-style 'ascii)
          drawn)
      (xl-spinner-start (lambda (text) (push text drawn)))
      (should (xl-spinner-running-p))
      (xl-spinner-stop)
      (should-not (xl-spinner-running-p))
      (should (equal '(nil "[|]") drawn)))))

;;; ============================================================
;;; xl-spinner: frames per display and the mode-line slot
;;; ============================================================

(ert-deftest xl-spinner-frames-fall-back-to-ascii ()
  "Dots only when every dot glyph keeps the mode line's size, else ASCII."
  (let ((xl-spinner-style 'auto))
    (cl-letf (((symbol-function 'xl-spinner--glyph-safe-p) #'always))
      (should (equal (xl-spinner-default-frames) xl-spinner-dot-frames)))
    ;; One glyph from a fallback font (taller mode line) means ASCII.
    (cl-letf (((symbol-function 'xl-spinner--glyph-safe-p)
               (lambda (char) (/= char ?●))))
      (should (equal (xl-spinner-default-frames) xl-spinner-frames))))
  (let ((xl-spinner-style 'ascii))
    (should (equal (xl-spinner-default-frames) xl-spinner-frames)))
  (let ((xl-spinner-style 'dots))
    (should (equal (xl-spinner-default-frames) xl-spinner-dot-frames)))
  ;; A terminal without UTF-8 (e.g. LANG=C over ssh) gets ASCII.
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
            ((symbol-function 'terminal-coding-system)
             (lambda (&rest _) 'us-ascii)))
    (should-not (xl-spinner--glyph-safe-p ?●))))

(ert-deftest xl-spinner-slot-never-flashes-a-fast-operation ()
  "Work that ends before the delay leaves the mode line untouched."
  (with-temp-buffer
    (xl-spinner-mode-line-install)
    (xl-spinner-mode-line-start "Staging")
    (should (xl-spinner-mode-line-busy-p))
    (should-not (xl-spinner-running-p))
    (should-not xl-spinner-mode-line)
    (xl-spinner-mode-line-stop)
    (should-not (xl-spinner-mode-line-busy-p))
    (should-not xl-spinner-mode-line)))

(ert-deftest xl-spinner-slot-lingers-and-keeps-its-width ()
  "A shown loader stays its minimum time, a restart keeps it, it never narrows."
  (with-temp-buffer
    (let ((xl-spinner-show-delay 0)
          (xl-spinner-min-duration 0.2)
          (xl-spinner-style 'ascii))
      (xl-spinner-mode-line-install)
      (xl-spinner-mode-line-start "Pushing main to origin")
      (should (equal " [|] Pushing main to origin " xl-spinner-mode-line))
      ;; A shorter label keeps the width, so nothing after the slot moves.
      (xl-spinner-mode-line-label "Refreshing")
      (should (equal (string-width " [|] Pushing main to origin ")
                     (string-width xl-spinner-mode-line)))
      (should (string-prefix-p " [|] Refreshing " xl-spinner-mode-line))
      ;; Stopping right away lingers instead of flashing off.
      (xl-spinner-mode-line-stop)
      (should (xl-spinner-running-p))
      ;; New work during the linger takes the same loader over.
      (xl-spinner-mode-line-start "Staging")
      (should (string-prefix-p " [|] Staging " xl-spinner-mode-line))
      (xl-spinner-mode-line-stop)
      (xl-test--wait-for-slot (current-buffer))
      (should-not (xl-spinner-running-p))
      (should-not xl-spinner-mode-line))))

(ert-deftest xl-spinner-slot-also-draws-each-frame ()
  "`:also' gets every frame the slot draws, and nil when the slot empties."
  (dolist (stop '(plain with-text))
    (with-temp-buffer
      (let ((xl-spinner-show-delay 0)
            (xl-spinner-min-duration 0)
            (xl-spinner-style 'ascii)
            drawn)
        (xl-spinner-mode-line-install)
        (xl-spinner-mode-line-start "work" :also (lambda (frame) (push frame drawn)))
        (xl-spinner-tick (current-buffer))
        (should (equal '("[/]" "[|]") (mapcar #'substring-no-properties drawn)))
        (if (eq stop 'plain)
            (xl-spinner-mode-line-stop)
          (xl-spinner-mode-line-stop "failed"))
        ;; The last call is the nil that clears it, made once.
        (should (null (car drawn)))
        (should (= 1 (cl-count nil drawn)))))))

(ert-deftest xl-spinner-slot-shows-text-after-stop ()
  "A stop with text (an error) replaces the loader at once and stays."
  (with-temp-buffer
    (let ((xl-spinner-show-delay 0))
      (xl-spinner-mode-line-install)
      (xl-spinner-mode-line-start "git blame")
      (xl-spinner-mode-line-stop "git blame failed: 100% broken")
      (should-not (xl-spinner-running-p))
      (should-not (xl-spinner-mode-line-busy-p))
      ;; `%' is escaped, because the slot is a mode line construct.
      (should (equal " git blame failed: 100%% broken " xl-spinner-mode-line))
      ;; The next start clears it.
      (xl-spinner-mode-line-start "git blame")
      (should-not (string-match-p "failed" (or xl-spinner-mode-line ""))))))

(ert-deftest xl-spinner-tint-blends-toward-the-loader-colour ()
  "The tint moves the background by `xl-spinner-tint' toward the accent."
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216))
            ((symbol-function 'face-background) (lambda (&rest _) "#000000"))
            ((symbol-function 'face-foreground) (lambda (&rest _) "#ffffff")))
    (let ((xl-spinner-tint 0.2))
      (should (equal "#333333" (xl-spinner--tint-color 'mode-line-active))))
    (let ((xl-spinner-tint nil))
      (should-not (xl-spinner--tint-color 'mode-line-active)))))

;;; ============================================================
;;; xl-async: owned processes, slots and queues
;;; ============================================================

(defun xl-test--async-wait (pred)
  "Spin the event loop until PRED returns non-nil, at most 10 seconds."
  (let ((end (+ (float-time) 10)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (should (funcall pred))))

(ert-deftest xl-async-run-delivers-output-in-the-owner ()
  "ON-DONE gets the output and exit code, with the owner buffer current."
  (with-temp-buffer
    (let ((owner (current-buffer)) result)
      (xl-async-run :command '("sh" "-c" "printf hi; exit 3")
                    :on-done (lambda (output code)
                               (setq result (list output code (current-buffer)))))
      (should (xl-async-busy-p))
      (xl-test--async-wait (lambda () result))
      (should (equal result (list "hi" 3 owner)))
      (should-not (xl-async-busy-p)))))

(ert-deftest xl-async-stdin-reaches-the-process ()
  "STDIN is written and closed, so a reader such as `git apply' finishes."
  (with-temp-buffer
    (let (result)
      (xl-async-run :command '("cat") :stdin "patch text\n"
                    :on-done (lambda (output _code) (setq result output)))
      (xl-test--async-wait (lambda () result))
      (should (equal result "patch text\n")))))

(ert-deftest xl-async-slot-cancels-the-older-run ()
  "A newer run in the same slot kills the older one, whose ON-DONE never runs."
  (with-temp-buffer
    (let (calls)
      (xl-async-run :command '("sleep" "5") :slot 'test
                    :on-done (lambda (&rest _) (push 'old calls)))
      (xl-async-run :command '("true") :slot 'test
                    :on-done (lambda (&rest _) (push 'new calls)))
      (xl-test--async-wait (lambda () (not (xl-async-busy-p 'test))))
      ;; Give a stray sentinel of the killed run a chance to fire.
      (accept-process-output nil 0.1)
      (should (equal calls '(new))))))

(ert-deftest xl-async-killing-the-owner-kills-its-processes ()
  "A killed owner takes its processes with it, even after a mode change."
  (let* ((buffer (generate-new-buffer " *xl-async-test*"))
         (proc (with-current-buffer buffer
                 (prog1 (xl-async-run :command '("sleep" "5") :slot 'test)
                   ;; A major mode resets buffer-locals; ownership survives.
                   (fundamental-mode)))))
    (should (xl-async-busy-p 'test buffer))
    (kill-buffer buffer)
    (should-not (process-live-p proc))))

(ert-deftest xl-async-unowned-run-outlives-buffers ()
  "With :owner nil, ON-DONE runs and no buffer tracks the process."
  (let (result)
    (with-temp-buffer
      (xl-async-run :command '("echo" "done") :owner nil
                    :on-done (lambda (output _code) (setq result output)))
      (should-not (xl-async-busy-p)))
    (xl-test--async-wait (lambda () result))
    (should (equal result "done\n"))))

(ert-deftest xl-async-queue-runs-in-order-and-retries ()
  "Jobs run one at a time; a failed attempt falls back; drain runs once."
  (with-temp-buffer
    (let (events)
      (dolist (n '(1 2))
        (xl-async-enqueue
         'q (list :attempts (list '("false") (list "echo" (number-to-string n)))
                  :on-start (lambda () (push (list 'start n) events))
                  :on-done (lambda (output code)
                             (push (list 'done (string-trim output) code) events)))
         (lambda () (push 'drained events))))
      (should (= 1 (xl-async-queue-length 'q)))
      (should (xl-async-queue-busy-p 'q))
      (xl-test--async-wait (lambda () (memq 'drained events)))
      (should (equal (nreverse events)
                     '((start 1) (done "1" 0) (start 2) (done "2" 0) drained)))
      (should-not (xl-async-queue-busy-p 'q)))))

(ert-deftest xl-git-status-parses-nul-separated-status ()
  "`-z' status gives raw paths: spaces stay unquoted, renames use the new path."
  (let* ((status (concat "## main...origin/main [ahead 1]\0"
                         " M a b.txt\0"
                         "R  new name\0plain\0"
                         "?? ü.txt\0"))
         (entries (xl-git-status--entries status)))
    (should (equal (xl-git-status--branch status) "main"))
    (should (equal (xl-git-status--upstream status) "origin/main"))
    (should (equal entries '((("renamed" . "new name"))
                             (("modified" . "a b.txt"))
                             (("untracked" . "ü.txt")))))))

(ert-deftest xl-git-status-splits-a-whole-repo-diff ()
  "One `git diff' splits into the per-file diffs the sections show."
  (let* ((text (concat "diff --git a/a b.txt b/a b.txt\n"
                       "index 1..2 100644\n--- a/a b.txt\t\n+++ b/a b.txt\t\n"
                       "@@ -1 +1,2 @@\n a\n+b\n"
                       "diff --git a/plain b/new name\n"
                       "similarity index 100%\nrename from plain\nrename to new name\n"
                       "diff --git a/img.png b/img.png\n"
                       "Binary files a/img.png and b/img.png differ\n"))
         (files (xl-git-status--split-files text)))
    (should (equal (mapcar #'car files) '("a b.txt" "new name" "img.png")))
    (should (string-prefix-p "diff --git a/a b.txt" (cdr (nth 0 files))))
    (should (string-suffix-p "+b\n" (cdr (nth 0 files))))
    (should (string-match-p "rename to new name\n\\'" (cdr (nth 1 files))))))

;;; ============================================================
;;; ============================================================

(defun xl-test-menu-state (buffer)
  "Return what the Package Menu shows in BUFFER: text, rows and point."
  (with-current-buffer buffer
    (list (buffer-string) tabulated-list-entries (point))))

(ert-deftest xl-git-status-reads-do-not-rewrite-the-index ()
  "Every status command carries --no-optional-locks, so a refresh does not
rewrite .git/index and retrigger the auto refresh."
  (should (member "--no-optional-locks"
                  (xl-git-status--git-command '("status" "--porcelain=v1"))))
  (should (equal "--no-optional-locks"
                 (nth 2 (xl-git-status--git-command '("status"))))))

(ert-deftest xl-git-status-watch-ignores-noise ()
  "Lock files, editor backups and most of .git do not refresh the status."
  (let ((root "/repo/"))
    (should (xl-git-status--relevant-event-p root "/repo/src/a.el"))
    (should (xl-git-status--relevant-event-p root "/repo/.git/index"))
    (should (xl-git-status--relevant-event-p root "/repo/.git/HEAD"))
    (should (xl-git-status--relevant-event-p root "/repo/.git/refs/heads/main"))
    (should-not (xl-git-status--relevant-event-p root "/repo/.git/index.lock"))
    (should-not (xl-git-status--relevant-event-p root "/repo/.git/objects/ab/cd"))
    (should-not (xl-git-status--relevant-event-p root "/repo/.git/logs/HEAD"))
    (should-not (xl-git-status--relevant-event-p root "/repo/a.el~"))
    (should-not (xl-git-status--relevant-event-p root "/repo/.#a.el"))
    (should-not (xl-git-status--relevant-event-p root "/repo/#a.el#"))))

(ert-deftest xl-git-status-watch-dirs-include-parents ()
  "The directories of the listed files, each with its parents, root first."
  (let ((dirs (xl-git-status--watch-dirs "/repo" '("a.el" "src/b.el" "src/x/c.el"))))
    (should (equal (car dirs) "/repo"))
    (should (member "/repo/src" dirs))
    (should (member "/repo/src/x" dirs))
    (should (= (length dirs) (length (delete-dups (copy-sequence dirs)))))))

(ert-deftest xl-git-status-debounces-a-burst-into-one-refresh ()
  "Several changes in a row refresh the visible status once, after the delay."
  (let ((xl-git-status-refresh-delay 0.1)
        (refreshes 0)
        (buffer (generate-new-buffer " *status-watch-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'xl-git-status)
                   (lambda () (cl-incf refreshes)))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'window)))
          (with-current-buffer buffer
            (setq default-directory "/repo/"))
          (dotimes (_ 3)
            (xl-git-status--on-event buffer '(1 changed "/repo/a.el")))
          (should (= refreshes 0))
          (sit-for 0.3)
          (should (= refreshes 1)))
      (kill-buffer buffer))))

(ert-deftest xl-git-status-hidden-buffer-is-not-refreshed ()
  "A status buffer that is not on screen is left alone."
  (let ((xl-git-status-refresh-delay 0.05)
        (refreshes 0)
        (buffer (generate-new-buffer " *status-hidden-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'xl-git-status)
                   (lambda () (cl-incf refreshes)))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil)))
          (with-current-buffer buffer (setq default-directory "/repo/"))
          (xl-git-status--on-event buffer '(1 changed "/repo/a.el"))
          (sit-for 0.2)
          (should (= refreshes 0)))
      (kill-buffer buffer))))

;;; ============================================================
;;; xl-org-babel-async: `:async yes' shell blocks
;;; ============================================================

(defun xl-test-org-buffer (body &optional header)
  "Return a temporary org buffer with one bash block running BODY.
HEADER is added to the block's header line.  Point is in the block."
  (require 'org)
  (require 'ob-shell)
  (let ((org-confirm-babel-evaluate nil)
        (dir (make-temp-file "xl-test-org-" t)))
    (let ((buffer (find-file-noselect (expand-file-name "t.org" dir))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (format "* Test\n\n#+begin_src bash %s\n%s\n#+end_src\n\nafter\n"
                        (or header ":async yes") body))
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^#\\+begin_src")
        (forward-line 1))
      buffer)))

(defun xl-test-org-wait (buffer)
  "Wait until BUFFER has no running block."
  (let ((end (+ (float-time) 15)))
    (while (and (buffer-local-value 'xl-org-babel--runs buffer)
                (< (float-time) end))
      (sit-for 0.02)))
  (should-not (buffer-local-value 'xl-org-babel--runs buffer)))

(defun xl-test-org-text (buffer)
  "Return the text of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest xl-org-babel-async-spinner-overlay-while-running ()
  "A running block shows a spinner on its #+RESULTS: line, not in the text."
  (skip-unless (executable-find "bash"))
  (let ((buffer (xl-test-org-buffer "sleep 0.6; echo late")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          ;; Starting the block wrote the placeholder; only the ticks are tested.
          (set-buffer-modified-p nil)
          (let* ((run (car xl-org-babel--runs))
                 (overlay (xl-org-babel-run-overlay run))
                 (first (overlay-get overlay 'display)))
            (should (overlayp overlay))
            (should (xl-spinner-running-p))
            (should (string-match-p "running… (C-c C-k cancels)" first))
            ;; Frames move, and the buffer text does not change for them.
            (xl-spinner-tick (current-buffer))
            (should-not (equal first (overlay-get overlay 'display)))
            (should-not (buffer-modified-p))
            (should (string-match-p "^#\\+RESULTS:$" (xl-test-org-text buffer)))
            (xl-test-org-wait buffer)
            ;; Done: the overlay is gone and so is the spinner.
            (should-not (overlay-buffer overlay))
            (should-not (xl-spinner-running-p))
            (should (string-match-p "^: late$" (xl-test-org-text buffer)))))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-mode-line-counts-running-blocks ()
  "The mode line says how many blocks run in the background, then clears."
  (skip-unless (executable-find "bash"))
  (require 'org)
  (require 'ob-shell)
  (let* ((org-confirm-babel-evaluate nil)
         (dir (make-temp-file "xl-test-org-" t))
         (buffer (find-file-noselect (expand-file-name "two.org" dir))))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "#+begin_src bash :async yes\nsleep 0.6; echo one\n#+end_src\n\n"
                  "#+begin_src bash :async yes\nsleep 0.6; echo two\n#+end_src\n")
          (org-mode)
          (goto-char (point-min))
          (forward-line 1)
          (org-babel-execute-src-block)
          ;; The slot is at the left edge of this buffer's mode line.
          (should (equal '(:eval xl-spinner-mode-line) (car mode-line-format)))
          (should (string-match-p "1 block running" xl-spinner-mode-line))
          (goto-char (point-min))
          (re-search-forward "echo two")
          (org-babel-execute-src-block)
          (should (string-match-p "2 blocks running" xl-spinner-mode-line))
          ;; The overlays still animate: one clock draws both.
          (let ((overlay (xl-org-babel-run-overlay (car xl-org-babel--runs))))
            (let ((before (overlay-get overlay 'display)))
              (xl-spinner-tick (current-buffer))
              (should-not (equal before (overlay-get overlay 'display)))))
          (xl-test-org-wait buffer)
          (should-not xl-spinner-mode-line)
          (should-not (xl-spinner-running-p))
          (should (string-match-p "^: one$" (xl-test-org-text buffer)))
          (should (string-match-p "^: two$" (xl-test-org-text buffer))))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-returns-at-once-and-fills-results ()
  "An `:async yes' block returns before it ends; its results come after."
  (skip-unless (executable-find "bash"))
  (let ((buffer (xl-test-org-buffer "echo first; sleep 0.5; echo second")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((t0 (float-time)))
            (org-babel-execute-src-block)
            (should (< (- (float-time) t0) 0.4))
            (should (string-match-p "running…" (xl-test-org-text buffer))))
          (xl-test-org-wait buffer)
          (should (string-match-p "^: first$" (xl-test-org-text buffer)))
          (should (string-match-p "^: second$" (xl-test-org-text buffer)))
          (should-not (string-match-p "running…" (xl-test-org-text buffer))))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-keeps-the-first-results-lines ()
  "Long output: the live tail is replaced by the first lines and a note."
  (skip-unless (executable-find "bash"))
  (let ((xl-org-babel-result-lines 20)
        (buffer (xl-test-org-buffer "seq 1 300")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (xl-test-org-wait buffer)
          (let ((text (xl-test-org-text buffer)))
            (should (string-match-p "^: 20$" text))
            (should-not (string-match-p "^: 21$" text))
            (should (string-match-p "280 more lines not written" text)))
          ;; The whole output is still in its buffer.
          (with-current-buffer (get-buffer (format "*Org Babel Output: %s:%d*"
                                                   (buffer-name) 3))
            (should (= 300 (count-lines (point-min) (point-max))))))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-reports-exit-code-and-timeout ()
  "A failing block says its exit code; a timed-out one says it was stopped."
  (skip-unless (executable-find "bash"))
  (let ((buffer (xl-test-org-buffer "echo x; exit 3")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (xl-test-org-wait buffer)
          (should (string-match-p "exit code 3" (xl-test-org-text buffer))))
      (kill-buffer buffer)))
  (let ((buffer (xl-test-org-buffer "sleep 5" ":async yes :timeout 0.3")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (xl-test-org-wait buffer)
          (should (string-match-p "timed out, stopped" (xl-test-org-text buffer))))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-cancel-keeps-the-output-so-far ()
  "C-c C-k stops the block and writes that it was cancelled."
  (skip-unless (executable-find "bash"))
  (let ((buffer (xl-test-org-buffer "echo before; sleep 5; echo never")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (sit-for 0.3)
          (xl-org-babel-cancel-at-point)
          (should-not xl-org-babel--runs)
          (let ((text (xl-test-org-text buffer)))
            (should (string-match-p "^: before$" text))
            (should (string-match-p "cancelled" text))
            ;; The body mentions `never'; the output must not have it.
            (should-not (string-match-p "^: never$" text))))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-running-again-replaces-the-run ()
  "Running a block that is still going cancels that run."
  (skip-unless (executable-find "bash"))
  (let ((buffer (xl-test-org-buffer "sleep 5; echo OLDRUN")))
    (unwind-protect
        (with-current-buffer buffer
          (org-babel-execute-src-block)
          (should (= 1 (length xl-org-babel--runs)))
          (goto-char (point-min))
          (re-search-forward "^#\\+begin_src")
          (forward-line 1)
          (org-babel-execute-src-block)
          ;; The second run owns the results now; one run is listed.
          (should (= 1 (length xl-org-babel--runs)))
          (xl-org-babel-cancel-at-point)
          ;; The first run was killed: its output never reaches the file.
          (should-not (string-match-p "^: OLDRUN$" (xl-test-org-text buffer))))
      (kill-buffer buffer))))

(ert-deftest xl-org-babel-async-sessions-and-plain-blocks-stay-synchronous ()
  "Without `:async yes' (and no default), or with a `:session', a block runs as org-babel does."
  (skip-unless (executable-find "bash"))
  (let ((org-babel-default-header-args:bash nil)
        (org-babel-default-header-args:sh nil))
    (dolist (header '(":results output" ":async yes :session s"))
      (let ((buffer (xl-test-org-buffer "echo done" header)))
        (unwind-protect
            (with-current-buffer buffer
              (org-babel-execute-src-block)
              (should-not xl-org-babel--runs)
              (should-not (string-match-p "running…" (xl-test-org-text buffer)))
              (should (string-match-p "done" (xl-test-org-text buffer))))
          (kill-buffer buffer))))))

(ert-deftest xl-org-babel-async-falls-back-silently-for-other-results ()
  "With the default on, blocks that are not output, or have a session, run as before."
  (skip-unless (executable-find "bash"))
  (let ((org-babel-default-header-args:bash '((:async . "yes")))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (dolist (header '(":results value" ":results silent" ":session s" ":results output file"))
        (let ((buffer (xl-test-org-buffer "echo done" header)))
          (unwind-protect
              (with-current-buffer buffer
                (org-babel-execute-src-block)
                (should-not xl-org-babel--runs))
            (kill-buffer buffer)))))
    ;; org prints the results of the blocks that ran as before; we print nothing.
    (should-not (cl-some (lambda (m) (string-match-p "async\\|session" m)) messages))))

(ert-deftest xl-org-babel-async-default-header-makes-blocks-async ()
  "With `:async yes' as the default header, a plain bash block runs in the background."
  (skip-unless (executable-find "bash"))
  (let ((org-babel-default-header-args:bash '((:async . "yes"))))
    (let ((buffer (xl-test-org-buffer "sleep 0.3; echo later" ":results output")))
      (unwind-protect
          (with-current-buffer buffer
            (org-babel-execute-src-block)
            (should (= 1 (length xl-org-babel--runs)))
            (xl-test-org-wait buffer)
            (should (string-match-p "later" (xl-test-org-text buffer))))
        (kill-buffer buffer)))))

(ert-deftest xl-consult-spinner-keeps-consults-arity ()
  "The real `consult--async-indicator' still takes no arguments and returns
a pipeline element, which takes the sink.  Calling it both ways must work."
  (require 'consult)
  (require 'xl-consult-spinner)
  (with-temp-buffer
    (insert "Search: ")
    (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 8)))
      (let ((handler (funcall (funcall #'consult--async-indicator) #'ignore)))
        (should (functionp handler))
        (funcall handler 'setup)
        (funcall handler 'destroy)))))

(ert-deftest xl-consult-spinner-animates-the-running-indicator ()
  "While consult's async list runs, its indicator animates at the pace of
the other loaders, and a vertico update redraws the frame.  When the list ends, consult's
own final character is drawn."
  (require 'consult)
  (require 'xl-consult-spinner)
  (with-temp-buffer
    (insert "Search: ")
    (let* ((xl-spinner-show-delay 0)
           (xl-spinner-min-duration 0)
           (overlay (make-overlay 6 7))
           ;; Stands in for consult's indicator: `*' while running, `:' when done.
           (consult-side
            (lambda (_sink)
              (lambda (action)
                (pcase action
                  ('setup (overlay-put overlay 'category
                                       'consult-async-indicator-overlay))
                  (`[indicator running] (overlay-put overlay 'display "*"))
                  (`[indicator finished] (overlay-put overlay 'display ":"))))))
           ;; consult's constructor takes no arguments, like this stand-in.
           (handler (funcall (xl-consult-spinner--around-indicator
                              (lambda () consult-side))
                             #'ignore)))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 8)))
        (funcall handler 'setup)
        ;; Idle from the start: the reserved cells are never empty.
        (should (equal xl-consult-spinner-idle (overlay-get overlay 'display)))
        (funcall handler [indicator running])
        (should (xl-spinner-running-p))
        (should (= xl-spinner-one-dot-interval
                   (plist-get xl-spinner--state :interval)))
        (should (member (substring-no-properties (overlay-get overlay 'display))
                        xl-spinner-one-dot-frames))
        ;; Every frame and the idle text take the same one cell.
        (dolist (frame xl-spinner-one-dot-frames)
          (should (= 1 (string-width frame))))
        (should (= 1 (string-width xl-consult-spinner-idle)))
        ;; Same colour as the mode-line loaders: the frame keeps its face.
        (should (eq 'xl-spinner
                    (get-text-property 0 'face (overlay-get overlay 'display))))
        (let ((first (overlay-get overlay 'display)))
          (xl-spinner-tick (current-buffer))
          (should-not (equal first (overlay-get overlay 'display))))
        ;; A vertico update redraws the current frame at once.
        (let ((frame (xl-spinner-current-frame)))
          (xl-consult-spinner--after-vertico-update)
          (should (equal frame (overlay-get overlay 'display))))
        (funcall handler [indicator finished])
        (should-not (xl-spinner-running-p))
        ;; The spinner stopped, and the idle text is back, at the same width.
        (should (equal xl-consult-spinner-idle (overlay-get overlay 'display)))
        (should (= 1 (string-width (overlay-get overlay 'display))))
        (funcall handler 'destroy)))))

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
  (dolist (file '("tuned-emacs/personal.el" "tuned-emacs/lisp/xl-utils.el"
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
  "A package function is byte code, unless eln-opt built its file.  Emacs's own
JIT is off, so a native package function must come from the eln for consult's
current source.  Names are compared, not folders: `ert-run-tests-batch-and-exit'
points the first eln folder at a temporary cache."
  (require 'consult)
  (require 'eln-opt nil t)
  (let* ((fn (symbol-function 'consult-line))
         (native (and (fboundp 'native-comp-function-p) (native-comp-function-p fn)))
         (built (and native (fboundp 'eln-opt--eln-for)
                     (equal (file-name-nondirectory
                             (native-comp-unit-file (subr-native-comp-unit fn)))
                            (file-name-nondirectory
                             (eln-opt--eln-for
                              (eln-opt--source-for (locate-library "consult"))))))))
    (should (if native built (byte-code-function-p fn)))))

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
  "F2-F12 keep the vim/Neovim habits from `vimrc'."
  (dolist (spec '(("<f2>" . consult-imenu)
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
                     (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
            (xl-git-status)
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
                            'xl-git-status-heading)))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "modified" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'xl-git-status-status))
                (should (search-forward "file-1.txt" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'xl-git-status-file)))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "Recent commits" nil t))
                (should (re-search-forward
                         "^\\([0-9a-f]+\\) origin/master initial$" nil t))
                (let ((hash-start (match-beginning 1)))
                  (should (eq (get-text-property hash-start 'face)
                              'xl-git-status-hash))
                  (goto-char hash-start)
                  (should (search-forward "origin/master" nil t))
                  (should (eq (get-text-property (1- (point)) 'face)
                              'xl-git-status-ref))))
              ;; The newest commit appears again below the log heading.
              (should (= 2 (how-many "initial" (point-min) (point-max))))
              (should (= (point) (point-min)))
              (let ((window (get-buffer-window buffer t)))
                (when window
                  (should (= (window-point window) (point-min))))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-loading-spinner ()
  "The first load shows the loader in the mode line and in the middle.
The buffer stays empty: the middle loader is an overlay's before-string.
Both go when the status is rendered."
  (skip-unless (executable-find "git"))
  (let* ((xl-spinner-show-delay 0)
         (repo (xl-test--git-repo))
         (name (format "*git-status: %s*"
                       (file-name-nondirectory (directory-file-name repo))))
         (buffer nil))
    (unwind-protect
        (let ((default-directory (file-name-as-directory repo)))
          (delete-other-windows)
          (xl-git-status)
          (setq buffer (get-buffer name))
          (with-current-buffer buffer
            (should xl-git-status--blank)
            (should (= 0 (buffer-size)))
            (should (string-match-p "git status" xl-spinner-mode-line))
            ;; The window exists now; the next frame draws in its middle.
            (xl-spinner-tick buffer)
            (should (overlayp xl-git-status--center))
            (let ((first (overlay-get xl-git-status--center 'before-string)))
              (should (string-match-p "git status…" first))
              (xl-spinner-tick buffer)
              (should-not (equal first (overlay-get xl-git-status--center
                                                    'before-string))))
            (should (= 0 (buffer-size))))
          (xl-test--wait-for-git-status buffer)
          (with-current-buffer buffer
            (should-not xl-git-status--blank)
            (should-not xl-git-status--center)
            (should (string-match-p "^Head:" (buffer-string))))
          (xl-test--wait-for-slot buffer))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-refreshes-without-blanking ()
  "A refresh keeps the old status on screen until the new one is ready."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (let ((before (buffer-string)))
                ;; The load is stubbed, so only the buffer handling runs.
                (cl-letf (((symbol-function 'xl-git-status--load)
                           (lambda (&rest _) nil)))
                  (xl-git-status))
                (should (equal before (buffer-string)))
                (should-not (string-match-p "Loading\\.\\.\\." (buffer-string)))
                (should buffer-read-only)
                (should (derived-mode-p 'xl-git-status-mode))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-says-refreshing-after-a-job ()
  "The queue's loader keeps running under `Refreshing' while reloading."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (let ((before (buffer-string)))
                (cl-letf (((symbol-function 'xl-git-status--load)
                           (lambda (&rest _) nil)))
                  (xl-git-status-stage)
                  ;; Pretend the operation outlived the delay.
                  (xl-test--slot-show-now buffer)
                  (let ((end (+ (float-time) 10)))
                    (while (and (xl-async-queue-busy-p 'queue)
                                (< (float-time) end))
                      (accept-process-output nil 0.05)))
                  ;; The same loader carries on under a new label.
                  (should (xl-spinner-running-p))
                  (should (string-match-p "Refreshing" xl-spinner-mode-line))
                  ;; Nothing was blanked: the old status is still there.
                  (should (equal before (buffer-string)))))
              (let ((xl-spinner-min-duration 0))
                (xl-spinner-mode-line-stop)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))


(ert-deftest xl-git-status-keys ()
  "Folding, operations and RET are bound in the status mode and Evil."
  (dolist (spec '(("TAB" . xl-git-status-toggle)
                  ("<backtab>" . xl-git-status-toggle-files)
                  ("s" . xl-git-status-stage)
                  ("u" . xl-git-status-unstage)
                  ("X" . xl-git-status-discard)
                  ("S" . xl-git-status-stage-all)
                  ("U" . xl-git-status-unstage-all)
                  ("f" . xl-git-status-fetch)
                  ("F" . xl-git-status-pull)
                  ("RET" . xl-git-status-visit)))
    (should (eq (keymap-lookup xl-git-status-mode-map (car spec))
                (cdr spec))))
  (dolist (key '("P" "p"))
    (should (eq (keymap-lookup xl-git-status-mode-map key)
                #'xl-git-status-push)))
  ;; The menu keymap carries the refs it was built with as which-key labels.
  (require 'which-key)
  (let ((map (xl-git-status--push-menu "origin/main" "origin/main")))
    (dolist (spec '(("p" . xl-git-status-push-to-pushremote)
                    ("u" . xl-git-status-push-to-upstream)
                    ("e" . xl-git-status-push-elsewhere)))
      (should (eq (keymap-lookup map (car spec)) (cdr spec))))
    (dolist (spec '(("p" . "push to origin/main")
                    ("u" . "push to origin/main")
                    ("e" . "push elsewhere")))
      (should (equal (cdr (assoc (car spec)
                                 (which-key--get-keymap-bindings map)))
                     (cdr spec)))))
  (require 'evil)
  (let ((normal (evil-get-auxiliary-keymap
                 xl-git-status-mode-map 'normal t)))
    (dolist (spec '(("TAB" . xl-git-status-toggle)
                    ("<backtab>" . xl-git-status-toggle-files)
                    ("s" . xl-git-status-stage)
                    ("u" . xl-git-status-unstage)
                    ("X" . xl-git-status-discard)
                    ("x" . xl-git-status-discard)
                    ("S" . xl-git-status-stage-all)
                    ("U" . xl-git-status-unstage-all)
                    ("RET" . xl-git-status-visit)))
      (should (eq (keymap-lookup normal (car spec)) (cdr spec))))
    (dolist (key '("P" "p"))
      (should (eq (keymap-lookup normal key) #'xl-git-status-push)))))

(ert-deftest xl-git-status-stages-and-unstages ()
  "`s' stages the file at point and `u' puts it back in the index."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              ;; file-0.txt is unstaged: stage it.
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xl-git-status-stage)
              (xl-test--wait-for-git-status buffer)
              (should (equal "file-0.txt"
                             (xl-test--git-output repo "diff" "--cached"
                                                  "--name-only")))
              ;; It is under Staged changes now: unstage it again.
              (goto-char (point-min))
              (should (search-forward "Staged changes" nil t))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xl-git-status-unstage)
              (xl-test--wait-for-git-status buffer)
              (should (equal "" (xl-test--git-output repo "diff" "--cached"
                                                      "--name-only"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-keeps-point-on-a-staged-file ()
  "`s' leaves point on the file, which now sits under Staged changes."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xl-git-status-stage)
              (xl-test--wait-for-git-status buffer)
              ;; The refresh moved the file, not point: same path, now staged.
              (should (looking-at "modified +file-0\\.txt"))
              (should (eq (plist-get (xl-git-status--section-at-point) :status)
                          'staged))
              ;; Unstaging brings point back with the file.
              (xl-git-status-unstage)
              (xl-test--wait-for-git-status buffer)
              (should (looking-at "modified +file-0\\.txt"))
              (should (eq (plist-get (xl-git-status--section-at-point) :status)
                          'unstaged)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-keeps-point-on-a-staged-hunk ()
  "`s' on a hunk follows the hunk into Staged changes.
The staged file diff is unfolded so the restored line is visible."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "file.txt" nil t))
              (beginning-of-line)
              ;; File diffs start folded, so unfold this one to reach a hunk.
              (xl-git-status-toggle)
              (should (search-forward "+changed 1" nil t))
              (beginning-of-line)
              (xl-git-status-stage)
              (xl-test--wait-for-git-status buffer)
              ;; Same line of the same hunk, now under Staged changes.
              (should (looking-at "\\+changed 1"))
              (should (eq (plist-get (xl-git-status--section-at-point) :status)
                          'staged))
              (should-not (invisible-p (point))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))


;; A background push that ends while its status window is closed must not
;; open the window again; one that ends while the window is on screen refreshes it.
(ert-deftest xl-git-status-drain-does-not-reopen-a-closed-status ()
  "The queue draining refreshes a status only when it is on screen."
  (let ((buffer (generate-new-buffer " *git-status-drain-test*"))
        (refreshed 0)
        (popped 0))
    (unwind-protect
        (cl-letf (((symbol-function 'xl-git-status)
                   (lambda (&rest _) (cl-incf refreshed)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (&rest _) (cl-incf popped)))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (cl-incf popped))))
          (with-current-buffer buffer
            (xl-spinner-mode-line-start "Pushing"))
          ;; Closed: no refresh, no window, and the loader ends.
          (xl-git-status--queue-drained buffer)
          (should (= refreshed 0))
          (should (= popped 0))
          (with-current-buffer buffer
            (should-not xl-git-status--queue-action))
          ;; On screen: the status refreshes in place.
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (&rest _) 'window)))
            (xl-git-status--queue-drained buffer))
          (should (= refreshed 1))
          (should (= popped 0)))
      (kill-buffer buffer))))

(ert-deftest xl-git-status-queues-operations-in-order ()
  "Rapid `s' presses run one git command at a time, then refresh once.
The loader is visible while the queue runs."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xl-git-status-stage)
              ;; The command returned with git still running: no blocking,
              ;; and no loader yet, because fast operations must not flicker.
              (should (xl-async-busy-p 'queue))
              (should (equal "Staged file-0.txt"
                             xl-git-status--queue-action))
              (should-not (xl-spinner-running-p))
              (should-not xl-spinner-mode-line)
              ;; Once it runs past the delay, the mode line says what runs.
              (xl-test--slot-show-now buffer)
              (should (xl-spinner-running-p))
              (should (equal (concat " " (xl-spinner-current-frame)
                                     " Staged file-0.txt ")
                             xl-spinner-mode-line))
              (should (equal '(:eval xl-spinner-mode-line)
                             (car mode-line-format)))
              (goto-char (point-min))
              (should (search-forward "modified   file-1.txt" nil t))
              (beginning-of-line)
              (xl-git-status-stage))
            (xl-test--wait-for-git-status buffer)
            (should (equal '("file-0.txt" "file-1.txt")
                           (sort (split-string
                                  (xl-test--git-output repo "diff" "--cached"
                                                       "--name-only")
                                  "\n" t)
                                 #'string<)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-pushes ()
  "The push menu pushes to the push remote and to the upstream."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo))
         (remote (make-temp-file "xl-test-remote-" t))
         (branch (xl-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD"))
         (buffer nil))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "-C" remote
                                       "init" "--bare" "-q")))
          (should (zerop (call-process "git" nil nil nil "-C" repo
                                       "remote" "add" "origin" remote)))
          (should (zerop (call-process "git" nil nil nil "-C" repo
                                       "push" "-q" "-u" "origin" branch)))
          (let ((default-directory (file-name-as-directory repo)))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
              (xl-git-status)
              (xl-test--wait-for-git-status buffer)
              ;; The render remembers what the push menu offers.
              (with-current-buffer buffer
                (should (equal branch xl-git-status--head-branch))
                (should (equal (format "origin/%s" branch)
                               xl-git-status--head-upstream)))
              ;; A new commit is only on the local side until the push.
              (with-temp-file (expand-file-name "file-0.txt" repo)
                (insert "changed again\n"))
              (should (zerop (call-process "git" nil nil nil "-C" repo
                                           "-c" "user.email=test@example.com"
                                           "-c" "user.name=Test"
                                           "commit" "-qam" "second")))
              (with-current-buffer buffer
                (require 'which-key)
                ;; `execute-kbd-macro' runs commands in the selected window's
                ;; buffer, not in `current-buffer', so make that the status.
                (set-window-buffer (selected-window) buffer)
                (let (title labels)
                  (cl-letf (((symbol-function 'which-key--show-keymap)
                             (lambda (name map &rest _)
                               (setq title name)
                               (setq labels
                                     (which-key--get-keymap-bindings map)))))
                    ;; `p' pops the menu at once, without which-key's delay.
                    (xl-git-status-push))
                  (should (equal (format "Push %s to" branch) title))
                  (should (equal (format "push to origin/%s" branch)
                                 (cdr (assoc "p" labels))))
                  (should (equal (format "push to origin/%s" branch)
                                 (cdr (assoc "u" labels))))
                  ;; The transient map reads the very next key, here `p'.
                  (execute-kbd-macro (kbd "p"))
                  ;; The push runs in the queue; the mode line shows it only
                  ;; when the operation outlives the delay.
                  (should (equal (format "Pushing %s to origin" branch)
                                 xl-git-status--queue-action))
                  (should-not (xl-spinner-running-p))
                  (xl-test--slot-show-now buffer)
                  (should (equal (format " %s Pushing %s to origin "
                                         (xl-spinner-current-frame) branch)
                                 xl-spinner-mode-line))
                  (let ((frame (xl-spinner-current-frame)))
                    (xl-spinner-tick)
                    (should-not (equal frame (xl-spinner-current-frame))))
                  (should (equal (format " %s Pushing %s to origin "
                                         (xl-spinner-current-frame) branch)
                                 xl-spinner-mode-line)))))
            (xl-test--wait-for-git-status buffer)
            (xl-test--wait-for-slot buffer)
            (with-current-buffer buffer
              (should-not xl-spinner-mode-line))
            (should (equal (xl-test--git-output repo "rev-parse" branch)
                           (xl-test--git-output remote "rev-parse" branch)))
            ;; A third commit goes out through the upstream choice.
            (with-temp-file (expand-file-name "file-0.txt" repo)
              (insert "changed once more\n"))
            (should (zerop (call-process "git" nil nil nil "-C" repo
                                         "-c" "user.email=test@example.com"
                                         "-c" "user.name=Test"
                                         "commit" "-qam" "third")))
            (with-current-buffer buffer
              (xl-git-status-push-to-upstream)
              (should (equal (format "Pushing to origin/%s" branch)
                             xl-git-status--queue-action)))
            (xl-test--wait-for-git-status buffer)
            (should (equal (xl-test--git-output repo "rev-parse" branch)
                           (xl-test--git-output remote "rev-parse" branch)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory remote t))))

(ert-deftest xl-git-status-pushes-elsewhere ()
  "The `e' choice asks for a remote and branch, then pushes there."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo))
         (remote (make-temp-file "xl-test-remote-" t))
         (branch (xl-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD"))
         (buffer nil))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "-C" remote
                                       "init" "--bare" "-q")))
          (should (zerop (call-process "git" nil nil nil "-C" repo
                                       "remote" "add" "origin" remote)))
          (should (zerop (call-process "git" nil nil nil "-C" repo
                                       "push" "-q" "-u" "origin" branch)))
          (let ((default-directory (file-name-as-directory repo)))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
              (xl-git-status)
              (xl-test--wait-for-git-status buffer)
              (with-temp-file (expand-file-name "file-0.txt" repo)
                (insert "changed again\n"))
              (should (zerop (call-process "git" nil nil nil "-C" repo
                                           "-c" "user.email=test@example.com"
                                           "-c" "user.name=Test"
                                           "commit" "-qam" "second")))
              (with-current-buffer buffer
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (&rest _) "origin"))
                          ((symbol-function 'read-string)
                           (lambda (&rest _) "elsewhere")))
                  (xl-git-status-push-elsewhere))
                (should (equal "Pushing to origin/elsewhere"
                               xl-git-status--queue-action))))
            (xl-test--wait-for-git-status buffer)
            (should (equal (xl-test--git-output repo "rev-parse" branch)
                           (xl-test--git-output remote "rev-parse"
                                                "elsewhere")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory remote t))))

(ert-deftest xl-git-status-visits-files-and-commits ()
  "`RET' opens the file at point and shows the commit at point."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil)
        (opened nil)
        (commit nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b))
                  ((symbol-function 'find-file)
                   (lambda (file) (setq opened file)))
                  ((symbol-function 'xl-git-status--show-commit)
                   (lambda (hash) (setq commit hash))))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (xl-git-status-visit)
              (should (equal opened (expand-file-name "file-0.txt" repo)))
              (goto-char (point-min))
              (should (search-forward "Recent commits" nil t))
              (should (re-search-forward "^[0-9a-f]+ " nil t))
              (xl-git-status-visit)
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
                     (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
            (xl-git-status))
          (xl-test--wait-for-git-status buffer)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "modified   file.txt")
            (beginning-of-line)
            (xl-git-status-toggle))
          (let ((visit-line
                 (lambda (regexp nth)
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (dotimes (_ nth) (re-search-forward regexp))
                     (beginning-of-line)
                     (xl-git-status-visit))
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


(ert-deftest xl-git-show-uses-the-status-faces ()
  "The commit buffer reuses the status buffer's faces for headers and diffs."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (progn
          ;; Commit the two working-tree changes so `git show' has a diff.
          (should (zerop (call-process "git" nil nil nil
                                       "-C" repo
                                       "-c" "user.email=test@example.com"
                                       "-c" "user.name=Test"
                                       "commit" "-qam" "change file")))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status--show-commit "HEAD")
            (setq buffer (get-buffer "*git-show: HEAD*"))
            (should (buffer-live-p buffer))
            (xl-test--wait-for-git-show buffer)
            (with-current-buffer buffer
              (should (derived-mode-p 'special-mode))
              (should (derived-mode-p 'xl-git-show-mode))
              (should buffer-read-only)
              (let ((face-at
                     (lambda (regexp)
                       (goto-char (point-min))
                       (should (re-search-forward regexp nil t))
                       (get-text-property (match-beginning 0) 'face))))
                ;; Commit header.
                (should (eq (funcall face-at "^commit ") 'xl-git-status-heading))
                (goto-char (point-min))
                (should (re-search-forward "^commit \\([0-9a-f]+\\)" nil t))
                (should (eq (get-text-property (match-beginning 1) 'face)
                            'xl-git-status-hash))
                (should (eq (funcall face-at "^Author: ")
                            'xl-git-status-heading))
                ;; Diff header, hunk and body: the same faces as the status.
                (should (eq (funcall face-at "^diff --git ") 'diff-header))
                (should (eq (funcall face-at "^--- ") 'diff-header))
                (should (eq (funcall face-at "^@@ ")
                            'xl-git-status-hunk-heading))
                (should (eq (funcall face-at "^ line 0") 'diff-context))
                (should (eq (funcall face-at "^-line 1") 'diff-removed))
                (should (eq (funcall face-at "^\\+changed 1") 'diff-added))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-show-opens-before-git-answers ()
  "The commit buffer appears with a loader, then the diff replaces it."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil
                                       "-C" repo
                                       "-c" "user.email=test@example.com"
                                       "-c" "user.name=Test"
                                       "commit" "-qam" "change file")))
          (let ((default-directory (file-name-as-directory repo)))
            (setq buffer (xl-git-status--show-commit "HEAD"))
            (with-current-buffer buffer
              ;; The buffer is there before git answers: loader on, no diff.
              (should (xl-spinner-running-p))
              (should (overlayp xl-git-status--show-spinner))
              (should (string-match-p "Loading\\.\\.\\." (buffer-string)))
              (should-not (string-match-p "^diff --git" (buffer-string))))
            (xl-test--wait-for-git-show buffer)
            (with-current-buffer buffer
              (should-not (xl-spinner-running-p))
              (should-not (overlayp xl-git-status--show-spinner))
              (should (string-prefix-p "commit " (buffer-string)))
              (should (string-match-p "^diff --git" (buffer-string))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-expands-an-abbreviated-root ()
  "A dired-style ~/... root is expanded before `git -C' sees it."
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (&rest _) "~/workspace/dotfiles/")))
    (let ((default-directory temporary-file-directory))
      (should (equal (xl-git-status--root)
                     (file-name-as-directory
                      (expand-file-name "~/workspace/dotfiles/")))))))

(ert-deftest xl-git-status-rejects-a-non-repo ()
  "Calling the status outside a Git repository is a clear user error."
  (xl-test-with-temp-dir dir
    (let ((default-directory (file-name-as-directory dir)))
      (should-error (xl-git-status) :type 'user-error))))

(ert-deftest xl-git-status-folds-sections ()
  "File diffs start folded, TAB toggles hunks, and refreshes keep the state."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
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
                            'xl-git-status-hunk-heading))
                (funcall goto-file)
                (xl-git-status-toggle)
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
                (xl-git-status-toggle)
                (should-not (invisible-p (funcall hunk-heading)))
                (should (invisible-p (funcall hunk-body)))
                ;; A refresh keeps the file open and the hunk folded.
                (xl-git-status)
                (xl-test--wait-for-git-status buffer)
                (should-not (invisible-p (funcall hunk-heading)))
                (should (invisible-p (funcall hunk-body)))
                ;; S-TAB folds every file diff, then unfolds it again.
                (xl-git-status-toggle-files)
                (should (invisible-p (funcall hunk-heading)))
                (xl-git-status-toggle-files)
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
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "modified   file.txt")
              (beginning-of-line)
              (xl-git-status-toggle)
              (goto-char (point-min))
              (re-search-forward "^@@")
              (beginning-of-line)
              (xl-git-status-stage))
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

(defun xl-test--remote-with-peer ()
  "Return (REPO . PEER) sharing a bare remote; REPO tracks it, PEER pushes.
PEER commits `peer.txt' and pushes it, so REPO is one commit behind."
  (let* ((remote (make-temp-file "xl-test-remote-" t))
         (repo (xl-test--git-repo))
         (peer (make-temp-file "xl-test-peer-" t))
         (branch (xl-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD")))
    (cl-flet ((git (dir &rest args)
                (should (zerop (apply #'call-process "git" nil nil nil
                                      "-C" dir
                                      "-c" "user.email=test@example.com"
                                      "-c" "user.name=Test" args)))))
      (git remote "init" "--bare" "-q")
      (git repo "remote" "add" "origin" remote)
      (git repo "push" "-q" "-u" "origin" branch)
      ;; The peer clones the remote and pushes one more commit.
      (git peer "clone" "-q" remote ".")
      (with-temp-file (expand-file-name "peer.txt" peer) (insert "peer\n"))
      (git peer "add" "peer.txt")
      (git peer "commit" "-qm" "from the peer")
      (git peer "push" "-q" "origin" branch))
    (cons repo peer)))

(ert-deftest xl-git-status-fetch-updates-the-remote-branch ()
  "`f' fetches the upstream: the remote-tracking ref moves, the branch does not."
  (skip-unless (executable-find "git"))
  (let* ((pair (xl-test--remote-with-peer))
         (repo (car pair))
         (before (xl-test--git-output repo "rev-parse" "HEAD"))
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (xl-git-status-fetch)
              (should (xl-async-queue-busy-p 'queue)))
            (xl-test--wait-for-git-status buffer)
            ;; The branch stays where it was; only origin/<branch> moved.
            (should (equal before (xl-test--git-output repo "rev-parse" "HEAD")))
            (should-not (equal before
                               (xl-test--git-output repo "rev-parse" "@{u}")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory (cdr pair) t))))

(ert-deftest xl-git-status-pull-fast-forwards-the-branch ()
  "`F' fast-forwards the branch to its upstream and refreshes the status."
  (skip-unless (executable-find "git"))
  (let* ((pair (xl-test--remote-with-peer))
         (repo (car pair))
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            ;; The peer's commit only adds peer.txt, so the local edits to
            ;; file-N.txt do not get in the way of a fast-forward.
            (with-current-buffer buffer
              (xl-git-status-pull))
            (xl-test--wait-for-git-status buffer)
            (should (equal (xl-test--git-output repo "rev-parse" "HEAD")
                           (xl-test--git-output repo "rev-parse" "@{u}")))
            (should (file-exists-p (expand-file-name "peer.txt" repo)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory (cdr pair) t))))

(ert-deftest xl-git-status-pull-refuses-without-an-upstream ()
  "`F' on a branch with no upstream says so, instead of running git."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (should-error (xl-git-status-pull) :type 'user-error)
              (should-not (xl-async-queue-busy-p 'queue)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-stages-a-path-with-a-space ()
  "A path with a space shows its diff and stages.
Porcelain without `-z' quoted it, and `git add' took the quotes literally."
  (skip-unless (executable-find "git"))
  (let ((repo (make-temp-file "xl-test-git-space-" t))
        (buffer nil))
    (unwind-protect
        (progn
          (cl-flet ((run (&rest args)
                      (apply #'call-process "git" nil nil nil "-C" repo
                             "-c" "user.email=test@example.com"
                             "-c" "user.name=Test" args)))
            (run "init" "-q")
            (with-temp-file (expand-file-name "a b.txt" repo) (insert "one\n"))
            (run "add" ".")
            (run "commit" "-qm" "initial")
            (with-temp-file (expand-file-name "a b.txt" repo) (insert "two\n")))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
            (let ((default-directory (file-name-as-directory repo)))
              (xl-git-status)
              (xl-test--wait-for-git-status buffer)
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (search-forward "modified   a b.txt" nil t))
                (beginning-of-line)
                ;; The single whole-repo diff was split back onto the file.
                (xl-git-status-toggle)
                (should (string-match-p "^\\+two$" (buffer-string)))
                (goto-char (point-min))
                (search-forward "modified   a b.txt")
                (beginning-of-line)
                (xl-git-status-stage))
              (xl-test--wait-for-git-status buffer)
              (should (equal "a b.txt"
                             (xl-test--git-output repo "diff" "--cached"
                                                  "--name-only"))))))
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
                     (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
            (let ((default-directory (file-name-as-directory repo)))
              (xl-git-status)
              (xl-test--wait-for-git-status buffer)
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Staged changes")
                (search-forward "modified   file.txt")
                (beginning-of-line)
                (xl-git-status-toggle)
                (goto-char (point-min))
                (re-search-forward "^@@")
                (beginning-of-line)
                (xl-git-status-unstage))
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
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "modified   file.txt")
              (beginning-of-line)
              (xl-git-status-toggle)
              ;; The second hunk reverts line 18 and nothing else.
              (goto-char (point-min))
              (re-search-forward "^@@")
              (re-search-forward "^@@")
              (beginning-of-line)
              (xl-git-status-discard))
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

(ert-deftest xl-git-status-deletes-untracked-with-y-or-n ()
  "Deleting an untracked file asks with `y-or-n-p', not `yes-or-no-p'."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo))
         (file (expand-file-name "untracked.txt" repo))
         (buffer nil)
         asked)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b))
                  ((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) t))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (error "git-status delete must use y-or-n-p"))))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "untracked  untracked.txt")
              (beginning-of-line)
              (xl-git-status-discard))
            (should (string-match-p "Delete" asked))
            (should-not (file-exists-p file))
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (should-not (string-match-p "untracked.txt" (buffer-string))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest xl-git-status-stages-and-unstages-a-section ()
  "`s' on a section heading stages all its files; S and U act globally."
  (skip-unless (executable-find "git"))
  (let ((repo (xl-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (xl-git-status)
            (xl-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Unstaged changes")
              (beginning-of-line)
              (xl-git-status-stage))
            (xl-test--wait-for-git-status buffer)
            (should (equal "file-0.txt\nfile-1.txt\nfile-2.txt"
                           (xl-test--git-output repo "diff" "--cached"
                                                "--name-only")))
            (with-current-buffer buffer
              (xl-git-status-stage-all))
            (xl-test--wait-for-git-status buffer)
            (should (string-match-p "untracked\\.txt"
                                    (xl-test--git-output repo "diff" "--cached"
                                                         "--name-only")))
            (with-current-buffer buffer
              (xl-git-status-unstage-all))
            (xl-test--wait-for-git-status buffer)
            (should (equal "" (xl-test--git-output repo "diff" "--cached"
                                                    "--name-only")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

;;; ============================================================
;;; GBlame
;;; ============================================================

(ert-deftest xl-gblame-shows-and-syncs ()
  "GBlame lists one blame line per source line and follows the window."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-hunks))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (should (buffer-live-p xl-gblame--buffer))
          (should (get-buffer-window xl-gblame--buffer))
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            (should (derived-mode-p 'xl-gblame-mode))
            (should buffer-read-only)
            ;; One line of blame for each of the 20 source lines.
            (should (= 20 (how-many "\n" (point-min) (point-max))))
            ;; The first commit gets the first palette face.
            (should (eq (get-text-property (point-min) 'face)
                        (car xl-gblame-hash-faces)))
            (should (string-match-p
                     "\\`[0-9a-f]\\{8\\}"
                     (buffer-substring (point-min) (+ (point-min) 8))))
            (goto-char (point-min))
            (should (search-forward "Test" (line-end-position) t))
            ;; The working-tree change on line 2 has no commit yet.
            (goto-char (point-min))
            (forward-line 1)
            (should (string-match-p
                     "\\`0+\\'" (get-text-property (point) 'xl-gblame-hash)))
            (should (eq (get-text-property (point) 'face)
                        'xl-git-status-comment))
            (should-error (xl-gblame-show-commit) :type 'user-error))
          ;; Moving point moves the highlight; scrolling moves the window.
          (goto-char (point-min))
          (forward-line 9)
          (xl-gblame--sync (get-buffer-window buf t))
          (with-current-buffer xl-gblame--buffer
            (should (= 10 (line-number-at-pos
                           (overlay-start xl-gblame--highlight)))))
          (with-current-buffer buf
            (set-window-start
             (get-buffer-window buf t)
             (save-excursion (goto-char (point-min)) (forward-line 14) (point)))
            (xl-gblame--sync (get-buffer-window buf t)))
          (with-current-buffer xl-gblame--buffer
            (let ((window (get-buffer-window xl-gblame--buffer t)))
              (should (= 15 (line-number-at-pos (window-start window))))
              ;; The blame window's point stays inside the visible lines,
              ;; so redisplay does not scroll it back to point-min.
              (should (<= 15 (line-number-at-pos (window-point window))))
              (should (< (line-number-at-pos (window-point window))
                         (+ 15 (window-body-height window))))))
          ;; RET shows the commit of a committed line.
          (let (shown)
            (cl-letf (((symbol-function 'xl-git-status--show-commit)
                       (lambda (hash) (setq shown hash))))
              (with-current-buffer xl-gblame--buffer
                (goto-char (point-min))
                (xl-gblame-show-commit))
              (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'" shown))))
          ;; :Gblame is the vim-fugitive command name.
          (should (eq (cdr (assoc "Gblame" evil-ex-commands)) 'xl-gblame))
          ;; The G* family lives in xl-git-commands.el, not in example/xueliang.el.
          (dolist (cmd '(Gstatus Gwrite Gcommit xl-gblame))
            (should (equal "xl-git-commands"
                           (file-name-sans-extension
                            (file-name-nondirectory (symbol-file cmd 'defun))))))
          (should (eq (cdr (assoc "Gstatus" evil-ex-commands)) 'Gstatus))
          (should (eq (cdr (assoc "Gwrite" evil-ex-commands)) 'Gwrite))
          (should (eq (cdr (assoc "Gcommit" evil-ex-commands)) 'Gcommit))
          ;; The blame window is an ordinary split, not an Emacs side window,
          ;; and it is not hidden from `other-window' (C-w C-w).
          (let ((window (get-buffer-window xl-gblame--buffer t)))
            (should-not (window-parameter window 'window-side))
            (should-not (window-parameter window 'no-other-window))
            (should (memq window (window-list))))
          ;; :only (delete-other-windows) closes it; SPC g b opens it again.
          (select-window (get-buffer-window buf t))
          (delete-other-windows)
          (should-not (get-buffer-window xl-gblame--buffer t))
          (xl-gblame)
          (should (get-buffer-window xl-gblame--buffer t))
          ;; Toggling closes the window and removes the source hooks.
          (xl-gblame)
          (should-not (buffer-live-p xl-gblame--buffer))
          (should-not (memq #'xl-gblame--sync post-command-hook))
          (should-not (memq #'xl-gblame--refresh after-save-hook)))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-blank-window-shows-a-centered-loader ()
  "A first GBlame shows the loader in the middle of its blank window too.
The loader is a before-string: no line is added, so the placeholders stay
aligned.  It goes as soon as the blame is in, and a refresh of a window
that shows blame has the mode-line loader only."
  (skip-unless (executable-find "git"))
  (let* ((xl-spinner-show-delay 0)
         (repo (xl-test--git-repo-blame))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (let ((blame xl-gblame--buffer))
            (with-current-buffer blame
              (should xl-gblame--blank)
              ;; The window exists now; the next frame draws in it.
              (xl-spinner-tick (current-buffer))
              (should (overlayp xl-gblame--center))
              (let ((first (overlay-get xl-gblame--center 'before-string)))
                (should (string-match-p "git blame…" first))
                (should (= (overlay-start xl-gblame--center)
                           (overlay-end xl-gblame--center)))
                (xl-spinner-tick (current-buffer))
                (should-not (equal first (overlay-get xl-gblame--center
                                                      'before-string))))
              ;; One placeholder per source line: nothing was inserted.
              (should (= (count-lines (point-min) (point-max))
                         (with-current-buffer buf
                           (count-lines (point-min) (point-max))))))
            (xl-test--wait-for-gblame blame)
            (with-current-buffer blame
              (should-not xl-gblame--blank)
              (should-not xl-gblame--center)
              ;; A refresh keeps the blame on screen: no middle loader.
              (xl-gblame-refresh)
              (xl-spinner-tick (current-buffer))
              (should-not xl-gblame--center))
            (xl-test--wait-for-gblame blame)))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-loads-asynchronously ()
  "GBlame returns at once: placeholders and a loader, then the blame."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-blame))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (with-current-buffer xl-gblame--buffer
            ;; The command returned before git did: the UI was not blocked,
            ;; and the placeholders already line up with the source file.
            (should (xl-spinner-mode-line-busy-p))
            (should (equal '(:eval xl-spinner-mode-line)
                           (car mode-line-format)))
            (should (= (count-lines (point-min) (point-max))
                       (with-current-buffer buf
                         (count-lines (point-min) (point-max)))))
            (should-not (get-text-property (point-min) 'xl-gblame-hash)))
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (xl-test--wait-for-slot xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            ;; Done: the loader is gone and the blame is in place.
            (should-not xl-spinner-mode-line)
            (should-not (xl-spinner-running-p))
            (should (get-text-property (point-min) 'xl-gblame-hash))))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-reports-failures-in-the-mode-line ()
  "A failed `git blame' keeps the error at the left of the mode line."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo))
         (buf (find-file-noselect (expand-file-name "untracked.txt" repo))))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            (should (string-match-p "git blame failed" xl-spinner-mode-line))
            (should-not (xl-spinner-running-p))
            ;; The placeholders stay, so the window remains aligned.
            (should (= (count-lines (point-min) (point-max))
                       (with-current-buffer buf
                         (count-lines (point-min) (point-max)))))))
      (let ((blame (get-buffer "*GBlame: untracked.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-colours-different-commits ()
  "Neighbouring commits get different hash faces; one commit keeps one."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-blame))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (xl-gblame)
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            (let (faces)
              (goto-char (point-min))
              (dotimes (_ 4)
                (push (get-text-property (point) 'face) faces)
                (forward-line 1))
              (setq faces (nreverse faces))
              ;; Line 1 is uncommitted, line 2 is the initial commit,
              ;; lines 3 and 4 belong to the second commit.
              (should (eq (nth 0 faces) 'xl-git-status-comment))
              (should (memq (nth 1 faces) xl-gblame-hash-faces))
              (should (memq (nth 2 faces) xl-gblame-hash-faces))
              ;; Different adjacent commits use different colours...
              (should-not (eq (nth 1 faces) (nth 2 faces)))
              ;; ...while one commit keeps one colour.
              (should (eq (nth 2 faces) (nth 3 faces))))))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
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
  (should (= xl-dired-preview-idle 0.1))
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
  "`personal.el' carries no-byte-compile, so neither compile-lisp nor eln-opt makes an .elc."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "tuned-emacs/personal.el" xl-dotfiles) nil 0 200)
    (should (string-match-p "no-byte-compile:[ \t]*t" (buffer-string)))))

;; eln-opt tier 1: our lisp runs as byte code exactly when a current .elc exists.
(ert-deftest xl-own-lisp-is-compiled-when-built ()
  "Our lisp is compiled (byte code or native) when its .elc is current, else not."
  (let ((root (expand-file-name "tuned-emacs" xl-dotfiles)))
    (dolist (case '((xl-git-status--entries "lisp/xl-git-status.el")))
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
