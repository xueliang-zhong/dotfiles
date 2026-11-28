;;; tuned-git-status-tests.el --- ERT tests for tuned-git-status -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-git-status-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Each test works in its own temporary repository and waits for the real
;; asynchronous Git processes.

(require 'ert)
(require 'cl-lib)
(require 'tuned-async)
(require 'tuned-spinner)
(require 'tuned-git-status)

(defmacro tuned-test-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory and run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "tuned-test-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun tuned-test--read-file (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))
(defun tuned-test--git-repo ()
  "Create a temporary git repo with commits, edits and an untracked file."
  (let ((dir (make-temp-file "tuned-test-git-" t))
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
(defun tuned-test--git-output (dir &rest args)
  "Run git ARGS in DIR and return its trimmed output."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil t nil "-C" dir args)))
      (should (zerop code))
      (string-trim (buffer-string)))))
(defun tuned-test--git-repo-hunks ()
  "Create a repository whose file.txt has two separate unstaged hunks."
  (let ((dir (make-temp-file "tuned-test-git-hunks-" t))
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
(defun tuned-test--wait-for-git-status (buffer)
  "Wait until the Git status BUFFER has rendered and its queue is empty.
A file-watch refresh waits `tuned-git-status-refresh-delay' after the last
event, so a pending one counts as work: otherwise the test looks before it
has moved the buffer."
  (with-current-buffer buffer
    (let ((key (tuned-git-core-queue-key))
          (end (+ (float-time) 10)))
      (while (and (or (not buffer-read-only)
                      (tuned-async-queue-busy-p key)
                      (tuned-async-busy-p nil buffer)
                      (timerp (bound-and-true-p tuned-git-status--watch-timer)))
                  (< (float-time) end))
        (accept-process-output nil 0.05))
      (should buffer-read-only)
      (should-not (tuned-async-queue-busy-p key))
      (should-not (tuned-async-busy-p nil buffer)))))
(defun tuned-test--slot-show-now (buffer)
  "Make BUFFER's waiting mode-line loader appear now.
It is what happens when the operation outlives `tuned-spinner-show-delay'."
  (with-current-buffer buffer
    (let ((timer (plist-get tuned-spinner--slot :show-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (tuned-spinner--slot-show buffer)))
(defun tuned-test--wait-for-slot (buffer)
  "Wait until BUFFER's mode-line loader has gone, minimum time included."
  (let ((end (+ (float-time) 10)))
    (while (and (tuned-spinner-mode-line-busy-p buffer)
                (< (float-time) end))
      (accept-process-output nil 0.05)))
  (should-not (tuned-spinner-mode-line-busy-p buffer)))
(defun tuned-test--wait-for-git-show (buffer)
  "Wait until the commit BUFFER has finished loading."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (tuned-async-busy-p 'show buffer)
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should-not (tuned-async-busy-p 'show buffer))))
(defun tuned-test--remote-with-peer ()
  "Return (REPO . PEER) sharing a bare remote; REPO tracks it, PEER pushes.
PEER commits `peer.txt' and pushes it, so REPO is one commit behind."
  (let* ((remote (make-temp-file "tuned-test-remote-" t))
         (repo (tuned-test--git-repo))
         (peer (make-temp-file "tuned-test-peer-" t))
         (branch (tuned-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD")))
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

(ert-deftest tuned-git-status-parses-nul-separated-status ()
  "`-z' status gives raw paths: spaces stay unquoted, renames use the new path."
  (let* ((status (concat "## main...origin/main [ahead 1]\0"
                         " M a b.txt\0"
                         "R  new name\0plain\0"
                         "?? ü.txt\0"))
         (entries (tuned-git-core-entries status)))
    (should (equal (tuned-git-core-branch status) "main"))
    (should (equal (tuned-git-core-upstream status) "origin/main"))
    (should (equal entries '((("renamed" . "new name"))
                             (("modified" . "a b.txt"))
                             (("untracked" . "ü.txt")))))))

(ert-deftest tuned-git-status-splits-a-whole-repo-diff ()
  "One `git diff' splits into the per-file diffs the sections show."
  (let* ((text (concat "diff --git a/a b.txt b/a b.txt\n"
                       "index 1..2 100644\n--- a/a b.txt\t\n+++ b/a b.txt\t\n"
                       "@@ -1 +1,2 @@\n a\n+b\n"
                       "diff --git a/plain b/new name\n"
                       "similarity index 100%\nrename from plain\nrename to new name\n"
                       "diff --git a/img.png b/img.png\n"
                       "Binary files a/img.png and b/img.png differ\n"))
         (files (tuned-git-show-split-files text)))
    (should (equal (mapcar #'car files) '("a b.txt" "new name" "img.png")))
    (should (string-prefix-p "diff --git a/a b.txt" (cdr (nth 0 files))))
    (should (string-suffix-p "+b\n" (cdr (nth 0 files))))
    (should (string-match-p "rename to new name\n\\'" (cdr (nth 1 files))))))

(ert-deftest tuned-git-status-reads-do-not-rewrite-the-index ()
  "Every status command carries --no-optional-locks, so a refresh does not
rewrite .git/index and retrigger the auto refresh."
  (should (member "--no-optional-locks"
                  (tuned-git-core-git-command '("status" "--porcelain=v1"))))
  (should (equal "--no-optional-locks"
                 (nth 2 (tuned-git-core-git-command '("status"))))))

(ert-deftest tuned-git-status-watch-ignores-noise ()
  "Lock files, editor backups and most of .git do not refresh the status."
  (let ((root "/repo/"))
    (should (tuned-git-status--relevant-event-p root "/repo/src/a.el"))
    (should (tuned-git-status--relevant-event-p root "/repo/.git/index"))
    (should (tuned-git-status--relevant-event-p root "/repo/.git/HEAD"))
    (should (tuned-git-status--relevant-event-p root "/repo/.git/refs/heads/main"))
    (should-not (tuned-git-status--relevant-event-p root "/repo/.git/index.lock"))
    (should-not (tuned-git-status--relevant-event-p root "/repo/.git/objects/ab/cd"))
    (should-not (tuned-git-status--relevant-event-p root "/repo/.git/logs/HEAD"))
    (should-not (tuned-git-status--relevant-event-p root "/repo/a.el~"))
    (should-not (tuned-git-status--relevant-event-p root "/repo/.#a.el"))
    (should-not (tuned-git-status--relevant-event-p root "/repo/#a.el#"))))

(ert-deftest tuned-git-status-watch-dirs-include-parents ()
  "The directories of the listed files, each with its parents, root first."
  (let ((dirs (tuned-git-status--watch-dirs "/repo" '("a.el" "src/b.el" "src/x/c.el"))))
    (should (equal (car dirs) "/repo"))
    (should (member "/repo/src" dirs))
    (should (member "/repo/src/x" dirs))
    (should (= (length dirs) (length (delete-dups (copy-sequence dirs)))))))

(ert-deftest tuned-git-status-debounces-a-burst-into-one-refresh ()
  "Several changes in a row refresh the visible status once, after the delay."
  (let ((tuned-git-status-refresh-delay 0.1)
        (refreshes 0)
        (buffer (generate-new-buffer " *status-watch-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'tuned-git-status)
                   (lambda () (cl-incf refreshes)))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'window)))
          (with-current-buffer buffer
            (setq default-directory "/repo/"))
          (dotimes (_ 3)
            (tuned-git-status--on-event buffer '(1 changed "/repo/a.el")))
          (should (= refreshes 0))
          (sit-for 0.3)
          (should (= refreshes 1)))
      (kill-buffer buffer))))

(ert-deftest tuned-git-status-hidden-buffer-is-not-refreshed ()
  "A status buffer that is not on screen is left alone."
  (let ((tuned-git-status-refresh-delay 0.05)
        (refreshes 0)
        (buffer (generate-new-buffer " *status-hidden-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'tuned-git-status)
                   (lambda () (cl-incf refreshes)))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil)))
          (with-current-buffer buffer (setq default-directory "/repo/"))
          (tuned-git-status--on-event buffer '(1 changed "/repo/a.el"))
          (sit-for 0.2)
          (should (= refreshes 0)))
      (kill-buffer buffer))))

(ert-deftest tuned-git-status-lists-files-and-log ()
  "The status buffer shows the head, changed files and recent commits."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
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
            (tuned-git-status)
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
                            'tuned-git-status-heading)))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "modified" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'tuned-git-status-status))
                (should (search-forward "file-1.txt" nil t))
                (should (eq (get-text-property (1- (point)) 'face)
                            'tuned-git-status-file)))
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "Recent commits" nil t))
                (should (re-search-forward
                         "^\\([0-9a-f]+\\) origin/master initial$" nil t))
                (let ((hash-start (match-beginning 1)))
                  (should (eq (get-text-property hash-start 'face)
                              'tuned-git-status-hash))
                  (goto-char hash-start)
                  (should (search-forward "origin/master" nil t))
                  (should (eq (get-text-property (1- (point)) 'face)
                              'tuned-git-status-ref))))
              ;; The newest commit appears again below the log heading.
              (should (= 2 (how-many "initial" (point-min) (point-max))))
              (should (= (point) (point-min)))
              (let ((window (get-buffer-window buffer t)))
                (when window
                  (should (= (window-point window) (point-min))))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-loading-spinner ()
  "The first load shows the loader in the mode line and in the middle.
The buffer stays empty: the middle loader is an overlay's before-string.
Both go when the status is rendered."
  (skip-unless (executable-find "git"))
  (let* ((tuned-spinner-show-delay 0)
         (repo (tuned-test--git-repo))
         (name (format "*git-status: %s*"
                       (file-name-nondirectory (directory-file-name repo))))
         (buffer nil))
    (unwind-protect
        (let ((default-directory (file-name-as-directory repo)))
          (delete-other-windows)
          (tuned-git-status)
          (setq buffer (get-buffer name))
          (with-current-buffer buffer
            (should tuned-git-status--blank)
            (should (= 0 (buffer-size)))
            (should (string-match-p "git status" tuned-spinner-mode-line))
            ;; The window exists now; the next frame draws in its middle.
            (tuned-spinner-tick buffer)
            (should (overlayp tuned-git-status--center))
            (let ((first (overlay-get tuned-git-status--center 'before-string)))
              (should (string-match-p "git status…" first))
              (tuned-spinner-tick buffer)
              (should-not (equal first (overlay-get tuned-git-status--center
                                                    'before-string))))
            (should (= 0 (buffer-size))))
          (tuned-test--wait-for-git-status buffer)
          (with-current-buffer buffer
            (should-not tuned-git-status--blank)
            (should-not tuned-git-status--center)
            (should (string-match-p "^Head:" (buffer-string))))
          (tuned-test--wait-for-slot buffer))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-refreshes-without-blanking ()
  "A refresh keeps the old status on screen until the new one is ready."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (let ((before (buffer-string)))
                ;; The load is stubbed, so only the buffer handling runs.
                (cl-letf (((symbol-function 'tuned-git-status--load)
                           (lambda (&rest _) nil)))
                  (tuned-git-status))
                (should (equal before (buffer-string)))
                (should-not (string-match-p "Loading\\.\\.\\." (buffer-string)))
                (should buffer-read-only)
                (should (derived-mode-p 'tuned-git-status-mode))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-says-refreshing-after-a-job ()
  "The queue's loader keeps running under `Refreshing' while reloading."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (let ((before (buffer-string)))
                (cl-letf (((symbol-function 'tuned-git-status--load)
                           (lambda (&rest _) nil)))
                  (tuned-git-status-stage)
                  ;; Pretend the operation outlived the delay.
                  (tuned-test--slot-show-now buffer)
                  (let ((end (+ (float-time) 10)))
                    (while (and (tuned-async-queue-busy-p (tuned-git-core-queue-key))
                                (< (float-time) end))
                      (accept-process-output nil 0.05)))
                  ;; The same loader carries on under a new label.
                  (should (tuned-spinner-running-p))
                  (should (string-match-p "Refreshing" tuned-spinner-mode-line))
                  ;; Nothing was blanked: the old status is still there.
                  (should (equal before (buffer-string)))))
              (let ((tuned-spinner-min-duration 0))
                (tuned-spinner-mode-line-stop)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))


(ert-deftest tuned-git-status-keys ()
  "Folding, operations and RET are bound in the status mode and Evil."
  (dolist (spec '(("TAB" . tuned-git-status-toggle)
                  ("<backtab>" . tuned-git-status-toggle-files)
                  ("s" . tuned-git-status-stage)
                  ("u" . tuned-git-status-unstage)
                  ("X" . tuned-git-status-discard)
                  ("S" . tuned-git-status-stage-all)
                  ("U" . tuned-git-status-unstage-all)
                  ("f" . tuned-git-status-fetch)
                  ("F" . tuned-git-status-pull)
                  ("RET" . tuned-git-status-visit)))
    (should (eq (keymap-lookup tuned-git-status-mode-map (car spec))
                (cdr spec))))
  (dolist (key '("P" "p"))
    (should (eq (keymap-lookup tuned-git-status-mode-map key)
                #'tuned-git-status-push)))
  ;; The menu keymap carries the refs it was built with as which-key labels.
  (require 'which-key)
  (let ((map (tuned-git-status--push-menu "origin/main" "origin/main")))
    (dolist (spec '(("p" . tuned-git-status-push-to-pushremote)
                    ("u" . tuned-git-status-push-to-upstream)
                    ("e" . tuned-git-status-push-elsewhere)))
      (should (eq (keymap-lookup map (car spec)) (cdr spec))))
    (dolist (spec '(("p" . "push to origin/main")
                    ("u" . "push to origin/main")
                    ("e" . "push elsewhere")))
      (should (equal (cdr (assoc (car spec)
                                 (which-key--get-keymap-bindings map)))
                     (cdr spec)))))
  (require 'evil)
  (let ((normal (evil-get-auxiliary-keymap
                 tuned-git-status-mode-map 'normal t)))
    (dolist (spec '(("TAB" . tuned-git-status-toggle)
                    ("<backtab>" . tuned-git-status-toggle-files)
                    ("s" . tuned-git-status-stage)
                    ("u" . tuned-git-status-unstage)
                    ("X" . tuned-git-status-discard)
                    ("x" . tuned-git-status-discard)
                    ("S" . tuned-git-status-stage-all)
                    ("U" . tuned-git-status-unstage-all)
                    ("RET" . tuned-git-status-visit)))
      (should (eq (keymap-lookup normal (car spec)) (cdr spec))))
    (dolist (key '("P" "p"))
      (should (eq (keymap-lookup normal key) #'tuned-git-status-push)))))

(ert-deftest tuned-git-status-stages-and-unstages ()
  "`s' stages the file at point and `u' puts it back in the index."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              ;; file-0.txt is unstaged: stage it.
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (tuned-git-status-stage)
              (tuned-test--wait-for-git-status buffer)
              (should (equal "file-0.txt"
                             (tuned-test--git-output repo "diff" "--cached"
                                                  "--name-only")))
              ;; It is under Staged changes now: unstage it again.
              (goto-char (point-min))
              (should (search-forward "Staged changes" nil t))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (tuned-git-status-unstage)
              (tuned-test--wait-for-git-status buffer)
              (should (equal "" (tuned-test--git-output repo "diff" "--cached"
                                                      "--name-only"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-keeps-point-on-a-staged-file ()
  "`s' leaves point on the file, which now sits under Staged changes."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (tuned-git-status-stage)
              (tuned-test--wait-for-git-status buffer)
              ;; The refresh moved the file, not point: same path, now staged.
              (should (looking-at "modified +file-0\\.txt"))
              (should (eq (plist-get (tuned-git-status--section-at-point) :status)
                          'staged))
              ;; Unstaging brings point back with the file.
              (tuned-git-status-unstage)
              (tuned-test--wait-for-git-status buffer)
              (should (looking-at "modified +file-0\\.txt"))
              (should (eq (plist-get (tuned-git-status--section-at-point) :status)
                          'unstaged)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-keeps-point-on-a-staged-hunk ()
  "`s' on a hunk follows the hunk into Staged changes.
The staged file diff is unfolded so the restored line is visible."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "file.txt" nil t))
              (beginning-of-line)
              ;; File diffs start folded, so unfold this one to reach a hunk.
              (tuned-git-status-toggle)
              (should (search-forward "+changed 1" nil t))
              (beginning-of-line)
              (tuned-git-status-stage)
              (tuned-test--wait-for-git-status buffer)
              ;; Same line of the same hunk, now under Staged changes.
              (should (looking-at "\\+changed 1"))
              (should (eq (plist-get (tuned-git-status--section-at-point) :status)
                          'staged))
              (should-not (invisible-p (point))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))


;; A background push that ends while its status window is closed must not
;; open the window again; one that ends while the window is on screen refreshes it.
(ert-deftest tuned-git-status-drain-does-not-reopen-a-closed-status ()
  "The queue draining refreshes a status only when it is on screen."
  (tuned-test-with-temp-dir root
    (let ((buffer (generate-new-buffer " *git-status-drain-test*"))
          (refreshed 0)
          (popped 0))
      (unwind-protect
          (cl-letf (((symbol-function 'tuned-git-status)
                     (lambda (&rest _) (cl-incf refreshed)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _) (cl-incf popped)))
                    ((symbol-function 'display-buffer)
                     (lambda (&rest _) (cl-incf popped))))
            (with-current-buffer buffer
              (tuned-git-status-mode)
              (setq default-directory (file-name-as-directory root))
              (tuned-spinner-mode-line-start "Pushing"))
            ;; Closed: no refresh, no window, and the loader ends.
            (tuned-git-status--queue-drained (file-name-as-directory root))
            (should (= refreshed 0))
            (should (= popped 0))
            (with-current-buffer buffer
              (should-not tuned-git-status--queue-action))
            ;; On screen: the status refreshes in place.
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (&rest _) 'window)))
              (tuned-git-status--queue-drained (file-name-as-directory root)))
            (should (= refreshed 1))
            (should (= popped 0)))
        (kill-buffer buffer)))))

(ert-deftest tuned-git-status-queues-operations-in-order ()
  "Rapid `s' presses run one git command at a time, then refresh once.
The loader is visible while the queue runs."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (tuned-git-status-stage)
              ;; The command returned with git still running: no blocking,
              ;; and no loader yet, because fast operations must not flicker.
              (should (tuned-async-queue-busy-p (tuned-git-core-queue-key)))
              (should (equal "Staged file-0.txt"
                             tuned-git-status--queue-action))
              (should-not (tuned-spinner-running-p))
              (should-not tuned-spinner-mode-line)
              ;; Once it runs past the delay, the mode line says what runs.
              (tuned-test--slot-show-now buffer)
              (should (tuned-spinner-running-p))
              (should (equal (concat " " (tuned-spinner-current-frame)
                                     " Staged file-0.txt ")
                             tuned-spinner-mode-line))
              (should (equal '(:eval tuned-spinner-mode-line)
                             (car mode-line-format)))
              (goto-char (point-min))
              (should (search-forward "modified   file-1.txt" nil t))
              (beginning-of-line)
              (tuned-git-status-stage))
            (tuned-test--wait-for-git-status buffer)
            (should (equal '("file-0.txt" "file-1.txt")
                           (sort (split-string
                                  (tuned-test--git-output repo "diff" "--cached"
                                                       "--name-only")
                                  "\n" t)
                                 #'string<)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-pushes ()
  "The push menu pushes to the push remote and to the upstream."
  (skip-unless (executable-find "git"))
  (let* ((repo (tuned-test--git-repo))
         (remote (make-temp-file "tuned-test-remote-" t))
         (branch (tuned-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD"))
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
              (tuned-git-status)
              (tuned-test--wait-for-git-status buffer)
              ;; The render remembers what the push menu offers.
              (with-current-buffer buffer
                (should (equal branch tuned-git-status--head-branch))
                (should (equal (format "origin/%s" branch)
                               tuned-git-status--head-upstream)))
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
                    (tuned-git-status-push))
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
                                 tuned-git-status--queue-action))
                  (should-not (tuned-spinner-running-p))
                  (tuned-test--slot-show-now buffer)
                  (should (equal (format " %s Pushing %s to origin "
                                         (tuned-spinner-current-frame) branch)
                                 tuned-spinner-mode-line))
                  (let ((frame (tuned-spinner-current-frame)))
                    (tuned-spinner-tick)
                    (should-not (equal frame (tuned-spinner-current-frame))))
                  (should (equal (format " %s Pushing %s to origin "
                                         (tuned-spinner-current-frame) branch)
                                 tuned-spinner-mode-line)))))
            (tuned-test--wait-for-git-status buffer)
            (tuned-test--wait-for-slot buffer)
            (with-current-buffer buffer
              (should-not tuned-spinner-mode-line))
            (should (equal (tuned-test--git-output repo "rev-parse" branch)
                           (tuned-test--git-output remote "rev-parse" branch)))
            ;; A third commit goes out through the upstream choice.
            (with-temp-file (expand-file-name "file-0.txt" repo)
              (insert "changed once more\n"))
            (should (zerop (call-process "git" nil nil nil "-C" repo
                                         "-c" "user.email=test@example.com"
                                         "-c" "user.name=Test"
                                         "commit" "-qam" "third")))
            (with-current-buffer buffer
              (tuned-git-status-push-to-upstream)
              (should (equal (format "Pushing to origin/%s" branch)
                             tuned-git-status--queue-action)))
            (tuned-test--wait-for-git-status buffer)
            (should (equal (tuned-test--git-output repo "rev-parse" branch)
                           (tuned-test--git-output remote "rev-parse" branch)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory remote t))))

(ert-deftest tuned-git-status-pushes-elsewhere ()
  "The `e' choice asks for a remote and branch, then pushes there."
  (skip-unless (executable-find "git"))
  (let* ((repo (tuned-test--git-repo))
         (remote (make-temp-file "tuned-test-remote-" t))
         (branch (tuned-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD"))
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
              (tuned-git-status)
              (tuned-test--wait-for-git-status buffer)
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
                  (tuned-git-status-push-elsewhere))
                (should (equal "Pushing to origin/elsewhere"
                               tuned-git-status--queue-action))))
            (tuned-test--wait-for-git-status buffer)
            (should (equal (tuned-test--git-output repo "rev-parse" branch)
                           (tuned-test--git-output remote "rev-parse"
                                                "elsewhere")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory remote t))))

(ert-deftest tuned-git-status-visits-files-and-commits ()
  "`RET' opens the file at point and shows the commit at point."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil)
        (opened nil)
        (commit nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b))
                  ((symbol-function 'find-file)
                   (lambda (file) (setq opened file)))
                  ((symbol-function 'tuned-git-show-commit)
                   (lambda (hash) (setq commit hash))))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "modified   file-0.txt" nil t))
              (beginning-of-line)
              (tuned-git-status-visit)
              (should (equal opened (expand-file-name "file-0.txt" repo)))
              (goto-char (point-min))
              (should (search-forward "Recent commits" nil t))
              (should (re-search-forward "^[0-9a-f]+ " nil t))
              (tuned-git-status-visit)
              (should (string-match-p "\\`[0-9a-f]+\\'" commit)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-visits-a-hunk-line ()
  "`RET' on a hunk opens its file at the line the hunk line maps to."
  (skip-unless (executable-find "git"))
  (let* ((repo (tuned-test--git-repo-hunks))
         (buffer nil)
         (path (expand-file-name "file.txt" repo)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory repo)))
          ;; Stub only the status window: the visits below use `find-file',
          ;; which itself goes through `display-buffer'.
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
            (tuned-git-status))
          (tuned-test--wait-for-git-status buffer)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "modified   file.txt")
            (beginning-of-line)
            (tuned-git-status-toggle))
          (let ((visit-line
                 (lambda (regexp nth)
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (dotimes (_ nth) (re-search-forward regexp))
                     (beginning-of-line)
                     (tuned-git-status-visit))
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


(ert-deftest tuned-git-show-uses-the-status-faces ()
  "The commit buffer reuses the status buffer's faces for headers and diffs."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
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
            (tuned-git-show-commit "HEAD")
            (setq buffer (get-buffer "*git-show: HEAD*"))
            (should (buffer-live-p buffer))
            (tuned-test--wait-for-git-show buffer)
            (with-current-buffer buffer
              (should (derived-mode-p 'special-mode))
              (should (derived-mode-p 'tuned-git-show-mode))
              (should buffer-read-only)
              (let ((face-at
                     (lambda (regexp)
                       (goto-char (point-min))
                       (should (re-search-forward regexp nil t))
                       (get-text-property (match-beginning 0) 'face))))
                ;; Commit header.
                (should (eq (funcall face-at "^commit ") 'tuned-git-status-heading))
                (goto-char (point-min))
                (should (re-search-forward "^commit \\([0-9a-f]+\\)" nil t))
                (should (eq (get-text-property (match-beginning 1) 'face)
                            'tuned-git-status-hash))
                (should (eq (funcall face-at "^Author: ")
                            'tuned-git-status-heading))
                ;; Diff header, hunk and body: the same faces as the status.
                (should (eq (funcall face-at "^diff --git ") 'diff-header))
                (should (eq (funcall face-at "^--- ") 'diff-header))
                (should (eq (funcall face-at "^@@ ")
                            'tuned-git-status-hunk-heading))
                (should (eq (funcall face-at "^ line 0") 'diff-context))
                (should (eq (funcall face-at "^-line 1") 'diff-removed))
                (should (eq (funcall face-at "^\\+changed 1") 'diff-added))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-show-opens-before-git-answers ()
  "The commit buffer appears with a loader, then the diff replaces it."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil
                                       "-C" repo
                                       "-c" "user.email=test@example.com"
                                       "-c" "user.name=Test"
                                       "commit" "-qam" "change file")))
          (let ((default-directory (file-name-as-directory repo)))
            (setq buffer (tuned-git-show-commit "HEAD"))
            (with-current-buffer buffer
              ;; The buffer is there before git answers: loader on, no diff.
              (should (tuned-spinner-running-p))
              (should (overlayp tuned-git-show--show-spinner))
              (should (string-match-p "Loading\\.\\.\\." (buffer-string)))
              (should-not (string-match-p "^diff --git" (buffer-string))))
            (tuned-test--wait-for-git-show buffer)
            (with-current-buffer buffer
              (should-not (tuned-spinner-running-p))
              (should-not (overlayp tuned-git-show--show-spinner))
              (should (string-prefix-p "commit " (buffer-string)))
              (should (string-match-p "^diff --git" (buffer-string))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-expands-an-abbreviated-root ()
  "A dired-style ~/... root is expanded before `git -C' sees it."
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (&rest _) "~/workspace/dotfiles/")))
    (let ((default-directory temporary-file-directory))
      (should (equal (tuned-git-status--root)
                     (file-name-as-directory
                      (expand-file-name "~/workspace/dotfiles/")))))))

(ert-deftest tuned-git-status-rejects-a-non-repo ()
  "Calling the status outside a Git repository is a clear user error."
  (tuned-test-with-temp-dir dir
    (let ((default-directory (file-name-as-directory dir)))
      (should-error (tuned-git-status) :type 'user-error))))

(ert-deftest tuned-git-status-folds-sections ()
  "File diffs start folded, TAB toggles hunks, and refreshes keep the state."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
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
                            'tuned-git-status-hunk-heading))
                (funcall goto-file)
                (tuned-git-status-toggle)
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
                (tuned-git-status-toggle)
                (should-not (invisible-p (funcall hunk-heading)))
                (should (invisible-p (funcall hunk-body)))
                ;; A refresh keeps the file open and the hunk folded.
                (tuned-git-status)
                (tuned-test--wait-for-git-status buffer)
                (should-not (invisible-p (funcall hunk-heading)))
                (should (invisible-p (funcall hunk-body)))
                ;; S-TAB folds every file diff, then unfolds it again.
                (tuned-git-status-toggle-files)
                (should (invisible-p (funcall hunk-heading)))
                (tuned-git-status-toggle-files)
                (should-not (invisible-p (funcall hunk-heading)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-stages-one-hunk ()
  "`s' on a hunk stages that hunk and leaves the other one unstaged."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "modified   file.txt")
              (beginning-of-line)
              (tuned-git-status-toggle)
              (goto-char (point-min))
              (re-search-forward "^@@")
              (beginning-of-line)
              (tuned-git-status-stage))
            (tuned-test--wait-for-git-status buffer)
            (let ((cached (tuned-test--git-output repo "diff" "--cached"
                                               "--unified=0"))
                  (unstaged (tuned-test--git-output repo "diff" "--unified=0"))
                  (worktree (tuned-test--read-file (expand-file-name "file.txt" repo))))
              (should (string-match-p "changed 1\\b" cached))
              (should-not (string-match-p "changed 18" cached))
              (should (string-match-p "changed 18" unstaged))
              (should-not (string-match-p "changed 1\\b" unstaged))
              ;; Staging a hunk touches the index, not the working tree.
              (should (string-match-p "changed 1\\b" worktree))
              (should (string-match-p "changed 18" worktree)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(defun tuned-test--remote-with-peer ()
  "Return (REPO . PEER) sharing a bare remote; REPO tracks it, PEER pushes.
PEER commits `peer.txt' and pushes it, so REPO is one commit behind."
  (let* ((remote (make-temp-file "tuned-test-remote-" t))
         (repo (tuned-test--git-repo))
         (peer (make-temp-file "tuned-test-peer-" t))
         (branch (tuned-test--git-output repo "rev-parse" "--abbrev-ref" "HEAD")))
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

(ert-deftest tuned-git-status-fetch-updates-the-remote-branch ()
  "`f' fetches the upstream: the remote-tracking ref moves, the branch does not."
  (skip-unless (executable-find "git"))
  (let* ((pair (tuned-test--remote-with-peer))
         (repo (car pair))
         (before (tuned-test--git-output repo "rev-parse" "HEAD"))
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (tuned-git-status-fetch)
              (should (tuned-async-queue-busy-p (tuned-git-core-queue-key))))
            (tuned-test--wait-for-git-status buffer)
            ;; The branch stays where it was; only origin/<branch> moved.
            (should (equal before (tuned-test--git-output repo "rev-parse" "HEAD")))
            (should-not (equal before
                               (tuned-test--git-output repo "rev-parse" "@{u}")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory (cdr pair) t))))

(ert-deftest tuned-git-status-pull-fast-forwards-the-branch ()
  "`F' fast-forwards the branch to its upstream and refreshes the status."
  (skip-unless (executable-find "git"))
  (let* ((pair (tuned-test--remote-with-peer))
         (repo (car pair))
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            ;; The peer's commit only adds peer.txt, so the local edits to
            ;; file-N.txt do not get in the way of a fast-forward.
            (with-current-buffer buffer
              (tuned-git-status-pull))
            (tuned-test--wait-for-git-status buffer)
            (should (equal (tuned-test--git-output repo "rev-parse" "HEAD")
                           (tuned-test--git-output repo "rev-parse" "@{u}")))
            (should (file-exists-p (expand-file-name "peer.txt" repo)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t)
      (delete-directory (cdr pair) t))))

(ert-deftest tuned-git-status-pull-refuses-without-an-upstream ()
  "`F' on a branch with no upstream says so, instead of running git."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (should-error (tuned-git-status-pull) :type 'user-error)
              (should-not (tuned-async-queue-busy-p
                           (tuned-git-core-queue-key))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-stages-a-path-with-a-space ()
  "A path with a space shows its diff and stages.
Porcelain without `-z' quoted it, and `git add' took the quotes literally."
  (skip-unless (executable-find "git"))
  (let ((repo (make-temp-file "tuned-test-git-space-" t))
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
              (tuned-git-status)
              (tuned-test--wait-for-git-status buffer)
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (search-forward "modified   a b.txt" nil t))
                (beginning-of-line)
                ;; The single whole-repo diff was split back onto the file.
                (tuned-git-status-toggle)
                (should (string-match-p "^\\+two$" (buffer-string)))
                (goto-char (point-min))
                (search-forward "modified   a b.txt")
                (beginning-of-line)
                (tuned-git-status-stage))
              (tuned-test--wait-for-git-status buffer)
              (should (equal "a b.txt"
                             (tuned-test--git-output repo "diff" "--cached"
                                                  "--name-only"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-unstages-one-hunk ()
  "`u' on a staged hunk takes just that hunk out of the index."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil
                                       "-C" repo "add" "file.txt")))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
            (let ((default-directory (file-name-as-directory repo)))
              (tuned-git-status)
              (tuned-test--wait-for-git-status buffer)
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Staged changes")
                (search-forward "modified   file.txt")
                (beginning-of-line)
                (tuned-git-status-toggle)
                (goto-char (point-min))
                (re-search-forward "^@@")
                (beginning-of-line)
                (tuned-git-status-unstage))
              (tuned-test--wait-for-git-status buffer)
              (let ((cached (tuned-test--git-output repo "diff" "--cached"
                                                 "--unified=0"))
                    (unstaged (tuned-test--git-output repo "diff" "--unified=0")))
                (should-not (string-match-p "changed 1\\b" cached))
                (should (string-match-p "changed 18" cached))
                (should (string-match-p "changed 1\\b" unstaged))
                (should-not (string-match-p "changed 18" unstaged))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-discards-a-hunk ()
  "`X' reverts one hunk in the working tree after confirmation."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo-hunks))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "modified   file.txt")
              (beginning-of-line)
              (tuned-git-status-toggle)
              ;; The second hunk reverts line 18 and nothing else.
              (goto-char (point-min))
              (re-search-forward "^@@")
              (re-search-forward "^@@")
              (beginning-of-line)
              (tuned-git-status-discard))
            (tuned-test--wait-for-git-status buffer)
            (let ((worktree (tuned-test--read-file (expand-file-name "file.txt" repo))))
              (should (string-match-p "changed 1\\b" worktree))
              (should-not (string-match-p "changed 18" worktree))
              (should (equal "" (tuned-test--git-output repo "diff" "--cached"
                                                     "--name-only")))
              (let ((unstaged (tuned-test--git-output repo "diff" "--unified=0")))
                (should (string-match-p "changed 1\\b" unstaged))
                (should-not (string-match-p "changed 18" unstaged))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-deletes-untracked-with-y-or-n ()
  "Deleting an untracked file asks with `y-or-n-p', not `yes-or-no-p'."
  (skip-unless (executable-find "git"))
  (let* ((repo (tuned-test--git-repo))
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
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "untracked  untracked.txt")
              (beginning-of-line)
              (tuned-git-status-discard))
            (should (string-match-p "Delete" asked))
            (should-not (file-exists-p file))
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (should-not (string-match-p "untracked.txt" (buffer-string))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

(ert-deftest tuned-git-status-stages-and-unstages-a-section ()
  "`s' on a section heading stages all its files; S and U act globally."
  (skip-unless (executable-find "git"))
  (let ((repo (tuned-test--git-repo))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (b &rest _) (setq buffer b) (set-window-buffer (selected-window) b) b)))
          (let ((default-directory (file-name-as-directory repo)))
            (tuned-git-status)
            (tuned-test--wait-for-git-status buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Unstaged changes")
              (beginning-of-line)
              (tuned-git-status-stage))
            (tuned-test--wait-for-git-status buffer)
            (should (equal "file-0.txt\nfile-1.txt\nfile-2.txt"
                           (tuned-test--git-output repo "diff" "--cached"
                                                "--name-only")))
            (with-current-buffer buffer
              (tuned-git-status-stage-all))
            (tuned-test--wait-for-git-status buffer)
            (should (string-match-p "untracked\\.txt"
                                    (tuned-test--git-output repo "diff" "--cached"
                                                         "--name-only")))
            (with-current-buffer buffer
              (tuned-git-status-unstage-all))
            (tuned-test--wait-for-git-status buffer)
            (should (equal "" (tuned-test--git-output repo "diff" "--cached"
                                                    "--name-only")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory repo t))))

