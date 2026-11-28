;;; tuned-git-status.el --- Async Git status buffer for Emacs  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tuned-async "0.1.0") (tuned-git-core "0.1.0") (tuned-git-show "0.1.0"))
;; Keywords: tools, vc, convenience

;;; Commentary:
;; `tuned-git-status' shows a Magit-like status without loading Magit:
;; the head, untracked/unstaged/staged files with their diff hunks, and
;; recent commits.  The git commands run asynchronously; while they run an
;; animated ASCII spinner fills the buffer, then it is replaced by the
;; status.  The buffer is read-only, `g' refreshes it and `q' quits it.
;; Sections fold like Magit's (TAB toggles the file or hunk at point,
;; S-TAB toggles every file diff).  On a hunk, file or section line `s' stages and `u'
;; unstages it, `X' discards it after confirmation, and `RET' visits the
;; file at the hunk line or shows the commit at point in a buffer that
;; uses the same faces.  `p' opens a push menu like Magit's `Push <branch>
;; to': the labelled keymap is shown at once, without which-key's idle
;; delay, with `p' for the push remote, `u' for the upstream and `e' for
;; anywhere else.
;;
;; The Git processes, the parsing and the write queue live in
;; tuned-git-core.el; the faces, the diff insertion and the commit view live in
;; tuned-git-show.el.  This file draws the status, owns the mode and its keys,
;; and keeps the visible buffer fresh.  The vim-fugitive style G* commands
;; (Gstatus, Gblame, Gwrite, Gcommit) live in tuned-git-commands.el.

;;; Code:

(require 'subr-x)       ; string-trim-right
(require 'seq)          ; seq-find, seq-remove
(require 'tuned-async)     ; the shared loading indicator and write queue
(require 'tuned-spinner)
(require 'tuned-git-core)
(require 'tuned-git-show)
(autoload 'file-notify-add-watch "filenotify")
(autoload 'file-notify-rm-watch "filenotify")

;; The git processes belong to `tuned-async': the four reads of a refresh run
;; in the slots `load-log', `load-status', `load-unstaged' and
;; `load-staged', and the staging and push operations in the write queue of
;; the repository, which tuned-git-core owns and the G commands share.
;; `git show' runs in the commit buffer's slot `show' in tuned-git-show.

(defvar-local tuned-git-status--queue-action nil
  "What the queued operation is doing, for the mode line.")

(defvar-local tuned-git-status--sections nil
  "Sections of this status buffer, as plists.
Each plist has :key, :kind (`section', `file' or `hunk'), :status
\(`untracked', `unstaged', `staged' or `log'), :heading-start,
:content-start and :content-end.  File sections add :path; hunk
sections add :patch and :hunk-text (the hunk without the diff
header, used to follow a hunk across sections); change sections
add :files.")

(defvar-local tuned-git-status--overlays nil
  "Invisibility overlays that fold sections in this buffer.")

(defvar-local tuned-git-status--expanded nil
  "Hash table of section keys the user unfolded, kept across refreshes.
Sections missing from it use their default: foldable headings and
hunks start unfolded, file diffs start folded.")

(defvar-local tuned-git-status--restore-section nil
  "Section to return to after the next render, or nil.
It is the section at point when a refresh starts, with :heading-line
added (the line number before the buffer is emptied) and
:point-offset (the distance into the section when point was inside
it rather than on its heading).")

(defvar-local tuned-git-status--head-branch nil
  "Branch of the last render, or nil for a detached HEAD.")

(defvar-local tuned-git-status--head-upstream nil
  "Upstream ref of the last render, e.g. \"origin/master\", or nil.")


;; ============================================================
;; Loading indicator
;; ============================================================
;; Git runs asynchronously, so the buffer is briefly empty on its first
;; load.  The loader is the one every buffer of this config uses: the
;; `tuned-spinner' slot at the left of the mode line, and, while the buffer is
;; still empty, the same frames in the middle of the window, drawn through
;; the slot's `:also' hook on the slot's clock.  The slot waits
;; `tuned-spinner-show-delay' first, so a status that is ready sooner shows
;; no loader at all.

(defvar-local tuned-git-status--blank nil
  "Non-nil while this status buffer is empty, before its first render.")

(defvar-local tuned-git-status--center nil
  "Overlay drawing the loader in the middle of an empty status window.")

(defun tuned-git-status--draw-center (frame)
  "Draw FRAME in the middle of this status window while it is empty.
nil, or a buffer that has rendered, removes it.  The buffer has no text
yet, so the loader is the `before-string' of an overlay at its start,
with newlines that bring it down to the middle row."
  (if (not (and frame tuned-git-status--blank))
      (when (overlayp tuned-git-status--center)
        (delete-overlay tuned-git-status--center)
        (setq tuned-git-status--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  git status…"))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (rows (max 0 (/ (window-body-height window) 2))))
        (unless (overlayp tuned-git-status--center)
          (setq tuned-git-status--center (make-overlay (point-min) (point-min))))
        (overlay-put tuned-git-status--center 'before-string
                     (concat (make-string rows ?\n) (make-string pad ?\s) text))))))

(defun tuned-git-status--stop-spinner ()
  "Remove this status buffer's loaders once a render is on screen.
The loader in the middle goes at once.  The mode-line loader ends unless
more operations are queued; `tuned-spinner' keeps it for its minimum time,
so it never flashes."
  (setq tuned-git-status--blank nil)
  (tuned-git-status--draw-center nil)
  (unless (tuned-async-queue-busy-p (tuned-git-core-queue-key))
    (tuned-spinner-mode-line-stop)))

(defun tuned-git-status--start-spinner ()
  "Show the first-load loader, in the mode line and the window's middle."
  (setq tuned-git-status--blank t)
  (tuned-spinner-mode-line-start "git status" :also #'tuned-git-status--draw-center))


;; ============================================================
;; Sections and folding
;; ============================================================

(defun tuned-git-status--insert-file (status label path diff)
  "Insert a file line for PATH, then its DIFF when there is one.
STATUS is the section the file belongs to and LABEL its porcelain
status."
  (let ((section (list :key (format "file:%s:%s" status path)
                       :kind 'file
                       :status status
                       :path path
                       :heading-start (point))))
    (insert (propertize (format "%-10s " label)
                        'face (tuned-git-show-label-face label))
            (propertize path 'face 'tuned-git-status-file)
            "\n")
    (plist-put section :content-start (point))
    (when diff
      (setq tuned-git-status--sections
            (append (tuned-git-show-insert-diff status path diff)
                    tuned-git-status--sections)))
    (plist-put section :content-end (point))
    (push section tuned-git-status--sections)))

(defun tuned-git-status--insert-files (title status files diffs)
  "Insert a TITLE section for FILES with STATUS and their DIFFS.
FILES is a list of (LABEL . PATH); DIFFS maps \"STATUS:PATH\" to the
diff text."
  (when files
    (let ((section (list :key (concat "section:" (symbol-name status))
                         :kind 'section
                         :status status
                         :files (mapcar #'cdr files)
                         :heading-start (point))))
      (insert (propertize (format "%s (%d)\n" title (length files))
                          'face 'tuned-git-status-heading))
      (plist-put section :content-start (point))
      (dolist (entry files)
        (tuned-git-status--insert-file
         status (car entry) (cdr entry)
         (gethash (concat (symbol-name status) ":" (cdr entry)) diffs)))
      (plist-put section :content-end (point))
      (push section tuned-git-status--sections))
    (insert "\n")))

(defun tuned-git-status--expanded-p (section)
  "Return non-nil when SECTION is unfolded."
  (let ((value (gethash (plist-get section :key)
                        tuned-git-status--expanded 'unset)))
    (if (eq value 'unset)
        (memq (plist-get section :kind) '(section hunk))
      value)))

(defun tuned-git-status--section-at-point ()
  "Return the innermost section that contains point, or nil."
  (let ((pos (point))
        best)
    (dolist (section tuned-git-status--sections best)
      (let ((start (plist-get section :heading-start))
            (end (plist-get section :content-end)))
        (when (and start end (<= start pos) (< pos end))
          (when (or (null best)
                    (> start (plist-get best :heading-start)))
            (setq best section)))))))

(defun tuned-git-status--apply-folds ()
  "Create invisibility overlays for the sections that are folded."
  (mapc #'delete-overlay tuned-git-status--overlays)
  (setq tuned-git-status--overlays nil)
  (dolist (section tuned-git-status--sections)
    (let ((start (plist-get section :content-start))
          (end (plist-get section :content-end)))
      (when (and start end (< start end)
                 (not (tuned-git-status--expanded-p section)))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'invisible t)
          (push overlay tuned-git-status--overlays))))))

(defun tuned-git-status--move-out-of-hidden ()
  "Move point to the buffer start when folding hid it."
  (when (invisible-p (point))
    (goto-char (point-min))))

(defun tuned-git-status-toggle ()
  "Fold or unfold the section at point."
  (interactive)
  (let ((section (tuned-git-status--section-at-point)))
    (unless section
      (user-error "No section at point"))
    (when (= (plist-get section :content-start)
             (plist-get section :content-end))
      (user-error "Nothing to fold here"))
    (let ((expanded (not (tuned-git-status--expanded-p section))))
      (puthash (plist-get section :key) expanded tuned-git-status--expanded)
      (tuned-git-status--apply-folds)
      (unless expanded
        (goto-char (plist-get section :heading-start))))))

(defun tuned-git-status-toggle-files ()
  "Fold every file diff, or unfold them all when any is folded."
  (interactive)
  (let ((files (seq-filter (lambda (section)
                             (and (eq (plist-get section :kind) 'file)
                                  (< (plist-get section :content-start)
                                     (plist-get section :content-end))))
                           tuned-git-status--sections)))
    (unless files
      (user-error "No file changes to fold"))
    ;; Any folded diff means "show them all"; otherwise hide them all.
    (let ((expand (seq-some (lambda (section)
                              (not (tuned-git-status--expanded-p section)))
                            files)))
      (dolist (section files)
        (puthash (plist-get section :key) expand tuned-git-status--expanded)))
    (tuned-git-status--apply-folds)
    (tuned-git-status--move-out-of-hidden)))

(defun tuned-git-status--save-point ()
  "Remember the section at point for the next render."
  (let* ((section (tuned-git-status--section-at-point))
         (content (plist-get section :content-start)))
    (setq tuned-git-status--restore-section
          (when section
            (append section
                    (list :heading-line
                          (line-number-at-pos
                           (plist-get section :heading-start))
                          :point-offset
                          (when (and content (>= (point) content))
                            (- (point) content))))))))

(defun tuned-git-status--nearest-section (line)
  "Return the section whose heading line is closest to LINE."
  (let (best distance)
    (dolist (section tuned-git-status--sections best)
      (let ((current (line-number-at-pos (plist-get section :heading-start))))
        (when (or (null best) (< (abs (- current line)) distance))
          (setq best section
                distance (abs (- current line))))))))

(defun tuned-git-status--matching-section (old)
  "Return the section that replaces OLD after a render, or nil.
A hunk is followed by its text, because staging or unstaging moves
it to the other change section and renumbers the hunks there.  A
file is followed by its path, because the part that changes is its
status.  Anything else falls back to the heading closest to where
OLD was."
  (let ((kind (plist-get old :kind)))
    (or (if (eq kind 'hunk)
            (seq-find (lambda (section)
                        (and (eq (plist-get section :kind) 'hunk)
                             (equal (plist-get section :path)
                                    (plist-get old :path))
                             (equal (plist-get section :hunk-text)
                                    (plist-get old :hunk-text))))
                      tuned-git-status--sections)
          (seq-find (lambda (section)
                      (equal (plist-get section :key)
                             (plist-get old :key)))
                    tuned-git-status--sections))
        (when (eq kind 'file)
          (seq-find (lambda (section)
                      (and (eq (plist-get section :kind) 'file)
                           (equal (plist-get section :path)
                                  (plist-get old :path))))
                    tuned-git-status--sections))
        (tuned-git-status--nearest-section (plist-get old :heading-line)))))

(defun tuned-git-status--reveal (position)
  "Unfold the sections that hide POSITION, so point can go there."
  (let (changed)
    (dolist (section tuned-git-status--sections)
      (when (and (plist-get section :content-start)
                 (<= (plist-get section :content-start) position)
                 (< position (plist-get section :content-end))
                 (not (tuned-git-status--expanded-p section)))
        (puthash (plist-get section :key) t tuned-git-status--expanded)
        (setq changed t)))
    (when changed
      (tuned-git-status--apply-folds))))

(defun tuned-git-status--restore-target (section old)
  "Return the position in SECTION that corresponds to OLD."
  (let ((offset (plist-get old :point-offset))
        (start (plist-get section :content-start)))
    (if (and offset start)
        ;; Same line inside the section, when the new content still has it.
        (min (+ start offset) (max start (1- (plist-get section :content-end))))
      (plist-get section :heading-start))))

(defun tuned-git-status--restore-point ()
  "Move point back to the section marked for restore, else to the start."
  (let* ((old tuned-git-status--restore-section)
         (section (and old (tuned-git-status--matching-section old)))
         (target (if section
                     (tuned-git-status--restore-target section old)
                   (point-min))))
    (setq tuned-git-status--restore-section nil)
    ;; Staging a hunk moves it into a file diff that starts folded.
    (when (invisible-p target)
      (tuned-git-status--reveal target))
    (goto-char target)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window target))))


(defun tuned-git-status--buffer-for-root (root)
  "Return the live status buffer showing ROOT's repository, or nil.
Find it by directory, not by buffer name: two repositories with the
same directory name would share a name."
  (let ((root (file-truename (file-name-as-directory
                              (expand-file-name root)))))
    (seq-find (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'tuned-git-status-mode)
                       (not (tuned-async-buffer-dying-p))
                       (ignore-errors
                         (equal (file-truename
                                 (file-name-as-directory
                                  (expand-file-name default-directory)))
                                root)))))
              (buffer-list))))

(defun tuned-git-status--enqueue (job)
  "Queue JOB on the write queue of its repository, with the status loader.
JOB is a plist with :root, :attempts, :action, :progress (the mode line
label while it runs, defaulting to :action) and, for `git apply', :patch.
Only a status buffer shows the operation: a Gwrite from a file buffer
starts no loader and is unowned, so closing the file buffer cannot kill a
commit halfway through its hooks.  The queue refreshes the status once it
drains; see `tuned-git-core-enqueue'."
  (let* ((root (plist-get job :root))
         (status (and (derived-mode-p 'tuned-git-status-mode) (current-buffer)))
         (action (plist-get job :action))
         (progress (or (plist-get job :progress) action)))
    (tuned-git-core-enqueue
     (append job
             (list :owner status
                   :on-start (lambda ()
                               (when (buffer-live-p status)
                                 (with-current-buffer status
                                   (setq tuned-git-status--queue-action progress)
                                   (tuned-spinner-mode-line-label
                                    (tuned-git-status--queue-label root)))))))
     (lambda () (tuned-git-status--queue-drained root)))
    (when status
      (tuned-spinner-mode-line-start (tuned-git-status--queue-label root)))))

(defun tuned-git-status--queue-label (&optional root)
  "Return the mode-line label of ROOT's write queue.
It names the running operation and, after it, how many wait."
  (let ((waiting (tuned-async-queue-length (tuned-git-core-queue-key root))))
    (concat (or tuned-git-status--queue-action "git")
            (if (> waiting 0) (format " +%d" waiting) ""))))

(defun tuned-git-status--queue-drained (root)
  "Refresh the status of ROOT once its write queue has run out of jobs.
The drain belongs to the repository, not to the buffer that queued the
last job, so it finds the status buffer showing ROOT by its directory.
Only a status on screen is refreshed: a background push that finishes
while the status window is closed must not open it again.  The refresh
relabels the running loader \"Refreshing\" instead of ending it, so the
mode line stays put until the new status is drawn.  A hidden status
ends its loader here; opening it later refreshes it anyway."
  (when-let* ((buffer (tuned-git-status--buffer-for-root root)))
    (with-current-buffer buffer
      (setq tuned-git-status--queue-action nil)
      (if (get-buffer-window buffer t)
          (save-selected-window (tuned-git-status))
        (tuned-spinner-mode-line-stop)))))

;; ============================================================
;; Operations
;; ============================================================

(defun tuned-git-status--section-files (section)
  "Return the file paths SECTION acts on."
  (pcase (plist-get section :kind)
    ('section (plist-get section :files))
    ((or 'file 'hunk) (list (plist-get section :path)))))

(defun tuned-git-status--describe-files (files)
  "Return a short description of FILES for messages."
  (if (= (length files) 1)
      (car files)
    (format "%d files" (length files))))

(defun tuned-git-status--unstage-attempts (files)
  "Return git argument lists that remove FILES from the index.
`git reset' needs HEAD; a repository without commits needs `git rm'."
  (list (append (list "reset" "-q" "HEAD" "--") files)
        (append (list "rm" "--cached" "-q" "-r" "--") files)))

(defun tuned-git-status-stage ()
  "Stage the hunk, file or change section at point.
The work runs in the background; the status refreshes when it is done."
  (interactive)
  (let* ((section (tuned-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'unstaged)
         (user-error "That hunk is already staged"))
       (tuned-git-status--enqueue
        (list :root default-directory
              :attempts '(("apply" "--cached"))
              :patch (plist-get section :patch)
              :action "Staged hunk")))
      ((or 'file 'section)
       (unless (memq status '(unstaged untracked))
         (user-error "Nothing unstaged there"))
       (let ((files (tuned-git-status--section-files section)))
         (tuned-git-status--enqueue
          (list :root default-directory
                :attempts (list (append (list "add" "--") files))
                :action (format "Staged %s"
                                (tuned-git-status--describe-files files))))))
      (_ (user-error "Nothing to stage at point")))))

(defun tuned-git-status-unstage ()
  "Unstage the hunk, file or change section at point.
The work runs in the background; the status refreshes when it is done."
  (interactive)
  (let* ((section (tuned-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'staged)
         (user-error "That hunk is not staged"))
       (tuned-git-status--enqueue
        (list :root default-directory
              :attempts '(("apply" "--cached" "-R"))
              :patch (plist-get section :patch)
              :action "Unstaged hunk")))
      ((or 'file 'section)
       (unless (eq status 'staged)
         (user-error "Nothing staged there"))
       (let ((files (tuned-git-status--section-files section)))
         (tuned-git-status--enqueue
          (list :root default-directory
                :attempts (tuned-git-status--unstage-attempts files)
                :action (format "Unstaged %s"
                                (tuned-git-status--describe-files files))))))
      (_ (user-error "Nothing to unstage at point")))))

(defun tuned-git-status--delete-untracked (file)
  "Delete untracked FILE, a directory or a plain file."
  (let ((path (expand-file-name file default-directory)))
    (if (file-directory-p path)
        (delete-directory path t)
      (delete-file path))))

(defun tuned-git-status-discard ()
  "Discard the unstaged or untracked change at point, after confirmation.
The git work runs in the background; the status refreshes when it is done."
  (interactive)
  (let* ((section (tuned-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'unstaged)
         (user-error "Unstage that hunk first"))
       (when (y-or-n-p "Discard this hunk? ")
         (tuned-git-status--enqueue
          (list :root default-directory
                :attempts '(("apply" "-R"))
                :patch (plist-get section :patch)
                :action "Discarded hunk"))))
      ((or 'file 'section)
       (let ((files (tuned-git-status--section-files section)))
         (pcase status
           ('unstaged
            (when (y-or-n-p (format "Discard changes in %s? "
                                       (tuned-git-status--describe-files files)))
              (tuned-git-status--enqueue
               (list :root default-directory
                     :attempts (list (append (list "checkout" "--") files))
                     :action (format "Discarded %s"
                                     (tuned-git-status--describe-files files))))))
           ('untracked
            (when (y-or-n-p (format "Delete %s? "
                                       (tuned-git-status--describe-files files)))
              (dolist (file files)
                (tuned-git-status--delete-untracked file))
              (tuned-git-status)
              (message "Deleted %s"
                       (tuned-git-status--describe-files files))))
           (_ (user-error "Unstage that change first")))))
      (_ (user-error "Nothing to discard at point")))))

(defun tuned-git-status-stage-all ()
  "Stage every change in the working tree, in the background."
  (interactive)
  (tuned-git-status--enqueue
   (list :root default-directory
         :attempts '(("add" "--all"))
         :action "Staged all changes")))

(defun tuned-git-status-unstage-all ()
  "Unstage everything in the index, in the background."
  (interactive)
  (tuned-git-status--enqueue
   (list :root default-directory
         :attempts (tuned-git-status--unstage-attempts '("."))
         :action "Unstaged all changes")))

(defun tuned-git-status--push-branch ()
  "Return the checked-out branch, or signal an error when detached."
  (or tuned-git-status--head-branch
      (user-error "No branch is checked out")))

(defun tuned-git-status--upstream-parts ()
  "Return (REMOTE . BRANCH) for the current branch's upstream, or nil."
  (when-let* ((upstream tuned-git-status--head-upstream)
              (slash (string-match "/" upstream)))
    (cons (substring upstream 0 slash)
          (substring upstream (1+ slash)))))

(defun tuned-git-status--push-remote (branch)
  "Return the remote BRANCH pushes to, or nil when none is configured.
Git prefers `branch.<name>.pushRemote' over `remote.pushDefault'
over the remote of the upstream, and the menu offers the same."
  (or (tuned-git-core-git-string
       default-directory "config" "--get"
       (format "branch.%s.pushRemote" branch))
      (tuned-git-core-git-string default-directory "config" "--get"
                                 "remote.pushDefault")
      (car-safe (tuned-git-status--upstream-parts))))

(defun tuned-git-status--remotes ()
  "Return the names of this repository's remotes."
  (split-string (or (tuned-git-core-git-string default-directory "remote") "")
                "\n" t))

(defun tuned-git-status-push-to-pushremote ()
  "Push the current branch to its push remote, in the background.
This is `p' in the push menu, the `Push <branch> to' part of Magit's
`P p'.  The push joins the operation queue, so the mode line shows
a loader until git answers and the status refreshes once it is done."
  (interactive)
  (let* ((branch (tuned-git-status--push-branch))
         (remote (tuned-git-status--push-remote branch)))
    (tuned-git-status--enqueue
     (list :root default-directory
           :attempts (list
                      (if remote
                          (list "push" remote
                                (format "refs/heads/%s:refs/heads/%s"
                                        branch branch))
                        '("push")))
           :action (if remote
                       (format "Pushed %s to %s" branch remote)
                     "Pushed")
           :progress (if remote
                         (format "Pushing %s to %s" branch remote)
                       "Pushing")))))

(defun tuned-git-status-push-to-upstream ()
  "Push the current branch to its upstream, in the background.
This is `u' in the push menu.  Without an upstream the push falls
back to `git push', whose error message carries git's own hint."
  (interactive)
  (let* ((branch (tuned-git-status--push-branch))
         (parts (tuned-git-status--upstream-parts)))
    (tuned-git-status--enqueue
     (list :root default-directory
           :attempts (list
                      (if parts
                          (list "push" (car parts)
                                (format "refs/heads/%s:refs/heads/%s"
                                        branch (cdr parts)))
                        '("push")))
           :action (format "Pushed %s" branch)
           :progress (if tuned-git-status--head-upstream
                         (format "Pushing to %s" tuned-git-status--head-upstream)
                       "Pushing")))))

(defun tuned-git-status-push-elsewhere ()
  "Push the current branch to a remote and branch chosen now.
This is `e' in the push menu, Magit's plain `Push elsewhere'."
  (interactive)
  (let* ((branch (tuned-git-status--push-branch))
         (remotes (tuned-git-status--remotes)))
    (unless remotes
      (user-error "No Git remotes"))
    (let* ((remote (completing-read (format "Push %s to remote: " branch)
                                    remotes nil t))
           (target (read-string (format "Remote branch (default %s): " branch)
                                nil nil branch)))
      (when (string-empty-p target)
        (user-error "No remote branch to push to"))
      (tuned-git-status--enqueue
       (list :root default-directory
             :attempts (list
                        (list "push" remote
                              (format "refs/heads/%s:refs/heads/%s"
                                      branch target)))
             :action (format "Pushed %s to %s/%s" branch remote target)
             :progress (format "Pushing to %s/%s" remote target))))))

(defun tuned-git-status-fetch ()
  "Fetch from the upstream's remote, or from the default remote.
This is `f'.  The fetch joins the operation queue, so the mode line
shows a loader while git talks to the remote, and the status refreshes
once it is done: new remote branches show up in the Head line."
  (interactive)
  (let* ((parts (tuned-git-status--upstream-parts))
         (remote (car-safe parts)))
    (tuned-git-status--enqueue
     (list :root default-directory
           :attempts (list (if remote (list "fetch" remote) '("fetch")))
           :action (if remote (format "Fetched %s" remote) "Fetched")
           :progress (if remote
                         (format "Fetching from %s" remote)
                       "Fetching")))))

(defun tuned-git-status-pull ()
  "Fast-forward the current branch from its upstream, in the background.
This is `F'.  It only fast-forwards (`--ff-only'): a diverged branch
fails with git's message instead of opening a merge editor the
background process cannot show, or creating a merge commit unasked."
  (interactive)
  (let* ((branch (tuned-git-status--push-branch))
         (upstream tuned-git-status--head-upstream))
    (unless upstream
      (user-error "No upstream to pull %s from" branch))
    (tuned-git-status--enqueue
     (list :root default-directory
           :attempts (list (list "pull" "--ff-only"))
           :action (format "Pulled %s" upstream)
           :progress (format "Pulling from %s" upstream)))))

(defun tuned-git-status--push-menu (push upstream)
  "Return a push destination keymap.
PUSH and UPSTREAM are the refs the menu shows next to `p' and `u'."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "p" (cons (format "push to %s" (or push "push remote"))
                              #'tuned-git-status-push-to-pushremote))
    (keymap-set map "u" (cons (format "push to %s" (or upstream "upstream"))
                              #'tuned-git-status-push-to-upstream))
    (keymap-set map "e" (cons "push elsewhere"
                              #'tuned-git-status-push-elsewhere))
    map))

(defun tuned-git-status--hide-push-menu ()
  "Hide what `tuned-git-status-push' drew, when which-key drew it."
  (when (fboundp 'which-key--hide-popup)
    (which-key--hide-popup)))

(defun tuned-git-status-push ()
  "Choose where to push, like Magit's `Push <branch> to'.
The menu appears at once, without which-key's idle delay, and shows
the push remote and the upstream from the last render.  The chosen
push runs in the background queue."
  (interactive)
  (let* ((branch (tuned-git-status--push-branch))
         (remote (tuned-git-status--push-remote branch))
         (push (and remote (concat remote "/" branch)))
         (upstream tuned-git-status--head-upstream)
         (map (tuned-git-status--push-menu push upstream)))
    (set-transient-map map nil #'tuned-git-status--hide-push-menu)
    (if (require 'which-key nil t)
        (which-key--show-keymap (format "Push %s to" branch) map nil nil t)
      (message "Push %s to: p %s, u %s, e elsewhere"
               branch (or push "push remote") (or upstream "upstream")))))


(defun tuned-git-status--hunk-line (section)
  "Return the file line that the hunk line at point corresponds to.
SECTION is the hunk section around point; the line number comes from
the new side of the hunk header, so context and added lines map onto
the file as it is on disk."
  (let ((here (line-beginning-position)))
    (save-excursion
      (goto-char (plist-get section :heading-start))
      (if (not (re-search-forward "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)"
                                  (line-end-position) t))
          1
        (let ((line (string-to-number (match-string 1))))
          (if (= here (line-beginning-position))
              line
            ;; Lines before point that exist on the new side advance the
            ;; number; removed lines do not.
            (goto-char (plist-get section :content-start))
            (while (< (line-beginning-position) here)
              (when (memq (char-after) '(?\s ?+))
                (setq line (1+ line)))
              (forward-line 1))
            line))))))

(defun tuned-git-status-visit ()
  "Visit the file or show the commit at point.
On a hunk, open its file at the line the hunk line belongs to."
  (interactive)
  (let ((section (tuned-git-status--section-at-point)))
    (if (memq (plist-get section :kind) '(file hunk))
        (let* ((file (plist-get section :path))
               (path (expand-file-name file default-directory))
               (line (if (eq (plist-get section :kind) 'hunk)
                         (tuned-git-status--hunk-line section)
                       1)))
          (unless (file-exists-p path)
            (user-error "File does not exist: %s" file))
          (find-file path)
          (goto-char (point-min))
          (forward-line (1- line))
          (when (get-buffer-window (current-buffer) t)
            (recenter)))
      (let ((commit (tuned-git-status--property 'tuned-git-status-commit)))
        (if commit
            (tuned-git-show-commit commit)
          (user-error "Nothing to visit at point"))))))

(defun tuned-git-status--property (property)
  "Return PROPERTY from the line at point."
  (or (get-text-property (point) property)
      (get-text-property (line-beginning-position) property)))


;; ============================================================
;; Rendering
;; ============================================================

(defun tuned-git-status--render (buffer log status log-code status-code diffs)
  "Render LOG, STATUS and DIFFS into BUFFER.
LOG-CODE and STATUS-CODE are the exit codes of the git commands."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (tuned-git-status--stop-spinner)
      (let ((inhibit-read-only t)
            ;; A refresh keeps the old sections until here, which is how
            ;; the first load is told apart from a silent refresh.
            (first (null tuned-git-status--sections))
            (entries (tuned-git-core-entries status))
            (branch (tuned-git-core-branch status))
            (subject (tuned-git-core-subject log)))
        (erase-buffer)
        (setq tuned-git-status--sections nil
              tuned-git-status--overlays nil
              tuned-git-status--head-branch
              (unless (member branch '("" "detached HEAD")) branch)
              tuned-git-status--head-upstream
              (tuned-git-core-upstream status))
        (insert (propertize "Head:" 'face 'bold)
                "     "
                (propertize branch 'face 'tuned-git-status-branch)
                (if (string= subject "") "" (concat " " subject))
                "\n\n")
        (tuned-git-status--insert-files "Untracked files" 'untracked
                                           (nth 2 entries) diffs)
        (tuned-git-status--insert-files "Unstaged changes" 'unstaged
                                           (nth 1 entries) diffs)
        (tuned-git-status--insert-files "Staged changes" 'staged
                                           (nth 0 entries) diffs)
        (when (and (null (nth 0 entries))
                   (null (nth 1 entries))
                   (null (nth 2 entries)))
          (insert (propertize "Working tree clean\n\n"
                              'face 'tuned-git-status-comment)))
        (let ((section (list :key "section:log"
                             :kind 'section
                             :status 'log
                             :heading-start (point))))
          (insert (propertize "Recent commits" 'face 'tuned-git-status-heading)
                  "\n")
          (plist-put section :content-start (point))
          (if (string= log "")
              (insert (propertize "No commits yet\n"
                                  'face 'tuned-git-status-comment))
            (dolist (line (split-string (string-trim-right log) "\n" t))
              (tuned-git-show-insert-commit line)))
          (plist-put section :content-end (point))
          (push section tuned-git-status--sections))
        (unless (and (zerop status-code) (zerop log-code))
          (insert (propertize
                   (format "\nGit exited with %d (status) / %d (log)\n"
                           status-code log-code)
                   'face 'error)))
        (tuned-git-status--apply-folds)
        (setq buffer-read-only t)
        (tuned-git-status--restore-point)
        (when first
          (message "Git status ready: %s" (buffer-name buffer)))))))


(defun tuned-git-status--diffs (unstaged staged)
  "Return a table of \"STATUS:PATH\" to diff from two whole-repo diffs.
UNSTAGED is `git diff' and STAGED `git diff --cached'."
  (let ((diffs (make-hash-table :test #'equal)))
    (dolist (part (list (cons "unstaged" unstaged) (cons "staged" staged)))
      (dolist (file (tuned-git-show-split-files (cdr part)))
        (puthash (concat (car part) ":" (car file)) (cdr file) diffs)))
    diffs))

(defun tuned-git-status--load (buffer root)
  "Load the log, status and diffs of ROOT into BUFFER.
Four git commands run in parallel, one per `tuned-async' slot, and the
buffer renders once all have answered.  A refresh starts them again in
the same slots, which cancels the older runs, so a stale answer never
reaches the buffer.  The diffs are one `git diff' for the work tree and
one for the index, not one per file: a status with 50 changed files
starts four processes instead of over fifty."
  (let ((results (make-hash-table :test #'eq))
        (pending 4))
    (with-current-buffer buffer
      (dolist (spec '((log "log" "--oneline" "--decorate" "--no-color" "-n" "10")
                      (status "status" "--porcelain=v1" "--branch" "-z")
                      (unstaged "diff" "--no-color" "--no-ext-diff")
                      (staged "diff" "--cached" "--no-color" "--no-ext-diff")))
        (let ((key (car spec)))
          (tuned-async-run
           :name (format "git-status-%s" key)
           :command (tuned-git-core-git-command (cdr spec))
           :directory root
           :slot (intern (format "load-%s" key))
           :on-done
           (lambda (output code)
             (puthash key (cons output code) results)
             (when (zerop (setq pending (1- pending)))
               (let ((log (gethash 'log results))
                     (status (gethash 'status results)))
                 (tuned-git-status--render
                  buffer (car log) (car status) (cdr log) (cdr status)
                  (tuned-git-status--diffs (car (gethash 'unstaged results))
                                        (car (gethash 'staged results)))))))))))))

;; ============================================================
;; Mode and entry point
;; ============================================================

(defvar-keymap tuned-git-status-mode-map
  :doc "Keymap for `tuned-git-status-mode'."
  :parent special-mode-map
  "TAB" #'tuned-git-status-toggle
  "<tab>" #'tuned-git-status-toggle
  "<backtab>" #'tuned-git-status-toggle-files
  "s" #'tuned-git-status-stage
  "u" #'tuned-git-status-unstage
  "X" #'tuned-git-status-discard
  "S" #'tuned-git-status-stage-all
  "U" #'tuned-git-status-unstage-all
  "P" #'tuned-git-status-push
  "p" #'tuned-git-status-push
  "f" #'tuned-git-status-fetch
  "F" #'tuned-git-status-pull
  "RET" #'tuned-git-status-visit
  "<return>" #'tuned-git-status-visit)

(define-derived-mode tuned-git-status-mode special-mode "Git-Status"
  "Major mode for `tuned-git-status' buffers."
  (require 'diff-mode)
  (unless (hash-table-p tuned-git-status--expanded)
    (setq tuned-git-status--expanded (make-hash-table :test #'equal)))
  ;; The loader slot sits at the left edge of the mode line, where a
  ;; narrow split window still shows it.
  (tuned-spinner-mode-line-install)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (tuned-git-status))))

(define-derived-mode tuned-git-show-mode special-mode "Git-Show"
  "Major mode for `tuned-git-show-commit' buffers."
  (require 'diff-mode))

(defun tuned-git-status--root ()
  "Return the expanded Git root for `default-directory', or nil.
Dired and some other buffers use an abbreviated directory such as
\"~/workspace/repo/\"; `git -C' does not expand \"~\"."
  (when-let* ((root (locate-dominating-file default-directory ".git")))
    (file-name-as-directory (expand-file-name root))))


;; ============================================================
;; Auto refresh
;; ============================================================
;; A save in an editor, or a commit made in a terminal, changes the status,
;; and a visible status buffer should show it without `g'.  file-notify
;; reports the changes.  A save writes several files at once, so the events
;; are debounced: one refresh, `tuned-git-status-refresh-delay' after the last.
;;
;; file-notify does not recurse into subdirectories on macOS or on Linux.
;; FSEvents reports a change in a subdirectory only as a change of that
;; subdirectory, and inotify needs a watch per directory.  So the status
;; watches the root, the .git directory, and every directory that holds a
;; tracked or untracked file, as `git ls-files' lists them.  A directory
;; created later is watched when its creation is seen.

(defcustom tuned-git-status-auto-refresh t
  "Non-nil refreshes a visible status buffer when the work tree changes."
  :type 'boolean
  :group 'tuned-git-status)

(defcustom tuned-git-status-refresh-delay 0.5
  "Seconds to wait after the last file change before refreshing the status."
  :type 'number
  :group 'tuned-git-status)

(defcustom tuned-git-status-max-watches 1000
  "Most directories the status watches; the rest go unwatched."
  :type 'integer
  :group 'tuned-git-status)

(defvar-local tuned-git-status--watches nil
  "file-notify descriptors this status buffer holds.")

(defvar-local tuned-git-status--watching nil
  "Non-nil once this status buffer's watches are set up or being set up.")

(defvar-local tuned-git-status--watch-timer nil
  "Timer of the refresh this status buffer waits to run, if any.")

(defconst tuned-git-status--git-state-files
  '("index" "HEAD" "ORIG_HEAD" "FETCH_HEAD" "packed-refs")
  "Files in .git whose change can change the status.")

(defun tuned-git-status--relevant-event-p (root file)
  "Return non-nil when a change to FILE, under ROOT, can change the status.
Lock files, editor backups and most of .git (objects, logs) do not."
  (let* ((rel (file-relative-name file root))
         (name (file-name-nondirectory rel)))
    (cond
     ((string-suffix-p ".lock" name) nil)
     ((or (string-prefix-p ".#" name)
          (string-prefix-p "#" name)
          (string-suffix-p "~" name))
      nil)
     ((string-prefix-p ".git/" rel)
      ;; Only the state files directly in .git, not logs/HEAD and the like.
      (or (and (string= (file-name-directory rel) ".git/")
               (member name tuned-git-status--git-state-files))
          (string-match-p "/refs/" rel)))
     (t t))))

(defun tuned-git-status--watch-dirs (root paths)
  "Return the directories under ROOT that hold PATHS, root first.
PATHS are relative to ROOT, as `git ls-files -z' prints them.  A directory
is included with each of its parents, so a new subdirectory is seen."
  (let ((dirs (make-hash-table :test #'equal))
        (order nil))
    (cl-flet ((add (dir)
                (unless (gethash dir dirs)
                  (puthash dir t dirs)
                  (push dir order))))
      (add (directory-file-name root))
      (dolist (path paths)
        (let ((dir (file-name-directory path)))
          (while dir
            (add (directory-file-name (expand-file-name dir root)))
            (setq dir (file-name-directory (directory-file-name dir)))))))
    (nreverse order)))

(defun tuned-git-status--watch-dir (buffer dir)
  "Watch DIR for BUFFER, a status buffer, unless the limit is reached."
  (when (and (< (length (buffer-local-value 'tuned-git-status--watches buffer))
                tuned-git-status-max-watches)
             (file-directory-p dir))
    (with-current-buffer buffer
      (push (file-notify-add-watch
             dir '(change)
             (lambda (event) (tuned-git-status--on-event buffer event)))
            tuned-git-status--watches))))

(defun tuned-git-status--on-event (buffer event)
  "Handle a file-notify EVENT for BUFFER, a status buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((root default-directory)
             (action (nth 1 event))
             (file (nth 2 event))
             (new (nth 3 event)))
        (when (and tuned-git-status-auto-refresh (stringp file))
          (when (and (eq action 'created)
                     (string-prefix-p root file)
                     (file-directory-p file))
            (tuned-git-status--watch-dir buffer file))
          (when (or (tuned-git-status--relevant-event-p root file)
                    (and (stringp new)
                         (tuned-git-status--relevant-event-p root new)))
            (tuned-git-status--schedule-refresh buffer)))))))

(defun tuned-git-status--schedule-refresh (buffer)
  "Refresh BUFFER after `tuned-git-status-refresh-delay', restarting the wait."
  (with-current-buffer buffer
    (when (timerp tuned-git-status--watch-timer)
      (cancel-timer tuned-git-status--watch-timer))
    (setq tuned-git-status--watch-timer
          (run-with-timer tuned-git-status-refresh-delay nil
                          #'tuned-git-status--auto-refresh buffer))))

(defun tuned-git-status--auto-refresh (buffer)
  "Refresh BUFFER if it is on screen and its repository queue is idle.
A queue that is busy refreshes the status itself when it drains."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq tuned-git-status--watch-timer nil)
      (when (and (get-buffer-window buffer t)
                 (not (tuned-async-queue-busy-p
                       (tuned-git-core-queue-key default-directory))))
        (tuned-git-status)))))

(defun tuned-git-status--scan-watch-dirs (buffer root)
  "List the tracked and untracked files of ROOT, then watch their directories."
  (tuned-async-run
   :name "tuned-git-watch-scan"
   :command '("git" "ls-files" "-z" "--cached" "--others" "--exclude-standard")
   :directory root
   :owner buffer
   :slot 'watch-scan
   :on-done (lambda (output code)
              (when (zerop code)
                (let ((dirs (tuned-git-status--watch-dirs
                             root (split-string output "\0" t))))
                  (dolist (dir dirs)
                    (tuned-git-status--watch-dir buffer dir)))))))

(defun tuned-git-status--ensure-watching (buffer root)
  "Start watching ROOT for BUFFER, once, when auto refresh is on."
  (when (and tuned-git-status-auto-refresh
             (not (buffer-local-value 'tuned-git-status--watching buffer))
             (fboundp (quote file-notify-add-watch)))
    (with-current-buffer buffer
      (setq tuned-git-status--watching t)
      (tuned-git-status--watch-dir buffer root)
      (tuned-git-status--watch-dir buffer (expand-file-name ".git" root))
      (add-hook 'kill-buffer-hook #'tuned-git-status--stop-watching nil t)
      (tuned-git-status--scan-watch-dirs buffer root))))

(defun tuned-git-status--stop-watching ()
  "Remove this status buffer's watches and pending refresh."
  (dolist (descriptor tuned-git-status--watches)
    (ignore-errors (file-notify-rm-watch descriptor)))
  (setq tuned-git-status--watches nil
        tuned-git-status--watching nil)
  (when (timerp tuned-git-status--watch-timer)
    (cancel-timer tuned-git-status--watch-timer))
  (setq tuned-git-status--watch-timer nil))

;;;###autoload
(defun tuned-git-status (&optional no-display)
  "Show the Git status and recent log of the current repository.
With NO-DISPLAY, update the status buffer without showing it in a window.
The buffer lists the head, untracked, unstaged and staged files
with their diff hunks, and the ten most recent commits.  An animated
ASCII spinner is shown while the git commands run.  `g' refreshes the
buffer and `q' quits it.  TAB folds or unfolds the
file or hunk at point, S-TAB toggles every file diff.  `s' stages
the hunk, file or section at point, `u' unstages it, `X' discards
it after confirmation, and `S'/`U' stage or unstage everything.
`p' opens the push menu at once: `p' pushes to the push remote, `u' to
the upstream and `e' elsewhere.  `RET' opens the file at the hunk line,
or shows the commit at point."
  (interactive)
  (let* ((root (tuned-git-status--root))
         (repo (and root (file-name-nondirectory (directory-file-name root))))
         (buffer (and root (get-buffer-create (format "*git-status: %s*" repo)))))
    (unless root
      (user-error "Not inside a Git repository"))
    (unless (executable-find "git")
      (user-error "Git is not installed"))
    (with-current-buffer buffer
      ;; Re-running the mode would kill the buffer-locals that keep the
      ;; fold state, the section list and the refresh position.
      (unless (derived-mode-p 'tuned-git-status-mode)
        (tuned-git-status-mode))
      (tuned-git-status--save-point)
      ;; Keep a rendered status on screen until its replacement is ready:
      ;; a refresh only says so in the mode line, which moves nothing.
      ;; Only a buffer with nothing rendered yet shows the loading line.
      (if tuned-git-status--sections
          (tuned-spinner-mode-line-start "Refreshing")
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq tuned-git-status--sections nil
              tuned-git-status--overlays nil
              buffer-read-only nil)
        (tuned-git-status--start-spinner))
      (setq default-directory root))
    (unless no-display
      (display-buffer buffer))
    (tuned-git-status--ensure-watching buffer root)
    (tuned-git-status--load buffer root)))

(require 'evil nil t) ; a macro below: loaded now, or it is compiled as a call
;; Evil's normal state would otherwise take TAB, s, u, X, S, U, p, P and RET first.
(with-eval-after-load 'evil
  (evil-define-key 'normal tuned-git-status-mode-map
    (kbd "TAB") #'tuned-git-status-toggle
    (kbd "<backtab>") #'tuned-git-status-toggle-files
    "s" #'tuned-git-status-stage
    "u" #'tuned-git-status-unstage
    "X" #'tuned-git-status-discard
    "x" #'tuned-git-status-discard
    "S" #'tuned-git-status-stage-all
    "U" #'tuned-git-status-unstage-all
    "P" #'tuned-git-status-push
    "p" #'tuned-git-status-push
    ;; Evil's `f' and `F' find a character; the status buffer has none.
    "f" #'tuned-git-status-fetch
    "F" #'tuned-git-status-pull
    (kbd "RET") #'tuned-git-status-visit
    (kbd "<return>") #'tuned-git-status-visit))

;; The status view is the one that knows how to refresh a repository
;; after a queued write; register it with the core.
(tuned-git-core-set-refresh-function #'tuned-git-status--queue-drained)

(provide 'tuned-git-status)
;;; tuned-git-status.el ends here
