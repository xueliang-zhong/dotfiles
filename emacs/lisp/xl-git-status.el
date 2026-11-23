;;; xl-git-status.el --- Async Git status buffer for Emacs  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, vc, convenience

;;; Commentary:
;; `xl-git-status' shows a Magit-like status without loading Magit:
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
;; The vim-fugitive style G* commands (Gstatus, Gblame, Gwrite, Gcommit)
;; live in xl-git-commands.el, which loads after this file.

;;; Code:

(require 'subr-x)    ; string-trim-right
(require 'seq)       ; seq-find, seq-remove
(require 'diff-mode) ; diff-added, diff-removed, diff-context, diff-hunk-header
(require 'xl-spinner) ; the shared loading indicator
(require 'xl-async)   ; owned, cancellable git processes and the job queue

(defgroup xl-git-status nil
  "Faces for `xl-git-status'."
  :group 'tools)

(defcustom xl-git-status-queue-delay 0.1
  "Seconds to wait before showing the operation queue's loader.
Local staging finishes before that, so the mode line does not
flash on every keypress; a push over the network is still
indicated."
  :type 'number
  :group 'xl-git-status)

(defface xl-git-status-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for Git status section headings."
  :group 'xl-git-status)

(defface xl-git-status-branch
  '((t :inherit font-lock-keyword-face))
  "Face for the current branch in a Git status buffer."
  :group 'xl-git-status)

(defface xl-git-status-file
  '((t :inherit font-lock-function-name-face))
  "Face for changed file names."
  :group 'xl-git-status)

(defface xl-git-status-status
  '((t :inherit font-lock-string-face))
  "Face for status labels such as \"modified\"."
  :group 'xl-git-status)

(defface xl-git-status-deleted
  '((t :inherit font-lock-warning-face))
  "Face for deleted and unmerged files."
  :group 'xl-git-status)

(defface xl-git-status-hash
  '((t :inherit font-lock-variable-name-face))
  "Face for commit hashes."
  :group 'xl-git-status)

(defface xl-git-status-ref
  '((t :inherit font-lock-string-face :box t))
  "Face for branch and tag decorations in the log."
  :group 'xl-git-status)

(defface xl-git-status-comment
  '((t :inherit font-lock-comment-face))
  "Face for status notes such as a clean working tree."
  :group 'xl-git-status)

(defface xl-git-status-hunk-heading
  '((t :inherit diff-hunk-header))
  "Face for diff hunk headings."
  :group 'xl-git-status)

;; The git processes belong to `xl-async': the four reads of a refresh run
;; in the slots `load-log', `load-status', `load-unstaged' and
;; `load-staged', the staging and push operations in the queue `queue',
;; and `git show' in the commit buffer's slot `show'.

(defvar-local xl-git-status--queue-timer nil
  "Timer that will show the queue loader if the operation is slow.")

(defvar-local xl-git-status--queue-action nil
  "What the queued operation is doing, for the mode line.")

(defvar-local xl-git-status--sections nil
  "Sections of this status buffer, as plists.
Each plist has :key, :kind (`section', `file' or `hunk'), :status
\(`untracked', `unstaged', `staged' or `log'), :heading-start,
:content-start and :content-end.  File sections add :path; hunk
sections add :patch and :hunk-text (the hunk without the diff
header, used to follow a hunk across sections); change sections
add :files.")

(defvar-local xl-git-status--overlays nil
  "Invisibility overlays that fold sections in this buffer.")

(defvar-local xl-git-status--expanded nil
  "Hash table of section keys the user unfolded, kept across refreshes.
Sections missing from it use their default: foldable headings and
hunks start unfolded, file diffs start folded.")

(defvar-local xl-git-status--restore-section nil
  "Section to return to after the next render, or nil.
It is the section at point when a refresh starts, with :heading-line
added (the line number before the buffer is emptied) and
:point-offset (the distance into the section when point was inside
it rather than on its heading).")

(defvar-local xl-git-status--head-branch nil
  "Branch of the last render, or nil for a detached HEAD.")

(defvar-local xl-git-status--head-upstream nil
  "Upstream ref of the last render, e.g. \"origin/master\", or nil.")

;; ============================================================
;; Loading indicator
;; ============================================================
;; Git runs asynchronously, so the buffer is briefly empty.  Show a small
;; ASCII spinner instead, replaced by the status as soon as it is ready.
;; The clock and the frames come from `xl-spinner'; the overlay below is
;; only the place the frames are drawn.
(defvar-local xl-git-status--spinner-overlay nil
  "Overlay drawing the loading indicator in this status buffer.")

(defun xl-git-status--draw-spinner (text)
  "Show TEXT in this status buffer's loader; nil removes it."
  (if (and text (overlayp xl-git-status--spinner-overlay))
      (overlay-put xl-git-status--spinner-overlay 'display text)
    (when (overlayp xl-git-status--spinner-overlay)
      (delete-overlay xl-git-status--spinner-overlay)
      (setq xl-git-status--spinner-overlay nil))))

(defun xl-git-status--stop-spinner ()
  "Remove the loading indicator of this status buffer."
  (xl-spinner-stop))

(defun xl-git-status--start-spinner ()
  "Show an animated loading line until the next render."
  (xl-git-status--stop-spinner)
  (let ((inhibit-read-only t))
    (insert (propertize "Git status" 'face 'xl-git-status-heading)
            "  ")
    (let ((start (point)))
      ;; The overlay's display string replaces these three spaces.
      (insert "   ")
      (setq xl-git-status--spinner-overlay (make-overlay start (point)))
      (overlay-put xl-git-status--spinner-overlay
                   'face 'xl-git-status-branch))
    (insert "  " (propertize "Loading..." 'face 'xl-git-status-comment)
            "\n\n"))
  (xl-spinner-start #'xl-git-status--draw-spinner))

;; ============================================================
;; Parsing
;; ============================================================

(defun xl-git-status--label (char)
  "Return a human-readable label for porcelain status CHAR."
  (pcase char
    (?M "modified")
    (?A "new file")
    (?D "deleted")
    (?R "renamed")
    (?C "copied")
    (?U "unmerged")
    (?? "untracked")
    (_ "changed")))

(defun xl-git-status--head-line (status)
  "Return the `## branch' line of porcelain STATUS output.
The output is NUL-separated (`-z'); a newline also ends the line."
  (car (split-string status "[\0\n]")))

(defun xl-git-status--branch (status)
  "Return the branch name from porcelain STATUS output."
  (let ((line (xl-git-status--head-line status)))
    (cond
     ((string-match "\\`## No commits yet on \\(.+\\)\\'" line)
      (match-string 1 line))
     ((string-match "\\`## HEAD (no branch)" line) "detached HEAD")
     ((string-match "\\`## \\([^ \n]+\\)" line)
      (let ((branch (match-string 1 line)))
        (if (string-match "\\`\\(.*\\)\\.\\.\\." branch)
            (match-string 1 branch)
          branch)))
     (t ""))))

(defun xl-git-status--upstream (status)
  "Return the upstream ref from porcelain STATUS output, or nil."
  (let ((line (xl-git-status--head-line status)))
    (when (string-match "\\`## [^ \n]+\\.\\.\\.\\([^ \n]+\\)" line)
      (match-string 1 line))))

(defun xl-git-status--subject (log)
  "Return the newest commit's subject from LOG output."
  (let ((line (car (split-string log "\n"))))
    (if (string-match "\\`[0-9a-f]+ \\(?:([^)]*) \\)?\\(.*\\)\\'" line)
        (match-string 1 line)
      "")))

(defun xl-git-status--entries (status)
  "Return (STAGED UNSTAGED UNTRACKED) file entries from STATUS output.
STATUS is `git status --porcelain=v1 --branch -z': NUL-separated
records with raw paths.  Without `-z' git wraps a path with a space
in quotes, which `git add' then takes literally."
  (let ((records (cdr (split-string status "\0" t)))
        staged unstaged untracked)
    (while records
      (let ((line (pop records)))
        (when (>= (length line) 3)
          (let ((x (aref line 0))
                (y (aref line 1))
                (path (substring line 3)))
            ;; A rename or copy is followed by a record with the old path;
            ;; act on the new one.
            (when (or (memq x '(?R ?C)) (memq y '(?R ?C)))
              (pop records))
            (if (and (eq x ??) (eq y ??))
                (push (cons "untracked" path) untracked)
              (unless (memq x '(?\s ?! ??))
                (push (cons (xl-git-status--label x) path) staged))
              (unless (memq y '(?\s ?! ??))
                (push (cons (xl-git-status--label y) path) unstaged)))))))
    (list (nreverse staged) (nreverse unstaged) (nreverse untracked))))

(defun xl-git-status--label-face (label)
  "Return the face for status LABEL."
  (pcase label
    ((or "deleted" "unmerged") 'xl-git-status-deleted)
    ((or "renamed" "copied") 'xl-git-status-file)
    (_ 'xl-git-status-status)))

;; ============================================================
;; Diff view
;; ============================================================

(defun xl-git-status--split-diff (text)
  "Split diff TEXT into its header and hunks.
Return (HEADER HUNKS): HEADER is the text before the first hunk and
HUNKS is a list of line lists, each starting with its `@@' line."
  (let (header hunk hunks)
    (dolist (line (split-string (string-trim-right text) "\n"))
      (cond
       ((string-prefix-p "@@" line)
        (when hunk (push (nreverse hunk) hunks))
        (setq hunk (list line)))
       (hunk (push line hunk))
       (t (push line header))))
    (when hunk (push (nreverse hunk) hunks))
    (list (mapconcat #'identity (nreverse header) "\n") (nreverse hunks))))

(defun xl-git-status--diff-path (lines)
  "Return the path a per-file diff, given as LINES, applies to, or nil.
A rename or copy names it on its `rename to'/`copy to' line.
Otherwise the `diff --git a/X b/X' line names the same X twice, which
holds even when X has spaces; `diff --cc X' names it once."
  (let ((first (car lines)))
    (or (seq-some (lambda (line)
                    (and (string-match "\\`\\(?:rename\\|copy\\) to \\(.*\\)\\'" line)
                         (match-string 1 line)))
                  (seq-take-while (lambda (line) (not (string-prefix-p "@@" line)))
                                  (cdr lines)))
        (and (string-prefix-p "diff --cc " first)
             (substring first 10))
        (let* ((rest (substring first (length "diff --git ")))
               (n (/ (- (length rest) 5) 2))
               (path (and (> n 0) (substring rest 2 (+ 2 n)))))
          (and path
               (string= rest (concat "a/" path " b/" path))
               path)))))

(defun xl-git-status--split-files (text)
  "Split the diff TEXT of many files into a list of (PATH . DIFF).
Each DIFF starts at its `diff --git' line, the same text `git diff --
PATH' prints for that file alone.  A header whose path cannot be read
\(a C-quoted name with control characters) is left out."
  (let (chunks lines)
    (dolist (line (split-string (string-trim-right text "\n+") "\n"))
      (when (string-match-p "\\`diff --\\(?:git\\|cc\\) " line)
        (when lines (push (nreverse lines) chunks))
        (setq lines nil))
      (when (or lines (string-match-p "\\`diff --\\(?:git\\|cc\\) " line))
        (push line lines)))
    (when lines (push (nreverse lines) chunks))
    (delq nil
          (mapcar (lambda (chunk)
                    (when-let* ((path (xl-git-status--diff-path chunk)))
                      (cons path (concat (mapconcat #'identity chunk "\n") "\n"))))
                  (nreverse chunks)))))

(defun xl-git-status--header-notes (header)
  "Return the informative lines of a diff HEADER.
The file name already appears on the section line, so drop the
`diff --git', `index', `---' and `+++' lines."
  (seq-remove (lambda (line)
                (or (string-empty-p line)
                    (string-match-p "\\`\\(?:diff --git \\|index \\|--- \\|\\+\\+\\+ \\)"
                                    line)))
              (split-string header "\n")))

(defun xl-git-status--diff-line-face (line)
  "Return the face for one LINE of a diff."
  (if (string-empty-p line)
      'diff-context
    (pcase (aref line 0)
      (?+ 'diff-added)
      (?- 'diff-removed)
      (_ 'diff-context))))

(defun xl-git-status--insert-diff (status path text)
  "Insert diff TEXT for PATH with STATUS as foldable hunk sections."
  (let* ((parts (xl-git-status--split-diff text))
         (header (car parts))
         (hunks (cadr parts)))
    (if (null hunks)
        ;; Binary files and mode changes have no hunks, only header lines.
        (dolist (line (xl-git-status--header-notes header))
          (insert (propertize (concat line "\n") 'face 'diff-header)))
      (let ((index 0))
        (dolist (hunk hunks)
          (setq index (1+ index))
          (let* ((patch (concat (if (string-empty-p header) "" (concat header "\n"))
                                (mapconcat #'identity hunk "\n") "\n"))
                 (section (list :key (format "hunk:%s:%s:%d" status path index)
                                :kind 'hunk
                                :status status
                                :path path
                                :patch patch
                                :hunk-text (mapconcat #'identity hunk "\n")
                                :heading-start (point))))
            (insert (propertize (concat (car hunk) "\n")
                                'face 'xl-git-status-hunk-heading))
            (plist-put section :content-start (point))
            (dolist (line (cdr hunk))
              (insert (propertize (concat line "\n")
                                  'face (xl-git-status--diff-line-face line))))
            (plist-put section :content-end (point))
            (push section xl-git-status--sections)))))))

(defun xl-git-status--insert-commit (line)
  "Insert one LINE from `git log --oneline --decorate' with faces."
  (if (not (string-match "\\`\\([0-9a-f]+\\) \\(?:(\\([^)]*\\)) \\)?\\(.*\\)\\'" line))
      (insert (concat line "\n"))
    ;; Capture the groups before `split-string' overwrites the match data.
    (let* ((hash (match-string 1 line))
           (refs-raw (match-string 2 line))
           (subject (or (match-string 3 line) ""))
           (refs (and refs-raw
                      (seq-remove (lambda (ref)
                                    (or (string-prefix-p "HEAD -> " ref)
                                        (string-prefix-p "origin/HEAD -> " ref)
                                        (string= ref "origin/HEAD")))
                                  (split-string refs-raw ", " t))))
           (text (concat
                  (propertize hash 'face 'xl-git-status-hash)
                  " "
                  (if refs
                      (concat (mapconcat (lambda (ref)
                                           (propertize ref 'face 'xl-git-status-ref))
                                         refs " ")
                              " ")
                    "")
                  subject "\n")))
      (insert (propertize text 'xl-git-status-commit hash)))))

;; ============================================================
;; Sections and folding
;; ============================================================

(defun xl-git-status--insert-file (status label path diff)
  "Insert a file line for PATH, then its DIFF when there is one.
STATUS is the section the file belongs to and LABEL its porcelain
status."
  (let ((section (list :key (format "file:%s:%s" status path)
                       :kind 'file
                       :status status
                       :path path
                       :heading-start (point))))
    (insert (propertize (format "%-10s " label)
                        'face (xl-git-status--label-face label))
            (propertize path 'face 'xl-git-status-file)
            "\n")
    (plist-put section :content-start (point))
    (when diff
      (xl-git-status--insert-diff status path diff))
    (plist-put section :content-end (point))
    (push section xl-git-status--sections)))

(defun xl-git-status--insert-files (title status files diffs)
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
                          'face 'xl-git-status-heading))
      (plist-put section :content-start (point))
      (dolist (entry files)
        (xl-git-status--insert-file
         status (car entry) (cdr entry)
         (gethash (concat (symbol-name status) ":" (cdr entry)) diffs)))
      (plist-put section :content-end (point))
      (push section xl-git-status--sections))
    (insert "\n")))

(defun xl-git-status--expanded-p (section)
  "Return non-nil when SECTION is unfolded."
  (let ((value (gethash (plist-get section :key)
                        xl-git-status--expanded 'unset)))
    (if (eq value 'unset)
        (memq (plist-get section :kind) '(section hunk))
      value)))

(defun xl-git-status--section-at-point ()
  "Return the innermost section that contains point, or nil."
  (let ((pos (point))
        best)
    (dolist (section xl-git-status--sections best)
      (let ((start (plist-get section :heading-start))
            (end (plist-get section :content-end)))
        (when (and start end (<= start pos) (< pos end))
          (when (or (null best)
                    (> start (plist-get best :heading-start)))
            (setq best section)))))))

(defun xl-git-status--apply-folds ()
  "Create invisibility overlays for the sections that are folded."
  (mapc #'delete-overlay xl-git-status--overlays)
  (setq xl-git-status--overlays nil)
  (dolist (section xl-git-status--sections)
    (let ((start (plist-get section :content-start))
          (end (plist-get section :content-end)))
      (when (and start end (< start end)
                 (not (xl-git-status--expanded-p section)))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'invisible t)
          (push overlay xl-git-status--overlays))))))

(defun xl-git-status--move-out-of-hidden ()
  "Move point to the buffer start when folding hid it."
  (when (invisible-p (point))
    (goto-char (point-min))))

(defun xl-git-status-toggle ()
  "Fold or unfold the section at point."
  (interactive)
  (let ((section (xl-git-status--section-at-point)))
    (unless section
      (user-error "No section at point"))
    (when (= (plist-get section :content-start)
             (plist-get section :content-end))
      (user-error "Nothing to fold here"))
    (let ((expanded (not (xl-git-status--expanded-p section))))
      (puthash (plist-get section :key) expanded xl-git-status--expanded)
      (xl-git-status--apply-folds)
      (unless expanded
        (goto-char (plist-get section :heading-start))))))

(defun xl-git-status-toggle-files ()
  "Fold every file diff, or unfold them all when any is folded."
  (interactive)
  (let ((files (seq-filter (lambda (section)
                             (and (eq (plist-get section :kind) 'file)
                                  (< (plist-get section :content-start)
                                     (plist-get section :content-end))))
                           xl-git-status--sections)))
    (unless files
      (user-error "No file changes to fold"))
    ;; Any folded diff means "show them all"; otherwise hide them all.
    (let ((expand (seq-some (lambda (section)
                              (not (xl-git-status--expanded-p section)))
                            files)))
      (dolist (section files)
        (puthash (plist-get section :key) expand xl-git-status--expanded)))
    (xl-git-status--apply-folds)
    (xl-git-status--move-out-of-hidden)))

(defun xl-git-status--save-point ()
  "Remember the section at point for the next render."
  (let* ((section (xl-git-status--section-at-point))
         (content (plist-get section :content-start)))
    (setq xl-git-status--restore-section
          (when section
            (append section
                    (list :heading-line
                          (line-number-at-pos
                           (plist-get section :heading-start))
                          :point-offset
                          (when (and content (>= (point) content))
                            (- (point) content))))))))

(defun xl-git-status--nearest-section (line)
  "Return the section whose heading line is closest to LINE."
  (let (best distance)
    (dolist (section xl-git-status--sections best)
      (let ((current (line-number-at-pos (plist-get section :heading-start))))
        (when (or (null best) (< (abs (- current line)) distance))
          (setq best section
                distance (abs (- current line))))))))

(defun xl-git-status--matching-section (old)
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
                      xl-git-status--sections)
          (seq-find (lambda (section)
                      (equal (plist-get section :key)
                             (plist-get old :key)))
                    xl-git-status--sections))
        (when (eq kind 'file)
          (seq-find (lambda (section)
                      (and (eq (plist-get section :kind) 'file)
                           (equal (plist-get section :path)
                                  (plist-get old :path))))
                    xl-git-status--sections))
        (xl-git-status--nearest-section (plist-get old :heading-line)))))

(defun xl-git-status--reveal (position)
  "Unfold the sections that hide POSITION, so point can go there."
  (let (changed)
    (dolist (section xl-git-status--sections)
      (when (and (plist-get section :content-start)
                 (<= (plist-get section :content-start) position)
                 (< position (plist-get section :content-end))
                 (not (xl-git-status--expanded-p section)))
        (puthash (plist-get section :key) t xl-git-status--expanded)
        (setq changed t)))
    (when changed
      (xl-git-status--apply-folds))))

(defun xl-git-status--restore-target (section old)
  "Return the position in SECTION that corresponds to OLD."
  (let ((offset (plist-get old :point-offset))
        (start (plist-get section :content-start)))
    (if (and offset start)
        ;; Same line inside the section, when the new content still has it.
        (min (+ start offset) (max start (1- (plist-get section :content-end))))
      (plist-get section :heading-start))))

(defun xl-git-status--restore-point ()
  "Move point back to the section marked for restore, else to the start."
  (let* ((old xl-git-status--restore-section)
         (section (and old (xl-git-status--matching-section old)))
         (target (if section
                     (xl-git-status--restore-target section old)
                   (point-min))))
    (setq xl-git-status--restore-section nil)
    ;; Staging a hunk moves it into a file diff that starts folded.
    (when (invisible-p target)
      (xl-git-status--reveal target))
    (goto-char target)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window target))))

;; ============================================================
;; Git processes
;; ============================================================

(defun xl-git-status--git-command (args)
  "Return the full git command line for ARGS."
  (append '("git" "--no-pager" "-c" "core.quotepath=false") args))

(defun xl-git-status--enqueue (job)
  "Queue JOB and start the queue if it is idle.
JOB is a plist with :root, :attempts (git argument lists, tried in
order), :action (the message on success), :progress (the mode line
label while it runs, defaulting to :action) and, for `git apply',
:patch (the text to apply, sent on standard input).  `xl-async' runs
one job at a time, so a rapid `s s s' stages in order instead of
racing, and the status refreshes once when the queue drains.  A
spinner in the mode line shows the queue only when it runs longer
than `xl-git-status-queue-delay', so fast local operations do not
flicker; it has no frame limit, because a push over the network may
take longer than a local git command."
  (let ((action (plist-get job :action))
        (progress (or (plist-get job :progress) (plist-get job :action))))
    (xl-async-enqueue
     'queue
     (list :directory (plist-get job :root)
           :attempts (mapcar #'xl-git-status--git-command
                             (plist-get job :attempts))
           :stdin (plist-get job :patch)
           :on-start (lambda () (setq xl-git-status--queue-action progress))
           :on-done (lambda (output code)
                      (if (zerop code)
                          (message "%s" action)
                        (message "%s failed: %s" action (string-trim output)))))
     #'xl-git-status--queue-drained))
  (unless (or (xl-spinner-running-p)
              (timerp xl-git-status--queue-timer))
    (setq xl-git-status--queue-timer
          (run-with-timer xl-git-status-queue-delay nil
                          #'xl-git-status--queue-spinner (current-buffer)))))

(defun xl-git-status--queue-spinner (buffer)
  "Show the queue loader in BUFFER if its operation is still running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq xl-git-status--queue-timer nil)
      (when (and (not (xl-spinner-running-p))
                 (xl-async-queue-busy-p 'queue))
        (xl-spinner-start #'xl-git-status--queue-indicator)))))

(defvar-local xl-git-status--mode-line nil
  "Loader text shown at the left of this status buffer's mode line.")

(defun xl-git-status--queue-indicator (text)
  "Show TEXT and the queue at the left of the mode line; nil clears.
The mode line is always present, so it does not move the buffer
contents.  It is drawn at the left edge rather than through
`mode-line-process', which sits after the mode name where a narrow
split window truncates it."
  (setq xl-git-status--mode-line
        (when text
          (format "%s %s%s "
                  text
                  (or xl-git-status--queue-action "git")
                  (let ((waiting (xl-async-queue-length 'queue)))
                    (if (> waiting 0) (format " +%d" waiting) "")))))
  (force-mode-line-update))

(defun xl-git-status--queue-drained ()
  "Refresh the status once the operation queue has run out of jobs."
  (when (timerp xl-git-status--queue-timer)
    (cancel-timer xl-git-status--queue-timer)
    (setq xl-git-status--queue-timer nil))
  ;; Keep the loader running under a new label until the render replaces
  ;; the old content, so the mode line never flickers.
  (setq xl-git-status--queue-action "Refreshing")
  (when (xl-spinner-running-p)
    (xl-git-status--queue-indicator (car xl-spinner-frames)))
  (xl-git-status))

(defun xl-git-status--queue-teardown ()
  "Stop this status buffer's loader timer; `xl-async' kills its processes."
  (when (timerp xl-git-status--queue-timer)
    (cancel-timer xl-git-status--queue-timer)
    (setq xl-git-status--queue-timer nil))
  (xl-spinner-stop)
  (setq xl-git-status--queue-action nil))

(defun xl-git-status--git-string (root &rest args)
  "Return the trimmed output of git ARGS run in ROOT, or nil.
A failing command, or one that prints nothing, returns nil.  This
runs synchronously and is only for the small config lookups the push
menu needs; the status, diff and log reads, and every write, stay
asynchronous."
  (with-temp-buffer
    (let ((default-directory root)
          (code (apply #'call-process "git" nil t nil
                       "--no-pager" "-c" "core.quotepath=false" args)))
      (when (zerop code)
        (let ((text (string-trim (buffer-string))))
          (unless (string-empty-p text) text))))))

;; ============================================================
;; Operations
;; ============================================================

(defun xl-git-status--section-files (section)
  "Return the file paths SECTION acts on."
  (pcase (plist-get section :kind)
    ('section (plist-get section :files))
    ((or 'file 'hunk) (list (plist-get section :path)))))

(defun xl-git-status--describe-files (files)
  "Return a short description of FILES for messages."
  (if (= (length files) 1)
      (car files)
    (format "%d files" (length files))))

(defun xl-git-status--unstage-attempts (files)
  "Return git argument lists that remove FILES from the index.
`git reset' needs HEAD; a repository without commits needs `git rm'."
  (list (append (list "reset" "-q" "HEAD" "--") files)
        (append (list "rm" "--cached" "-q" "-r" "--") files)))

(defun xl-git-status-stage ()
  "Stage the hunk, file or change section at point.
The work runs in the background; the status refreshes when it is done."
  (interactive)
  (let* ((section (xl-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'unstaged)
         (user-error "That hunk is already staged"))
       (xl-git-status--enqueue
        (list :root default-directory
              :attempts '(("apply" "--cached"))
              :patch (plist-get section :patch)
              :action "Staged hunk")))
      ((or 'file 'section)
       (unless (memq status '(unstaged untracked))
         (user-error "Nothing unstaged there"))
       (let ((files (xl-git-status--section-files section)))
         (xl-git-status--enqueue
          (list :root default-directory
                :attempts (list (append (list "add" "--") files))
                :action (format "Staged %s"
                                (xl-git-status--describe-files files))))))
      (_ (user-error "Nothing to stage at point")))))

(defun xl-git-status-unstage ()
  "Unstage the hunk, file or change section at point.
The work runs in the background; the status refreshes when it is done."
  (interactive)
  (let* ((section (xl-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'staged)
         (user-error "That hunk is not staged"))
       (xl-git-status--enqueue
        (list :root default-directory
              :attempts '(("apply" "--cached" "-R"))
              :patch (plist-get section :patch)
              :action "Unstaged hunk")))
      ((or 'file 'section)
       (unless (eq status 'staged)
         (user-error "Nothing staged there"))
       (let ((files (xl-git-status--section-files section)))
         (xl-git-status--enqueue
          (list :root default-directory
                :attempts (xl-git-status--unstage-attempts files)
                :action (format "Unstaged %s"
                                (xl-git-status--describe-files files))))))
      (_ (user-error "Nothing to unstage at point")))))

(defun xl-git-status--delete-untracked (file)
  "Delete untracked FILE, a directory or a plain file."
  (let ((path (expand-file-name file default-directory)))
    (if (file-directory-p path)
        (delete-directory path t)
      (delete-file path))))

(defun xl-git-status-discard ()
  "Discard the unstaged or untracked change at point, after confirmation.
The git work runs in the background; the status refreshes when it is done."
  (interactive)
  (let* ((section (xl-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'unstaged)
         (user-error "Unstage that hunk first"))
       (when (y-or-n-p "Discard this hunk? ")
         (xl-git-status--enqueue
          (list :root default-directory
                :attempts '(("apply" "-R"))
                :patch (plist-get section :patch)
                :action "Discarded hunk"))))
      ((or 'file 'section)
       (let ((files (xl-git-status--section-files section)))
         (pcase status
           ('unstaged
            (when (y-or-n-p (format "Discard changes in %s? "
                                       (xl-git-status--describe-files files)))
              (xl-git-status--enqueue
               (list :root default-directory
                     :attempts (list (append (list "checkout" "--") files))
                     :action (format "Discarded %s"
                                     (xl-git-status--describe-files files))))))
           ('untracked
            (when (y-or-n-p (format "Delete %s? "
                                       (xl-git-status--describe-files files)))
              (dolist (file files)
                (xl-git-status--delete-untracked file))
              (xl-git-status)
              (message "Deleted %s"
                       (xl-git-status--describe-files files))))
           (_ (user-error "Unstage that change first")))))
      (_ (user-error "Nothing to discard at point")))))

(defun xl-git-status-stage-all ()
  "Stage every change in the working tree, in the background."
  (interactive)
  (xl-git-status--enqueue
   (list :root default-directory
         :attempts '(("add" "--all"))
         :action "Staged all changes")))

(defun xl-git-status-unstage-all ()
  "Unstage everything in the index, in the background."
  (interactive)
  (xl-git-status--enqueue
   (list :root default-directory
         :attempts (xl-git-status--unstage-attempts '("."))
         :action "Unstaged all changes")))

(defun xl-git-status--push-branch ()
  "Return the checked-out branch, or signal an error when detached."
  (or xl-git-status--head-branch
      (user-error "No branch is checked out")))

(defun xl-git-status--upstream-parts ()
  "Return (REMOTE . BRANCH) for the current branch's upstream, or nil."
  (when-let* ((upstream xl-git-status--head-upstream)
              (slash (string-match "/" upstream)))
    (cons (substring upstream 0 slash)
          (substring upstream (1+ slash)))))

(defun xl-git-status--push-remote (branch)
  "Return the remote BRANCH pushes to, or nil when none is configured.
Git prefers `branch.<name>.pushRemote' over `remote.pushDefault'
over the remote of the upstream, and the menu offers the same."
  (or (xl-git-status--git-string
       default-directory "config" "--get"
       (format "branch.%s.pushRemote" branch))
      (xl-git-status--git-string default-directory "config" "--get"
                                 "remote.pushDefault")
      (car-safe (xl-git-status--upstream-parts))))

(defun xl-git-status--remotes ()
  "Return the names of this repository's remotes."
  (split-string (or (xl-git-status--git-string default-directory "remote") "")
                "\n" t))

(defun xl-git-status-push-to-pushremote ()
  "Push the current branch to its push remote, in the background.
This is `p' in the push menu, the `Push <branch> to' part of Magit's
`P p'.  The push joins the operation queue, so the mode line shows
a loader until git answers and the status refreshes once it is done."
  (interactive)
  (let* ((branch (xl-git-status--push-branch))
         (remote (xl-git-status--push-remote branch)))
    (xl-git-status--enqueue
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

(defun xl-git-status-push-to-upstream ()
  "Push the current branch to its upstream, in the background.
This is `u' in the push menu.  Without an upstream the push falls
back to `git push', whose error message carries git's own hint."
  (interactive)
  (let* ((branch (xl-git-status--push-branch))
         (parts (xl-git-status--upstream-parts)))
    (xl-git-status--enqueue
     (list :root default-directory
           :attempts (list
                      (if parts
                          (list "push" (car parts)
                                (format "refs/heads/%s:refs/heads/%s"
                                        branch (cdr parts)))
                        '("push")))
           :action (format "Pushed %s" branch)
           :progress (if xl-git-status--head-upstream
                         (format "Pushing to %s" xl-git-status--head-upstream)
                       "Pushing")))))

(defun xl-git-status-push-elsewhere ()
  "Push the current branch to a remote and branch chosen now.
This is `e' in the push menu, Magit's plain `Push elsewhere'."
  (interactive)
  (let* ((branch (xl-git-status--push-branch))
         (remotes (xl-git-status--remotes)))
    (unless remotes
      (user-error "No Git remotes"))
    (let* ((remote (completing-read (format "Push %s to remote: " branch)
                                    remotes nil t))
           (target (read-string (format "Remote branch (default %s): " branch)
                                nil nil branch)))
      (when (string-empty-p target)
        (user-error "No remote branch to push to"))
      (xl-git-status--enqueue
       (list :root default-directory
             :attempts (list
                        (list "push" remote
                              (format "refs/heads/%s:refs/heads/%s"
                                      branch target)))
             :action (format "Pushed %s to %s/%s" branch remote target)
             :progress (format "Pushing to %s/%s" remote target))))))

(defun xl-git-status--push-menu (push upstream)
  "Return a push destination keymap.
PUSH and UPSTREAM are the refs the menu shows next to `p' and `u'."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "p" (cons (format "push to %s" (or push "push remote"))
                              #'xl-git-status-push-to-pushremote))
    (keymap-set map "u" (cons (format "push to %s" (or upstream "upstream"))
                              #'xl-git-status-push-to-upstream))
    (keymap-set map "e" (cons "push elsewhere"
                              #'xl-git-status-push-elsewhere))
    map))

(defun xl-git-status--hide-push-menu ()
  "Hide what `xl-git-status-push' drew, when which-key drew it."
  (when (fboundp 'which-key--hide-popup)
    (which-key--hide-popup)))

(defun xl-git-status-push ()
  "Choose where to push, like Magit's `Push <branch> to'.
The menu appears at once, without which-key's idle delay, and shows
the push remote and the upstream from the last render.  The chosen
push runs in the background queue."
  (interactive)
  (let* ((branch (xl-git-status--push-branch))
         (remote (xl-git-status--push-remote branch))
         (push (and remote (concat remote "/" branch)))
         (upstream xl-git-status--head-upstream)
         (map (xl-git-status--push-menu push upstream)))
    (set-transient-map map nil #'xl-git-status--hide-push-menu)
    (if (require 'which-key nil t)
        (which-key--show-keymap (format "Push %s to" branch) map nil nil t)
      (message "Push %s to: p %s, u %s, e elsewhere"
               branch (or push "push remote") (or upstream "upstream")))))

(defun xl-git-status--show-line-face (line)
  "Return the face for one LINE of `git show' diff output."
  (cond
   ((string-prefix-p "@@" line) 'xl-git-status-hunk-heading)
   ((string-prefix-p "+++" line) 'diff-header)
   ((string-prefix-p "---" line) 'diff-header)
   ((string-prefix-p "+" line) 'diff-added)
   ((string-prefix-p "-" line) 'diff-removed)
   ((string-match-p
     "\\`\\(?:diff --\\(?:git\\|cc\\|combined\\) \\|index \\|new file mode \\|deleted file mode \\|old mode \\|new mode \\|similarity index \\|dissimilarity index \\|rename \\|copy \\|Binary files \\)"
     line)
    'diff-header)
   (t 'diff-context)))

(defun xl-git-status--insert-show-header-line (line)
  "Insert one LINE of a `git show' commit header with faces."
  (cond
   ((string-match "\\`commit \\([0-9a-f]+\\)\\'" line)
    (insert (propertize "commit " 'face 'xl-git-status-heading)
            (propertize (match-string 1 line) 'face 'xl-git-status-hash)
            "\n"))
   ((string-match "\\`\\(Author\\|Date\\|Merge\\|Committer\\): *\\(.*\\)\\'" line)
    (insert (propertize (concat (match-string 1 line) ":")
                        'face 'xl-git-status-heading)
            " " (match-string 2 line) "\n"))
   (t (insert line "\n"))))

(defun xl-git-status--insert-show (text)
  "Insert `git show' output TEXT, styled like the status buffer.
The commit header gets the status faces and every diff line the same
added, removed, context and hunk-heading faces as the status hunks."
  (let ((header t))
    (dolist (line (split-string (string-trim-right text) "\n"))
      (when (and header
                 (string-match-p "\\`diff --\\(?:git\\|cc\\|combined\\) " line))
        (setq header nil))
      (if header
          (xl-git-status--insert-show-header-line line)
        (insert (propertize (concat line "\n")
                            'face (xl-git-status--show-line-face line)))))))

;; ============================================================
;; Commit view
;; ============================================================
;; `git show' runs in the background: the buffer opens at once with the
;; commit hash and a loader, and is filled when the diff arrives.  A new
;; request for the same buffer (another commit, or the same hash again)
;; discards the older answer.
(defvar-local xl-git-status--show-spinner nil
  "Overlay drawing the loader in this commit buffer.")

(defun xl-git-status--show-draw (text)
  "Show TEXT in this commit buffer's loader; nil removes it."
  (if (and text (overlayp xl-git-status--show-spinner))
      (overlay-put xl-git-status--show-spinner 'display text)
    (when (overlayp xl-git-status--show-spinner)
      (delete-overlay xl-git-status--show-spinner)
      (setq xl-git-status--show-spinner nil))))

(defun xl-git-status--show-teardown ()
  "Stop this commit buffer's loader; `xl-async' kills its process."
  (xl-spinner-stop))

(defun xl-git-status--show-fetch (hash root)
  "Fill this commit buffer with `git show' HASH, run in ROOT.
The run takes the buffer's `show' slot, so a newer request cancels it."
  (xl-async-run
   :name "xl-git-show"
   :command (list "git" "--no-pager" "show" "--no-color" hash)
   :directory root
   :slot 'show
   :on-done (lambda (text code)
              (xl-spinner-stop)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (if (zerop code)
                    (xl-git-status--insert-show text)
                  (insert (propertize "git show failed: " 'face 'error)
                          (string-trim text) "\n")))
              (goto-char (point-min))
              (dolist (window (get-buffer-window-list (current-buffer) nil t))
                (set-window-point window (point-min))))))

(defun xl-git-status--show-commit (hash)
  "Show commit HASH in a read-only buffer, with the status buffer's faces.
The buffer opens at once and fills when `git show' answers."
  (let* ((root default-directory)
         (buffer (get-buffer-create (format "*git-show: %s*" hash))))
    (with-current-buffer buffer
      ;; The mode resets buffer-locals, but not `xl-async''s ownership:
      ;; the fetch below still replaces an older one in the `show' slot.
      (xl-git-show-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "commit " 'face 'xl-git-status-heading)
                (propertize hash 'face 'xl-git-status-hash) "\n")
        (let ((start (point)))
          (insert "   ")
          (setq xl-git-status--show-spinner (make-overlay start (point)))
          (overlay-put xl-git-status--show-spinner
                       'face 'xl-git-status-branch))
        (insert "  " (propertize "Loading..." 'face 'xl-git-status-comment)
                "\n"))
      (xl-spinner-start #'xl-git-status--show-draw :limit 1200)
      (add-hook 'kill-buffer-hook #'xl-git-status--show-teardown nil t)
      (xl-git-status--show-fetch hash root)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun xl-git-status--hunk-line (section)
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

(defun xl-git-status-visit ()
  "Visit the file or show the commit at point.
On a hunk, open its file at the line the hunk line belongs to."
  (interactive)
  (let ((section (xl-git-status--section-at-point)))
    (if (memq (plist-get section :kind) '(file hunk))
        (let* ((file (plist-get section :path))
               (path (expand-file-name file default-directory))
               (line (if (eq (plist-get section :kind) 'hunk)
                         (xl-git-status--hunk-line section)
                       1)))
          (unless (file-exists-p path)
            (user-error "File does not exist: %s" file))
          (find-file path)
          (goto-char (point-min))
          (forward-line (1- line))
          (when (get-buffer-window (current-buffer) t)
            (recenter)))
      (let ((commit (xl-git-status--property 'xl-git-status-commit)))
        (if commit
            (xl-git-status--show-commit commit)
          (user-error "Nothing to visit at point"))))))

(defun xl-git-status--property (property)
  "Return PROPERTY from the line at point."
  (or (get-text-property (point) property)
      (get-text-property (line-beginning-position) property)))

;; ============================================================
;; Rendering
;; ============================================================

(defun xl-git-status--render (buffer log status log-code status-code diffs)
  "Render LOG, STATUS and DIFFS into BUFFER.
LOG-CODE and STATUS-CODE are the exit codes of the git commands."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (xl-git-status--stop-spinner)
      (let ((inhibit-read-only t)
            ;; A refresh keeps the old sections until here, which is how
            ;; the first load is told apart from a silent refresh.
            (first (null xl-git-status--sections))
            (entries (xl-git-status--entries status))
            (branch (xl-git-status--branch status))
            (subject (xl-git-status--subject log)))
        (erase-buffer)
        (setq xl-git-status--sections nil
              xl-git-status--overlays nil
              xl-git-status--head-branch
              (unless (member branch '("" "detached HEAD")) branch)
              xl-git-status--head-upstream
              (xl-git-status--upstream status))
        (insert (propertize "Head:" 'face 'bold)
                "     "
                (propertize branch 'face 'xl-git-status-branch)
                (if (string= subject "") "" (concat " " subject))
                "\n\n")
        (xl-git-status--insert-files "Untracked files" 'untracked
                                           (nth 2 entries) diffs)
        (xl-git-status--insert-files "Unstaged changes" 'unstaged
                                           (nth 1 entries) diffs)
        (xl-git-status--insert-files "Staged changes" 'staged
                                           (nth 0 entries) diffs)
        (when (and (null (nth 0 entries))
                   (null (nth 1 entries))
                   (null (nth 2 entries)))
          (insert (propertize "Working tree clean\n\n"
                              'face 'xl-git-status-comment)))
        (let ((section (list :key "section:log"
                             :kind 'section
                             :status 'log
                             :heading-start (point))))
          (insert (propertize "Recent commits" 'face 'xl-git-status-heading)
                  "\n")
          (plist-put section :content-start (point))
          (if (string= log "")
              (insert (propertize "No commits yet\n"
                                  'face 'xl-git-status-comment))
            (dolist (line (split-string (string-trim-right log) "\n" t))
              (xl-git-status--insert-commit line)))
          (plist-put section :content-end (point))
          (push section xl-git-status--sections))
        (unless (and (zerop status-code) (zerop log-code))
          (insert (propertize
                   (format "\nGit exited with %d (status) / %d (log)\n"
                           status-code log-code)
                   'face 'error)))
        (xl-git-status--apply-folds)
        (setq buffer-read-only t)
        (xl-git-status--restore-point)
        (when first
          (message "Git status ready: %s" (buffer-name buffer)))))))

(defun xl-git-status--diffs (unstaged staged)
  "Return a table of \"STATUS:PATH\" to diff from two whole-repo diffs.
UNSTAGED is `git diff' and STAGED `git diff --cached'."
  (let ((diffs (make-hash-table :test #'equal)))
    (dolist (part (list (cons "unstaged" unstaged) (cons "staged" staged)))
      (dolist (file (xl-git-status--split-files (cdr part)))
        (puthash (concat (car part) ":" (car file)) (cdr file) diffs)))
    diffs))

(defun xl-git-status--load (buffer root)
  "Load the log, status and diffs of ROOT into BUFFER.
Four git commands run in parallel, one per `xl-async' slot, and the
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
          (xl-async-run
           :name (format "git-status-%s" key)
           :command (xl-git-status--git-command (cdr spec))
           :directory root
           :slot (intern (format "load-%s" key))
           :on-done
           (lambda (output code)
             (puthash key (cons output code) results)
             (when (zerop (setq pending (1- pending)))
               (let ((log (gethash 'log results))
                     (status (gethash 'status results)))
                 (xl-git-status--render
                  buffer (car log) (car status) (cdr log) (cdr status)
                  (xl-git-status--diffs (car (gethash 'unstaged results))
                                        (car (gethash 'staged results)))))))))))))

;; ============================================================
;; Mode and entry point
;; ============================================================

(defvar-keymap xl-git-status-mode-map
  :doc "Keymap for `xl-git-status-mode'."
  :parent special-mode-map
  "TAB" #'xl-git-status-toggle
  "<tab>" #'xl-git-status-toggle
  "<backtab>" #'xl-git-status-toggle-files
  "s" #'xl-git-status-stage
  "u" #'xl-git-status-unstage
  "X" #'xl-git-status-discard
  "S" #'xl-git-status-stage-all
  "U" #'xl-git-status-unstage-all
  "P" #'xl-git-status-push
  "p" #'xl-git-status-push
  "RET" #'xl-git-status-visit
  "<return>" #'xl-git-status-visit)

(define-derived-mode xl-git-status-mode special-mode "Git-Status"
  "Major mode for `xl-git-status' buffers."
  (unless (hash-table-p xl-git-status--expanded)
    (setq xl-git-status--expanded (make-hash-table :test #'equal)))
  ;; Keep the queue loader at the left edge of the mode line, where a
  ;; narrow split window still shows it.
  (setq-local mode-line-format
              (cons '(:eval xl-git-status--mode-line)
                    (default-value 'mode-line-format)))
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (xl-git-status))))

(define-derived-mode xl-git-show-mode special-mode "Git-Show"
  "Major mode for `xl-git-status--show-commit' buffers.")

(defun xl-git-status--root ()
  "Return the expanded Git root for `default-directory', or nil.
Dired and some other buffers use an abbreviated directory such as
\"~/workspace/repo/\"; `git -C' does not expand \"~\"."
  (when-let* ((root (locate-dominating-file default-directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(defun xl-git-status ()
  "Show the Git status and recent log of the current repository.
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
  (let* ((root (xl-git-status--root))
         (repo (and root (file-name-nondirectory (directory-file-name root))))
         (buffer (and root (get-buffer-create (format "*git-status: %s*" repo)))))
    (unless root
      (user-error "Not inside a Git repository"))
    (unless (executable-find "git")
      (user-error "Git is not installed"))
    (with-current-buffer buffer
      ;; Re-running the mode would kill the buffer-locals that keep the
      ;; fold state, the section list and the refresh position.
      (unless (derived-mode-p 'xl-git-status-mode)
        (xl-git-status-mode))
      (add-hook 'kill-buffer-hook #'xl-git-status--queue-teardown nil t)
      (xl-git-status--save-point)
      ;; Keep a rendered status on screen until its replacement is ready.
      ;; Only a buffer with nothing rendered yet shows the loading line.
      (unless xl-git-status--sections
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq xl-git-status--sections nil
              xl-git-status--overlays nil
              buffer-read-only nil)
        (xl-git-status--start-spinner))
      (setq default-directory root))
    (display-buffer buffer)
    (xl-git-status--load buffer root)))

;; Evil's normal state would otherwise take TAB, s, u, X, S, U, p, P and RET first.
(with-eval-after-load 'evil
  (evil-define-key 'normal xl-git-status-mode-map
    (kbd "TAB") #'xl-git-status-toggle
    (kbd "<backtab>") #'xl-git-status-toggle-files
    "s" #'xl-git-status-stage
    "u" #'xl-git-status-unstage
    "X" #'xl-git-status-discard
    "x" #'xl-git-status-discard
    "S" #'xl-git-status-stage-all
    "U" #'xl-git-status-unstage-all
    "P" #'xl-git-status-push
    "p" #'xl-git-status-push
    (kbd "RET") #'xl-git-status-visit
    (kbd "<return>") #'xl-git-status-visit))

(provide 'xl-git-status)
;;; xl-git-status.el ends here
