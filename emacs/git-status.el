;;; git-status.el --- A small asynchronous Git status buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `xueliang-git-status' shows a Magit-like status without loading Magit:
;; the head, untracked/unstaged/staged files with their diff hunks, and
;; recent commits.  The git commands run asynchronously; the buffer is
;; read-only, `g' refreshes it and `q' quits it.  Sections fold like
;; Magit's (TAB toggles the file or hunk at point, S-TAB toggles every
;; file diff).  On a hunk, file or section line `s' stages and `u'
;; unstages it, `X' discards it after confirmation, and `RET' visits the
;; file at the hunk line or shows the commit at point in a buffer that
;; uses the same faces.
;;
;; The same module provides a GBlame side window (`xueliang-gblame',
;; also `:Gblame') and the vim-fugitive style `Gwrite' and `Gcommit'
;; commands.

;;; Code:

(require 'subr-x)    ; string-trim-right
(require 'seq)       ; seq-find, seq-remove
(require 'diff-mode) ; diff-added, diff-removed, diff-context, diff-hunk-header

(defgroup xueliang-git-status nil
  "Faces for `xueliang-git-status'."
  :group 'tools)

(defface xueliang-git-status-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for Git status section headings."
  :group 'xueliang-git-status)

(defface xueliang-git-status-branch
  '((t :inherit font-lock-keyword-face))
  "Face for the current branch in a Git status buffer."
  :group 'xueliang-git-status)

(defface xueliang-git-status-file
  '((t :inherit font-lock-function-name-face))
  "Face for changed file names."
  :group 'xueliang-git-status)

(defface xueliang-git-status-status
  '((t :inherit font-lock-string-face))
  "Face for status labels such as \"modified\"."
  :group 'xueliang-git-status)

(defface xueliang-git-status-deleted
  '((t :inherit font-lock-warning-face))
  "Face for deleted and unmerged files."
  :group 'xueliang-git-status)

(defface xueliang-git-status-hash
  '((t :inherit font-lock-variable-name-face))
  "Face for commit hashes."
  :group 'xueliang-git-status)

(defface xueliang-git-status-ref
  '((t :inherit font-lock-string-face :box t))
  "Face for branch and tag decorations in the log."
  :group 'xueliang-git-status)

(defface xueliang-git-status-comment
  '((t :inherit font-lock-comment-face))
  "Face for status notes such as a clean working tree."
  :group 'xueliang-git-status)

(defface xueliang-git-status-hunk-heading
  '((t :inherit diff-hunk-header))
  "Face for diff hunk headings."
  :group 'xueliang-git-status)

(defvar-local xueliang-git-status--processes nil
  "Git processes still running for this status buffer.")

(defvar-local xueliang-git-status--sections nil
  "Sections of this status buffer, as plists.
Each plist has :key, :kind (`section', `file' or `hunk'), :status
\(`untracked', `unstaged', `staged' or `log'), :heading-start,
:content-start and :content-end.  File sections add :path; hunk
sections add :patch; change sections add :files.")

(defvar-local xueliang-git-status--overlays nil
  "Invisibility overlays that fold sections in this buffer.")

(defvar-local xueliang-git-status--expanded nil
  "Hash table of section keys the user unfolded, kept across refreshes.
Sections missing from it use their default: foldable headings and
hunks start unfolded, file diffs start folded.")

(defvar-local xueliang-git-status--restore-key nil
  "Section key to return to after the next render.")

;; ============================================================
;; Parsing
;; ============================================================

(defun xueliang-git-status--label (char)
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

(defun xueliang-git-status--branch (status)
  "Return the branch name from porcelain STATUS output."
  (let ((line (car (split-string status "\n"))))
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

(defun xueliang-git-status--subject (log)
  "Return the newest commit's subject from LOG output."
  (let ((line (car (split-string log "\n"))))
    (if (string-match "\\`[0-9a-f]+ \\(?:([^)]*) \\)?\\(.*\\)\\'" line)
        (match-string 1 line)
      "")))

(defun xueliang-git-status--entries (status)
  "Return (STAGED UNSTAGED UNTRACKED) file entries from STATUS output."
  (let (staged unstaged untracked)
    (dolist (line (cdr (split-string status "\n" t)))
      (when (>= (length line) 3)
        (let* ((x (aref line 0))
               (y (aref line 1))
               (raw (substring line 3))
               ;; For a rename/copy the porcelain line is "old -> new";
               ;; act on the new path.
               (path (if (string-match " -> " raw)
                         (substring raw (match-end 0))
                       raw)))
          (if (and (eq x ??) (eq y ??))
              (push (cons "untracked" path) untracked)
            (unless (memq x '(?\s ?! ??))
              (push (cons (xueliang-git-status--label x) path) staged))
            (unless (memq y '(?\s ?! ??))
              (push (cons (xueliang-git-status--label y) path) unstaged))))))
    (list (nreverse staged) (nreverse unstaged) (nreverse untracked))))

(defun xueliang-git-status--label-face (label)
  "Return the face for status LABEL."
  (pcase label
    ((or "deleted" "unmerged") 'xueliang-git-status-deleted)
    ((or "renamed" "copied") 'xueliang-git-status-file)
    (_ 'xueliang-git-status-status)))

;; ============================================================
;; Diff view
;; ============================================================

(defun xueliang-git-status--split-diff (text)
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

(defun xueliang-git-status--header-notes (header)
  "Return the informative lines of a diff HEADER.
The file name already appears on the section line, so drop the
`diff --git', `index', `---' and `+++' lines."
  (seq-remove (lambda (line)
                (or (string-empty-p line)
                    (string-match-p "\\`\\(?:diff --git \\|index \\|--- \\|\\+\\+\\+ \\)"
                                    line)))
              (split-string header "\n")))

(defun xueliang-git-status--diff-line-face (line)
  "Return the face for one LINE of a diff."
  (if (string-empty-p line)
      'diff-context
    (pcase (aref line 0)
      (?+ 'diff-added)
      (?- 'diff-removed)
      (_ 'diff-context))))

(defun xueliang-git-status--insert-diff (status path text)
  "Insert diff TEXT for PATH with STATUS as foldable hunk sections."
  (let* ((parts (xueliang-git-status--split-diff text))
         (header (car parts))
         (hunks (cadr parts)))
    (if (null hunks)
        ;; Binary files and mode changes have no hunks, only header lines.
        (dolist (line (xueliang-git-status--header-notes header))
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
                                :heading-start (point))))
            (insert (propertize (concat (car hunk) "\n")
                                'face 'xueliang-git-status-hunk-heading))
            (plist-put section :content-start (point))
            (dolist (line (cdr hunk))
              (insert (propertize (concat line "\n")
                                  'face (xueliang-git-status--diff-line-face line))))
            (plist-put section :content-end (point))
            (push section xueliang-git-status--sections)))))))

(defun xueliang-git-status--insert-commit (line)
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
                  (propertize hash 'face 'xueliang-git-status-hash)
                  " "
                  (if refs
                      (concat (mapconcat (lambda (ref)
                                           (propertize ref 'face 'xueliang-git-status-ref))
                                         refs " ")
                              " ")
                    "")
                  subject "\n")))
      (insert (propertize text 'xueliang-git-status-commit hash)))))

;; ============================================================
;; Sections and folding
;; ============================================================

(defun xueliang-git-status--insert-file (status label path diff)
  "Insert a file line for PATH, then its DIFF when there is one.
STATUS is the section the file belongs to and LABEL its porcelain
status."
  (let ((section (list :key (format "file:%s:%s" status path)
                       :kind 'file
                       :status status
                       :path path
                       :heading-start (point))))
    (insert (propertize (format "%-10s " label)
                        'face (xueliang-git-status--label-face label))
            (propertize path 'face 'xueliang-git-status-file)
            "\n")
    (plist-put section :content-start (point))
    (when diff
      (xueliang-git-status--insert-diff status path diff))
    (plist-put section :content-end (point))
    (push section xueliang-git-status--sections)))

(defun xueliang-git-status--insert-files (title status files diffs)
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
                          'face 'xueliang-git-status-heading))
      (plist-put section :content-start (point))
      (dolist (entry files)
        (xueliang-git-status--insert-file
         status (car entry) (cdr entry)
         (gethash (concat (symbol-name status) ":" (cdr entry)) diffs)))
      (plist-put section :content-end (point))
      (push section xueliang-git-status--sections))
    (insert "\n")))

(defun xueliang-git-status--expanded-p (section)
  "Return non-nil when SECTION is unfolded."
  (let ((value (gethash (plist-get section :key)
                        xueliang-git-status--expanded 'unset)))
    (if (eq value 'unset)
        (memq (plist-get section :kind) '(section hunk))
      value)))

(defun xueliang-git-status--section-at-point ()
  "Return the innermost section that contains point, or nil."
  (let ((pos (point))
        best)
    (dolist (section xueliang-git-status--sections best)
      (let ((start (plist-get section :heading-start))
            (end (plist-get section :content-end)))
        (when (and start end (<= start pos) (< pos end))
          (when (or (null best)
                    (> start (plist-get best :heading-start)))
            (setq best section)))))))

(defun xueliang-git-status--apply-folds ()
  "Create invisibility overlays for the sections that are folded."
  (mapc #'delete-overlay xueliang-git-status--overlays)
  (setq xueliang-git-status--overlays nil)
  (dolist (section xueliang-git-status--sections)
    (let ((start (plist-get section :content-start))
          (end (plist-get section :content-end)))
      (when (and start end (< start end)
                 (not (xueliang-git-status--expanded-p section)))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'invisible t)
          (push overlay xueliang-git-status--overlays))))))

(defun xueliang-git-status--move-out-of-hidden ()
  "Move point to the buffer start when folding hid it."
  (when (invisible-p (point))
    (goto-char (point-min))))

(defun xueliang-git-status-toggle ()
  "Fold or unfold the section at point."
  (interactive)
  (let ((section (xueliang-git-status--section-at-point)))
    (unless section
      (user-error "No section at point"))
    (when (= (plist-get section :content-start)
             (plist-get section :content-end))
      (user-error "Nothing to fold here"))
    (let ((expanded (not (xueliang-git-status--expanded-p section))))
      (puthash (plist-get section :key) expanded xueliang-git-status--expanded)
      (xueliang-git-status--apply-folds)
      (unless expanded
        (goto-char (plist-get section :heading-start))))))

(defun xueliang-git-status-toggle-files ()
  "Fold every file diff, or unfold them all when any is folded."
  (interactive)
  (let ((files (seq-filter (lambda (section)
                             (and (eq (plist-get section :kind) 'file)
                                  (< (plist-get section :content-start)
                                     (plist-get section :content-end))))
                           xueliang-git-status--sections)))
    (unless files
      (user-error "No file changes to fold"))
    ;; Any folded diff means "show them all"; otherwise hide them all.
    (let ((expand (seq-some (lambda (section)
                              (not (xueliang-git-status--expanded-p section)))
                            files)))
      (dolist (section files)
        (puthash (plist-get section :key) expand xueliang-git-status--expanded)))
    (xueliang-git-status--apply-folds)
    (xueliang-git-status--move-out-of-hidden)))

(defun xueliang-git-status--restore-point ()
  "Move point back to the section marked for restore, else to the start."
  (let* ((key xueliang-git-status--restore-key)
         (section (and key
                       (seq-find (lambda (section)
                                   (equal key (plist-get section :key)))
                                 xueliang-git-status--sections)))
         (target (if section (plist-get section :heading-start) (point-min))))
    (setq xueliang-git-status--restore-key nil)
    (goto-char target)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window target))))

;; ============================================================
;; Git processes
;; ============================================================

(defun xueliang-git-status--git-sync (&rest args)
  "Run git ARGS in the current status buffer's repository.
Return a cons (EXIT-CODE . OUTPUT)."
  (let ((out (generate-new-buffer " *git-status-command*")))
    (unwind-protect
        (cons (apply #'call-process "git" nil out nil
                     "-C" default-directory "--no-pager" args)
              (with-current-buffer out (buffer-string)))
      (kill-buffer out))))

(defun xueliang-git-status--apply-patch (patch &rest args)
  "Feed PATCH to `git apply' with ARGS.
Return a cons (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (insert patch)
    (let ((out (generate-new-buffer " *git-status-apply*")))
      (unwind-protect
          (cons (apply #'call-process-region
                       (point-min) (point-max)
                       "git" nil out nil
                       "-C" default-directory "--no-pager"
                       "apply" (append args (list "-")))
                (with-current-buffer out (buffer-string)))
        (kill-buffer out)))))

(defun xueliang-git-status--finish (result action)
  "Refresh after RESULT succeeded, else report ACTION with its output."
  (if (zerop (car result))
      (progn
        (xueliang-git-status)
        (message "%s" action))
    (user-error "%s failed: %s" action
                (string-trim (cdr result)))))

(defun xueliang-git-status--git (buffer name root args callback)
  "Run git ARGS in ROOT, then call CALLBACK with output and exit code.
BUFFER owns the process; NAME names it."
  (let* ((out (generate-new-buffer (format " *%s*" name)))
         (proc
          (let ((default-directory root))
            (make-process
             :name name
             :buffer out
             :command (append '("git" "--no-pager" "-c" "core.quotepath=false")
                              args)
             :connection-type 'pipe
             :noquery t
             :sentinel
             (lambda (proc _event)
               (if (not (eq (process-status proc) 'exit))
                   (when (buffer-live-p out) (kill-buffer out))
                 (let ((output (with-current-buffer out (buffer-string)))
                       (code (process-exit-status proc)))
                   (when (buffer-live-p out) (kill-buffer out))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq xueliang-git-status--processes
                             (delq proc xueliang-git-status--processes)))
                     (funcall callback output code)))))))))
    (with-current-buffer buffer
      (setq xueliang-git-status--processes
            (cons proc xueliang-git-status--processes)))))

;; ============================================================
;; Operations
;; ============================================================

(defun xueliang-git-status--section-files (section)
  "Return the file paths SECTION acts on."
  (pcase (plist-get section :kind)
    ('section (plist-get section :files))
    ((or 'file 'hunk) (list (plist-get section :path)))))

(defun xueliang-git-status--describe-files (files)
  "Return a short description of FILES for messages."
  (if (= (length files) 1)
      (car files)
    (format "%d files" (length files))))

(defun xueliang-git-status--unstage-files (files)
  "Remove FILES from the index, coping with a repository without HEAD."
  (let ((result (apply #'xueliang-git-status--git-sync
                       "reset" "-q" "HEAD" "--" files)))
    (unless (zerop (car result))
      (setq result (apply #'xueliang-git-status--git-sync
                          "rm" "--cached" "-q" "-r" "--" files)))
    result))

(defun xueliang-git-status-stage ()
  "Stage the hunk, file or change section at point."
  (interactive)
  (let* ((section (xueliang-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'unstaged)
         (user-error "That hunk is already staged"))
       (xueliang-git-status--finish
        (xueliang-git-status--apply-patch (plist-get section :patch) "--cached")
        "Staged hunk"))
      ((or 'file 'section)
       (unless (memq status '(unstaged untracked))
         (user-error "Nothing unstaged there"))
       (let ((files (xueliang-git-status--section-files section)))
         (xueliang-git-status--finish
          (apply #'xueliang-git-status--git-sync "add" "--" files)
          (format "Staged %s" (xueliang-git-status--describe-files files)))))
      (_ (user-error "Nothing to stage at point")))))

(defun xueliang-git-status-unstage ()
  "Unstage the hunk, file or change section at point."
  (interactive)
  (let* ((section (xueliang-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'staged)
         (user-error "That hunk is not staged"))
       (xueliang-git-status--finish
        (xueliang-git-status--apply-patch (plist-get section :patch) "--cached" "-R")
        "Unstaged hunk"))
      ((or 'file 'section)
       (unless (eq status 'staged)
         (user-error "Nothing staged there"))
       (let ((files (xueliang-git-status--section-files section)))
         (xueliang-git-status--finish
          (xueliang-git-status--unstage-files files)
          (format "Unstaged %s" (xueliang-git-status--describe-files files)))))
      (_ (user-error "Nothing to unstage at point")))))

(defun xueliang-git-status--delete-untracked (file)
  "Delete untracked FILE, a directory or a plain file."
  (let ((path (expand-file-name file default-directory)))
    (if (file-directory-p path)
        (delete-directory path t)
      (delete-file path))))

(defun xueliang-git-status-discard ()
  "Discard the unstaged or untracked change at point, after confirmation."
  (interactive)
  (let* ((section (xueliang-git-status--section-at-point))
         (kind (plist-get section :kind))
         (status (plist-get section :status)))
    (pcase kind
      ('hunk
       (unless (eq status 'unstaged)
         (user-error "Unstage that hunk first"))
       (when (yes-or-no-p "Discard this hunk? ")
         (xueliang-git-status--finish
          (xueliang-git-status--apply-patch (plist-get section :patch) "-R")
          "Discarded hunk")))
      ((or 'file 'section)
       (let ((files (xueliang-git-status--section-files section)))
         (pcase status
           ('unstaged
            (when (yes-or-no-p (format "Discard changes in %s? "
                                       (xueliang-git-status--describe-files files)))
              (xueliang-git-status--finish
               (apply #'xueliang-git-status--git-sync "checkout" "--" files)
               (format "Discarded %s"
                       (xueliang-git-status--describe-files files)))))
           ('untracked
            (when (yes-or-no-p (format "Delete %s? "
                                       (xueliang-git-status--describe-files files)))
              (dolist (file files)
                (xueliang-git-status--delete-untracked file))
              (xueliang-git-status)
              (message "Deleted %s"
                       (xueliang-git-status--describe-files files))))
           (_ (user-error "Unstage that change first")))))
      (_ (user-error "Nothing to discard at point")))))

(defun xueliang-git-status-stage-all ()
  "Stage every change in the working tree."
  (interactive)
  (xueliang-git-status--finish
   (xueliang-git-status--git-sync "add" "--all")
   "Staged all changes"))

(defun xueliang-git-status-unstage-all ()
  "Unstage everything currently in the index."
  (interactive)
  (xueliang-git-status--finish
   (xueliang-git-status--unstage-files '("."))
   "Unstaged all changes"))

(defun xueliang-git-status--show-line-face (line)
  "Return the face for one LINE of `git show' diff output."
  (cond
   ((string-prefix-p "@@" line) 'xueliang-git-status-hunk-heading)
   ((string-prefix-p "+++" line) 'diff-header)
   ((string-prefix-p "---" line) 'diff-header)
   ((string-prefix-p "+" line) 'diff-added)
   ((string-prefix-p "-" line) 'diff-removed)
   ((string-match-p
     "\\`\\(?:diff --\\(?:git\\|cc\\|combined\\) \\|index \\|new file mode \\|deleted file mode \\|old mode \\|new mode \\|similarity index \\|dissimilarity index \\|rename \\|copy \\|Binary files \\)"
     line)
    'diff-header)
   (t 'diff-context)))

(defun xueliang-git-status--insert-show-header-line (line)
  "Insert one LINE of a `git show' commit header with faces."
  (cond
   ((string-match "\\`commit \\([0-9a-f]+\\)\\'" line)
    (insert (propertize "commit " 'face 'xueliang-git-status-heading)
            (propertize (match-string 1 line) 'face 'xueliang-git-status-hash)
            "\n"))
   ((string-match "\\`\\(Author\\|Date\\|Merge\\|Committer\\): *\\(.*\\)\\'" line)
    (insert (propertize (concat (match-string 1 line) ":")
                        'face 'xueliang-git-status-heading)
            " " (match-string 2 line) "\n"))
   (t (insert line "\n"))))

(defun xueliang-git-status--insert-show (text)
  "Insert `git show' output TEXT, styled like the status buffer.
The commit header gets the status faces and every diff line the same
added, removed, context and hunk-heading faces as the status hunks."
  (let ((header t))
    (dolist (line (split-string (string-trim-right text) "\n"))
      (when (and header
                 (string-match-p "\\`diff --\\(?:git\\|cc\\|combined\\) " line))
        (setq header nil))
      (if header
          (xueliang-git-status--insert-show-header-line line)
        (insert (propertize (concat line "\n")
                            'face (xueliang-git-status--show-line-face line)))))))

(defun xueliang-git-status--show-commit (hash)
  "Show commit HASH in a read-only buffer, with the status buffer's faces."
  (let ((out (generate-new-buffer " *git-show*")))
    (unwind-protect
        (let ((code (call-process "git" nil out nil
                                  "-C" default-directory "--no-pager"
                                  "show" "--no-color" hash)))
          (unless (zerop code)
            (user-error "git show %s failed: %s" hash
                        (string-trim (with-current-buffer out
                                       (buffer-string)))))
          (let ((buffer (get-buffer-create (format "*git-show: %s*" hash))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (xueliang-git-status--insert-show
                 (with-current-buffer out (buffer-string))))
              (xueliang-git-show-mode)
              (goto-char (point-min))
              (dolist (window (get-buffer-window-list buffer nil t))
                (set-window-point window (point-min))))
            (pop-to-buffer buffer)))
      (kill-buffer out))))

(defun xueliang-git-status--hunk-line (section)
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

(defun xueliang-git-status-visit ()
  "Visit the file or show the commit at point.
On a hunk, open its file at the line the hunk line belongs to."
  (interactive)
  (let ((section (xueliang-git-status--section-at-point)))
    (if (memq (plist-get section :kind) '(file hunk))
        (let* ((file (plist-get section :path))
               (path (expand-file-name file default-directory))
               (line (if (eq (plist-get section :kind) 'hunk)
                         (xueliang-git-status--hunk-line section)
                       1)))
          (unless (file-exists-p path)
            (user-error "File does not exist: %s" file))
          (find-file path)
          (goto-char (point-min))
          (forward-line (1- line))
          (when (get-buffer-window (current-buffer) t)
            (recenter)))
      (let ((commit (xueliang-git-status--property 'xueliang-git-status-commit)))
        (if commit
            (xueliang-git-status--show-commit commit)
          (user-error "Nothing to visit at point"))))))

(defun xueliang-git-status--property (property)
  "Return PROPERTY from the line at point."
  (or (get-text-property (point) property)
      (get-text-property (line-beginning-position) property)))

;; ============================================================
;; Rendering
;; ============================================================

(defun xueliang-git-status--render (buffer log status log-code status-code diffs)
  "Render LOG, STATUS and DIFFS into BUFFER.
LOG-CODE and STATUS-CODE are the exit codes of the git commands."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (entries (xueliang-git-status--entries status))
            (branch (xueliang-git-status--branch status))
            (subject (xueliang-git-status--subject log)))
        (erase-buffer)
        (setq xueliang-git-status--sections nil
              xueliang-git-status--overlays nil)
        (insert (propertize "Head:" 'face 'bold)
                "     "
                (propertize branch 'face 'xueliang-git-status-branch)
                (if (string= subject "") "" (concat " " subject))
                "\n\n")
        (xueliang-git-status--insert-files "Untracked files" 'untracked
                                           (nth 2 entries) diffs)
        (xueliang-git-status--insert-files "Unstaged changes" 'unstaged
                                           (nth 1 entries) diffs)
        (xueliang-git-status--insert-files "Staged changes" 'staged
                                           (nth 0 entries) diffs)
        (when (and (null (nth 0 entries))
                   (null (nth 1 entries))
                   (null (nth 2 entries)))
          (insert (propertize "Working tree clean\n\n"
                              'face 'xueliang-git-status-comment)))
        (let ((section (list :key "section:log"
                             :kind 'section
                             :status 'log
                             :heading-start (point))))
          (insert (propertize "Recent commits" 'face 'xueliang-git-status-heading)
                  "\n")
          (plist-put section :content-start (point))
          (if (string= log "")
              (insert (propertize "No commits yet\n"
                                  'face 'xueliang-git-status-comment))
            (dolist (line (split-string (string-trim-right log) "\n" t))
              (xueliang-git-status--insert-commit line)))
          (plist-put section :content-end (point))
          (push section xueliang-git-status--sections))
        (unless (and (zerop status-code) (zerop log-code))
          (insert (propertize
                   (format "\nGit exited with %d (status) / %d (log)\n"
                           status-code log-code)
                   'face 'error)))
        (xueliang-git-status--apply-folds))
      (setq buffer-read-only t)
      (xueliang-git-status--restore-point)
      (message "Git status ready: %s" (buffer-name buffer)))))

(defun xueliang-git-status--load (buffer root)
  "Load the log, status and per-file diffs of ROOT into BUFFER."
  (xueliang-git-status--git
   buffer "git-status-log" root
   '("log" "--oneline" "--decorate" "--no-color" "-n" "10")
   (lambda (log log-code)
     (xueliang-git-status--git
      buffer "git-status-status" root
      '("status" "--porcelain=v1" "--branch")
      (lambda (status status-code)
        (let* ((entries (xueliang-git-status--entries status))
               (unstaged (mapcar #'cdr (nth 1 entries)))
               (staged (mapcar #'cdr (nth 0 entries)))
               (diffs (make-hash-table :test #'equal))
               (pending (+ (length unstaged) (length staged))))
          (if (zerop pending)
              (xueliang-git-status--render buffer log status
                                           log-code status-code diffs)
            (dolist (spec (append (mapcar (lambda (path) (cons 'unstaged path))
                                          unstaged)
                                  (mapcar (lambda (path) (cons 'staged path))
                                          staged)))
              (let ((status-kind (car spec))
                    (path (cdr spec)))
                (xueliang-git-status--git
                 buffer (format "git-status-diff-%s-%s" status-kind path) root
                 (append (if (eq status-kind 'staged)
                             '("diff" "--cached")
                           '("diff"))
                         '("--no-color" "--no-ext-diff" "--")
                         (list path))
                 (lambda (text _code)
                   (puthash (concat (symbol-name status-kind) ":" path)
                            text diffs)
                   (setq pending (1- pending))
                   (when (zerop pending)
                     (xueliang-git-status--render buffer log status
                                                  log-code status-code
                                                  diffs)))))))))))))

;; ============================================================
;; Mode and entry point
;; ============================================================

(defvar-keymap xueliang-git-status-mode-map
  :doc "Keymap for `xueliang-git-status-mode'."
  :parent special-mode-map
  "TAB" #'xueliang-git-status-toggle
  "<tab>" #'xueliang-git-status-toggle
  "<backtab>" #'xueliang-git-status-toggle-files
  "s" #'xueliang-git-status-stage
  "u" #'xueliang-git-status-unstage
  "X" #'xueliang-git-status-discard
  "S" #'xueliang-git-status-stage-all
  "U" #'xueliang-git-status-unstage-all
  "RET" #'xueliang-git-status-visit
  "<return>" #'xueliang-git-status-visit)

(define-derived-mode xueliang-git-status-mode special-mode "Git-Status"
  "Major mode for `xueliang-git-status' buffers."
  (unless (hash-table-p xueliang-git-status--expanded)
    (setq xueliang-git-status--expanded (make-hash-table :test #'equal)))
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (xueliang-git-status))))

(define-derived-mode xueliang-git-show-mode special-mode "Git-Show"
  "Major mode for `xueliang-git-status--show-commit' buffers.")

(defun xueliang-git-status--root ()
  "Return the expanded Git root for `default-directory', or nil.
Dired and some other buffers use an abbreviated directory such as
\"~/workspace/repo/\"; `git -C' does not expand \"~\"."
  (when-let* ((root (locate-dominating-file default-directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(defun xueliang-git-status ()
  "Show the Git status and recent log of the current repository.
The buffer lists the head, untracked, unstaged and staged files
with their diff hunks, and the ten most recent commits.  `g'
refreshes the buffer and `q' quits it.  TAB folds or unfolds the
file or hunk at point, S-TAB toggles every file diff.  `s' stages
the hunk, file or section at point, `u' unstages it, `X' discards
it after confirmation, and `S'/`U' stage or unstage everything.
`RET' opens the file at the hunk line, or shows the commit at point."
  (interactive)
  (let* ((root (xueliang-git-status--root))
         (repo (and root (file-name-nondirectory (directory-file-name root))))
         (buffer (and root (get-buffer-create (format "*git-status: %s*" repo)))))
    (unless root
      (user-error "Not inside a Git repository"))
    (unless (executable-find "git")
      (user-error "Git is not installed"))
    (with-current-buffer buffer
      ;; Re-running the mode would kill the buffer-locals that keep the
      ;; fold state, the section list and the refresh position.
      (unless (derived-mode-p 'xueliang-git-status-mode)
        (xueliang-git-status-mode))
      (setq xueliang-git-status--restore-key
            (plist-get (xueliang-git-status--section-at-point) :key))
      ;; A refresh replaces the previous run instead of racing it.
      (dolist (proc xueliang-git-status--processes)
        (when (process-live-p proc) (delete-process proc)))
      (setq xueliang-git-status--processes nil)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq xueliang-git-status--sections nil
            xueliang-git-status--overlays nil
            buffer-read-only nil
            default-directory root))
    (display-buffer buffer)
    (xueliang-git-status--load buffer root)))

;; Evil's normal state would otherwise take TAB, s, u, X, S, U and RET first.
(with-eval-after-load 'evil
  (evil-define-key 'normal xueliang-git-status-mode-map
    (kbd "TAB") #'xueliang-git-status-toggle
    (kbd "<backtab>") #'xueliang-git-status-toggle-files
    "s" #'xueliang-git-status-stage
    "u" #'xueliang-git-status-unstage
    "X" #'xueliang-git-status-discard
    "x" #'xueliang-git-status-discard
    "S" #'xueliang-git-status-stage-all
    "U" #'xueliang-git-status-unstage-all
    (kbd "RET") #'xueliang-git-status-visit
    (kbd "<return>") #'xueliang-git-status-visit))

;; ============================================================
;; GBlame - git blame in a side window, like vim's :Gblame
;; ============================================================
;; `xueliang-gblame' (SPC g b, or :Gblame) shows one line of blame for
;; every line of the current file in a narrow window on the left:
;; abbreviated commit, author and date, aligned with the file.  Different
;; commits get different hash colours, so the boundaries between commits
;; stand out; uncommitted lines stay dim.  The window follows the source
;; window while scrolling, highlights the line at point, refreshes after
;; saving, and closes with `q'.  RET on a blame line shows that commit.
(defface xueliang-gblame-current-line
  '((t :inherit highlight))
  "Face for the line at point in the GBlame window."
  :group 'faces)

(defvar-local xueliang-gblame--buffer nil
  "GBlame buffer of this source buffer, when GBlame is open.")
(defvar-local xueliang-gblame--root nil
  "Repository root of the source buffer's file.")
(defvar-local xueliang-gblame--file nil
  "Absolute name of the file being blamed.")

(defvar-local xueliang-gblame--source nil
  "Source buffer of this GBlame buffer.")
(defvar-local xueliang-gblame--highlight nil
  "Overlay marking the source line in this GBlame buffer.")

(defun xueliang-gblame--git (&rest args)
  "Run git ARGS in `default-directory' and return its output."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil t nil
                       "-C" default-directory "--no-pager"
                       "-c" "core.quotepath=false" args)))
      (unless (zerop code)
        (user-error "git %s failed: %s"
                    (car args) (string-trim (buffer-string))))
      (buffer-string))))

(defun xueliang-gblame--parse (output)
  "Return blame records (LINE HASH AUTHOR TIME) parsed from OUTPUT.
OUTPUT is the text of `git blame --line-porcelain'."
  (let (records
        (line nil) (hash nil) (author nil) (time nil))
    (dolist (text (split-string output "\n"))
      (cond
       ((string-match "\\`\\([0-9a-f]+\\) [0-9]+ \\([0-9]+\\)" text)
        (setq hash (match-string 1 text)
              line (string-to-number (match-string 2 text))
              author nil
              time nil))
       ((string-prefix-p "author " text)
        (setq author (substring text 7)))
       ((string-prefix-p "author-time " text)
        (setq time (string-to-number (substring text 12))))
       ;; The tab-prefixed line is the content and ends a record.
       ((and hash (string-prefix-p "\t" text))
        (push (list line hash (or author "?") time) records)
        (setq hash nil))))
    records))

(defun xueliang-gblame--pad (text width)
  "Return TEXT padded with spaces to WIDTH display columns."
  (concat text (make-string (max 0 (- width (string-width text))) ?\s)))

(defvar xueliang-gblame-hash-faces
  '(font-lock-keyword-face        ; blue
    font-lock-warning-face        ; orange
    font-lock-string-face         ; green
    font-lock-function-name-face  ; magenta
    font-lock-doc-face            ; brown
    font-lock-constant-face       ; purple
    font-lock-type-face)          ; pink
  "Faces that colour different commits in a GBlame window.
A commit gets the next face the first time it appears in the file, so
commits next to each other are easy to tell apart.  The faces inherit
from the active theme; set this to nil to colour every hash with
`xueliang-git-status-hash'.")

(defun xueliang-gblame--format (record face)
  "Return the display line for one blame RECORD.
Its hash is coloured with FACE."
  (pcase-let ((`(,_ ,hash ,author ,time) record))
    (let ((short (substring hash 0 (min 8 (length hash))))
          (author (truncate-string-to-width author 18 nil nil "…"))
          (date (if time (format-time-string "%Y-%m-%d" time) "")))
      (concat (propertize short 'face face)
              " "
              (xueliang-gblame--pad author 18)
              " "
              (propertize date 'face 'xueliang-git-status-comment)))))

(defun xueliang-gblame--insert (records)
  "Insert one line per source line for blame RECORDS.
Different commits get different hash colours, so the boundaries between
commits are easy to see."
  (let ((table (make-hash-table :test #'eql))
        (faces (vconcat xueliang-gblame-hash-faces))
        (face-of (make-hash-table :test #'equal))
        (next 0)
        (lines 0))
    (dolist (record records)
      (puthash (car record) record table)
      (setq lines (max lines (car record))))
    (dotimes (i lines)
      (let* ((record (gethash (1+ i) table))
             (hash (and record (cadr record)))
             (face (cond
                    ((null record) nil)
                    ;; An uncommitted line gets no colour of its own.
                    ((string-match-p "\\`0+\\'" hash)
                     'xueliang-git-status-comment)
                    ((gethash hash face-of))
                    ((zerop (length faces))
                     'xueliang-git-status-hash)
                    (t
                     (let ((face (aref faces (mod next (length faces)))))
                       (setq next (1+ next))
                       (puthash hash face face-of)
                       face))))
             (start (point)))
        (insert (if record (xueliang-gblame--format record face) "") "\n")
        (when record
          (add-text-properties start (1- (point))
                               (list 'xueliang-gblame-hash hash)))))))

(defun xueliang-gblame--fill (buffer root file)
  "Fill BUFFER with the blame of FILE in ROOT."
  (let* ((default-directory root)
         (output (xueliang-gblame--git "blame" "--line-porcelain" "--"
                                       (file-relative-name file root))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (xueliang-gblame--insert (xueliang-gblame--parse output))
        (goto-char (point-min))
        (setq xueliang-gblame--highlight (make-overlay (point) (point)))
        (overlay-put xueliang-gblame--highlight
                     'face 'xueliang-gblame-current-line)))))

(defun xueliang-gblame--sync (&optional window _new-start)
  "Align the GBlame window with WINDOW and follow its point.
WINDOW defaults to the selected window; `window-scroll-functions' also
passes the new window start."
  (let* ((source-window (or window (selected-window)))
         (source (window-buffer source-window))
         (blame (buffer-local-value 'xueliang-gblame--buffer source))
         (blame-window (and (buffer-live-p blame)
                            (get-buffer-window blame t))))
    (when (and blame-window (window-live-p source-window)
               (eq source (current-buffer)))
      (let ((top (save-excursion
                   (goto-char (window-start source-window))
                   (line-number-at-pos)))
            (line (save-excursion
                    (goto-char (window-point source-window))
                    (line-number-at-pos))))
        (with-current-buffer blame
          (let* ((height (max 1 (window-body-height blame-window)))
                 ;; Keep the blame window's point inside the visible lines:
                 ;; otherwise redisplay scrolls back to point-min and the
                 ;; window never appears to follow the source window.
                 (target (if (and (>= line top) (< line (+ top height)))
                             line
                           top)))
            (when xueliang-gblame--highlight
              (goto-char (point-min))
              (forward-line (1- line))
              (move-overlay xueliang-gblame--highlight
                            (line-beginning-position)
                            (line-end-position)))
            (goto-char (point-min))
            (forward-line (1- top))
            (set-window-start blame-window (point) t)
            (goto-char (point-min))
            (forward-line (1- target))
            (set-window-point blame-window (point))))))))

(defun xueliang-gblame--refresh ()
  "Redraw this source buffer's GBlame window, if it is open."
  (when (and (buffer-live-p xueliang-gblame--buffer)
             xueliang-gblame--root
             xueliang-gblame--file)
    (xueliang-gblame--fill xueliang-gblame--buffer
                           xueliang-gblame--root xueliang-gblame--file)
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (xueliang-gblame--sync window))))

(defun xueliang-gblame--cleanup ()
  "Forget this GBlame buffer in its source buffer."
  (when (buffer-live-p xueliang-gblame--source)
    (with-current-buffer xueliang-gblame--source
      (setq xueliang-gblame--buffer nil)
      (remove-hook 'post-command-hook #'xueliang-gblame--sync t)
      (remove-hook 'window-scroll-functions #'xueliang-gblame--sync t)
      (remove-hook 'after-save-hook #'xueliang-gblame--refresh t)
      (remove-hook 'kill-buffer-hook #'xueliang-gblame--close t))))

(defun xueliang-gblame--close ()
  "Close this source buffer's GBlame window.
Return non-nil when a GBlame buffer was open."
  (when (buffer-live-p xueliang-gblame--buffer)
    (let ((window (get-buffer-window xueliang-gblame--buffer t)))
      (when (window-live-p window)
        (delete-window window)))
    (kill-buffer xueliang-gblame--buffer)
    t))

(defun xueliang-gblame-quit ()
  "Close the GBlame window."
  (interactive)
  (let ((source xueliang-gblame--source))
    (if (buffer-live-p source)
        (with-current-buffer source
          (xueliang-gblame--close))
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (delete-window window))
      (kill-buffer (current-buffer)))))

(defun xueliang-gblame-refresh ()
  "Re-run git blame for the current GBlame window."
  (interactive)
  (let ((source xueliang-gblame--source))
    (unless (buffer-live-p source)
      (user-error "The source buffer is gone"))
    (with-current-buffer source
      (xueliang-gblame--refresh))))

(defun xueliang-gblame-show-commit ()
  "Show the commit that last touched the line at point."
  (interactive)
  (let ((hash (get-text-property (line-beginning-position)
                                 'xueliang-gblame-hash)))
    (cond
     ((null hash) (user-error "No blame information on this line"))
     ((string-match-p "\\`0+\\'" hash)
      (user-error "This line is not committed yet"))
     ((fboundp 'xueliang-git-status--show-commit)
      (let ((default-directory (or xueliang-gblame--root default-directory)))
        (xueliang-git-status--show-commit hash)))
     (t (message "Commit %s" hash)))))

(defun xueliang-gblame ()
  "Toggle a git blame window on the left of the current file.
Each line shows the abbreviated commit, author and date, aligned with
the file; the window follows the file's scrolling and point.  It is an
ordinary split, so `C-w C-w' focuses it and `:only'/`C-w c' close it.
`RET' shows the commit at point, `g' refreshes, `q' closes the window."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (if (and (buffer-live-p xueliang-gblame--buffer)
           (get-buffer-window xueliang-gblame--buffer t))
      (progn
        (xueliang-gblame--close)
        (message "GBlame closed"))
    ;; The buffer may survive a window closed with :only, C-w c or :q.
    ;; Drop it before opening a fresh one.
    (xueliang-gblame--close)
    (let* ((source (current-buffer))
           (file (file-truename buffer-file-name))
           (root (locate-dominating-file file ".git")))
      (unless root
        (user-error "Not inside a Git repository"))
      (setq root (file-name-as-directory (expand-file-name root)))
      (let ((blame (get-buffer-create
                    (format "*GBlame: %s*" (file-name-nondirectory file)))))
        (with-current-buffer blame
          (xueliang-gblame-mode)
          (setq xueliang-gblame--source source
                xueliang-gblame--root root
                xueliang-gblame--file file)
          (add-hook 'kill-buffer-hook #'xueliang-gblame--cleanup nil t))
        (xueliang-gblame--fill blame root file)
        (with-current-buffer source
          (setq xueliang-gblame--root root
                xueliang-gblame--file file
                xueliang-gblame--buffer blame)
          (add-hook 'post-command-hook #'xueliang-gblame--sync nil t)
          (add-hook 'window-scroll-functions #'xueliang-gblame--sync nil t)
          (add-hook 'after-save-hook #'xueliang-gblame--refresh nil t)
          (add-hook 'kill-buffer-hook #'xueliang-gblame--close nil t))
        (display-buffer blame
                        '((display-buffer-in-direction)
                          (direction . left)
                          (window-width . 44)))
        (with-current-buffer source
          (xueliang-gblame--sync (get-buffer-window source t)))))))

(defvar-keymap xueliang-gblame-mode-map
  :doc "Keymap for `xueliang-gblame-mode'."
  :parent special-mode-map
  "RET" #'xueliang-gblame-show-commit
  "<return>" #'xueliang-gblame-show-commit
  "g" #'xueliang-gblame-refresh
  "q" #'xueliang-gblame-quit)

(define-derived-mode xueliang-gblame-mode special-mode "GBlame"
  "Major mode for `xueliang-gblame' buffers."
  (setq-local truncate-lines t))

;; Evil's normal state would otherwise take RET, g and q first.
(with-eval-after-load 'evil
  (evil-define-key 'normal xueliang-gblame-mode-map
    (kbd "RET") #'xueliang-gblame-show-commit
    (kbd "<return>") #'xueliang-gblame-show-commit
    "g" #'xueliang-gblame-refresh
    "q" #'xueliang-gblame-quit)
  ;; Same command as vim-fugitive's :Gblame
  (evil-ex-define-cmd "Gblame" #'xueliang-gblame))

;; ============================================================
;; Vim-fugitive style commands
;; ============================================================
;; The G* names match vim-fugitive's :Gwrite and :Gcommit, which are also
;; available as Ex commands.  Both act on the current file's directory, so
;; they work from any buffer that visits a tracked file.
(defun xueliang-git-status--file-directory ()
  "Return the current file's directory, or `default-directory'."
  (if buffer-file-name
      (file-name-directory (expand-file-name buffer-file-name))
    default-directory))

(defun Gwrite ()
  "Stage the current file, like vim-fugitive's :Gwrite."
  (interactive)
  (unless (fboundp 'evil-ex-execute)
    (user-error "Gwrite needs Evil"))
  (let ((default-directory (xueliang-git-status--file-directory)))
    (evil-ex-execute "!git add %")))

(defun Gcommit ()
  "Commit the current file, like vim-fugitive's :Gcommit."
  (interactive)
  (unless (fboundp 'evil-ex-execute)
    (user-error "Gcommit needs Evil"))
  (let ((default-directory (xueliang-git-status--file-directory)))
    (evil-ex-execute (format "!git commit -m \"Update %s\""
                             (file-name-nondirectory buffer-file-name)))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "Gwrite" #'Gwrite)
  (evil-ex-define-cmd "Gcommit" #'Gcommit))

(provide 'git-status)
;;; git-status.el ends here
