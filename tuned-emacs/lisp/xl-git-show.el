;;; xl-git-show.el --- Diff and commit views for xl-git-status -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0") (xl-git-core "0.1.0"))
;; Keywords: tools, vc

;;; Commentary:
;; The faces shared by the status and commit buffers, the code that turns a
;; diff into foldable hunks, and the asynchronous commit view.  The status
;; buffer and the G commands use these; the Git processes and parsing live
;; in xl-git-core.el.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'xl-async)
(require 'xl-spinner)
(require 'xl-git-core)

;; The faces keep the `xl-git-status-' prefix although they live here: they
;; are user-facing, and themes customize them by name.  Renaming them would
;; break every theme that sets, say, `xl-git-status-file'.
(defgroup xl-git-status nil
  "Faces for `xl-git-status'."
  :group 'tools)

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

(defun xl-git-show-label-face (label)
  "Return the face for status LABEL."
  (pcase label
    ((or "deleted" "unmerged") 'xl-git-status-deleted)
    ((or "renamed" "copied") 'xl-git-status-file)
    (_ 'xl-git-status-status)))

;; ============================================================
;; Diff view
;; ============================================================

(defun xl-git-show--split-diff (text)
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

(defun xl-git-show--diff-path (lines)
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

(defun xl-git-show-split-files (text)
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
                    (when-let* ((path (xl-git-show--diff-path chunk)))
                      (cons path (concat (mapconcat #'identity chunk "\n") "\n"))))
                  (nreverse chunks)))))

(defun xl-git-show--header-notes (header)
  "Return the informative lines of a diff HEADER.
The file name already appears on the section line, so drop the
`diff --git', `index', `---' and `+++' lines."
  (seq-remove (lambda (line)
                (or (string-empty-p line)
                    (string-match-p "\\`\\(?:diff --git \\|index \\|--- \\|\\+\\+\\+ \\)"
                                    line)))
              (split-string header "\n")))

(defun xl-git-show--diff-line-face (line)
  "Return the face for one LINE of a diff."
  (if (string-empty-p line)
      'diff-context
    (pcase (aref line 0)
      (?+ 'diff-added)
      (?- 'diff-removed)
      (_ 'diff-context))))

(defun xl-git-show-insert-diff (status path text)
  "Insert diff TEXT for PATH with STATUS as foldable hunk sections.
Return the hunk sections it made, newest first, or nil for a binary file
or a mode change.  The caller owns the section list, so this file does
not write another module's buffer-local state."
  (let* ((parts (xl-git-show--split-diff text))
         (header (car parts))
         (hunks (cadr parts)))
    (if (null hunks)
        ;; Binary files and mode changes have no hunks, only header lines.
        (dolist (line (xl-git-show--header-notes header))
          (insert (propertize (concat line "\n") 'face 'diff-header)))
      (let ((index 0)
            (sections nil))
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
                                  'face (xl-git-show--diff-line-face line))))
            (plist-put section :content-end (point))
            (push section sections)))
        sections))))

(defun xl-git-show-insert-commit (line)
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

(defun xl-git-show--show-line-face (line)
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

(defun xl-git-show--insert-show-header-line (line)
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

(defun xl-git-show--insert-show (text)
  "Insert `git show' output TEXT, styled like the status buffer.
The commit header gets the status faces and every diff line the same
added, removed, context and hunk-heading faces as the status hunks."
  (let ((header t))
    (dolist (line (split-string (string-trim-right text) "\n"))
      (when (and header
                 (string-match-p "\\`diff --\\(?:git\\|cc\\|combined\\) " line))
        (setq header nil))
      (if header
          (xl-git-show--insert-show-header-line line)
        (insert (propertize (concat line "\n")
                            'face (xl-git-show--show-line-face line)))))))


;; ============================================================
;; Commit view
;; ============================================================
;; `git show' runs in the background: the buffer opens at once with the
;; commit hash and a loader, and is filled when the diff arrives.  A new
;; request for the same buffer (another commit, or the same hash again)
;; discards the older answer.
(defvar-local xl-git-show--show-spinner nil
  "Overlay drawing the loader in this commit buffer.")

(defun xl-git-show--show-draw (text)
  "Show TEXT in this commit buffer's loader; nil removes it."
  (if (and text (overlayp xl-git-show--show-spinner))
      (overlay-put xl-git-show--show-spinner 'display text)
    (when (overlayp xl-git-show--show-spinner)
      (delete-overlay xl-git-show--show-spinner)
      (setq xl-git-show--show-spinner nil))))

(defun xl-git-show--show-teardown ()
  "Stop this commit buffer's loader; `xl-async' kills its process."
  (xl-spinner-stop))

(defun xl-git-show--show-fetch (hash root)
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
                    (xl-git-show--insert-show text)
                  (insert (propertize "git show failed: " 'face 'error)
                          (string-trim text) "\n")))
              (goto-char (point-min))
              (dolist (window (get-buffer-window-list (current-buffer) nil t))
                (set-window-point window (point-min))))))

(defun xl-git-show-commit (hash)
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
          (setq xl-git-show--show-spinner (make-overlay start (point)))
          (overlay-put xl-git-show--show-spinner
                       'face 'xl-git-status-branch))
        (insert "  " (propertize "Loading..." 'face 'xl-git-status-comment)
                "\n"))
      ;; A minute of frames at most, should git never answer.
      (xl-spinner-start #'xl-git-show--show-draw :limit 600)
      (add-hook 'kill-buffer-hook #'xl-git-show--show-teardown nil t)
      (xl-git-show--show-fetch hash root)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))


(define-derived-mode xl-git-show-mode special-mode "Git-Show"
  "Major mode for `xl-git-show-commit' buffers."
  (require 'diff-mode))

(provide 'xl-git-show)
;;; xl-git-show.el ends here
