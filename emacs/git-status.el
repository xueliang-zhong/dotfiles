;;; git-status.el --- A small asynchronous Git status buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `xueliang-git-status' shows a Magit-like summary without loading Magit:
;; the head, untracked/unstaged/staged files and recent commits.  Both git
;; commands run asynchronously; the buffer is read-only, `g' refreshes it and
;; `q' quits it.  On a file line `s' stages and `u' unstages; `RET' visits the
;; file or, on a recent commit, shows that commit.

;;; Code:

(require 'subr-x) ; string-trim-right
(require 'seq)    ; seq-remove

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

(defvar-local xueliang-git-status--processes nil
  "Git processes still running for this status buffer.")

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

(defun xueliang-git-status--insert-files (title kind files)
  "Insert a TITLE section for files of KIND, listing FILES."
  (when files
    (insert (propertize (format "%s (%d)\n" title (length files))
                        'face 'xueliang-git-status-heading))
    (dolist (entry files)
      (let ((start (point)))
        (insert (propertize (format "%-10s " (car entry))
                            'face (xueliang-git-status--label-face (car entry)))
                (propertize (cdr entry) 'face 'xueliang-git-status-file)
                "\n")
        (add-text-properties start (point)
                             (list 'xueliang-git-status-file (cdr entry)
                                   'xueliang-git-status-kind kind))))
    (insert "\n")))

(defun xueliang-git-status--property (property)
  "Return PROPERTY from the line at point."
  (or (get-text-property (point) property)
      (get-text-property (line-beginning-position) property)))

(defun xueliang-git-status--git-sync (&rest args)
  "Run git ARGS in the current status buffer's repository.
Return a cons (EXIT-CODE . OUTPUT)."
  (let ((out (generate-new-buffer " *git-status-command*")))
    (unwind-protect
        (cons (apply #'call-process "git" nil out nil
                     "-C" default-directory "--no-pager" args)
              (with-current-buffer out (buffer-string)))
      (kill-buffer out))))

(defun xueliang-git-status--finish (result action)
  "Refresh after RESULT succeeded, else report ACTION with its output."
  (if (zerop (car result))
      (progn
        (xueliang-git-status)
        (message "%s" action))
    (user-error "%s failed: %s" action
                (string-trim (cdr result)))))

(defun xueliang-git-status-stage ()
  "Stage the file at point."
  (interactive)
  (let ((file (xueliang-git-status--property 'xueliang-git-status-file))
        (kind (xueliang-git-status--property 'xueliang-git-status-kind)))
    (unless file
      (user-error "No file at point"))
    (when (eq kind 'staged)
      (user-error "Already staged: %s" file))
    (xueliang-git-status--finish
     (xueliang-git-status--git-sync "add" "--" file)
     (format "Staged %s" file))))

(defun xueliang-git-status-unstage ()
  "Unstage the file at point."
  (interactive)
  (let ((file (xueliang-git-status--property 'xueliang-git-status-file))
        (kind (xueliang-git-status--property 'xueliang-git-status-kind)))
    (unless file
      (user-error "No file at point"))
    (unless (eq kind 'staged)
      (user-error "Nothing staged at point"))
    (let ((result (xueliang-git-status--git-sync
                   "reset" "-q" "HEAD" "--" file)))
      ;; A repository without commits has no HEAD; remove the new file
      ;; from the index instead.
      (unless (zerop (car result))
        (setq result (xueliang-git-status--git-sync
                      "rm" "--cached" "-q" "--" file)))
      (xueliang-git-status--finish result (format "Unstaged %s" file)))))

(defun xueliang-git-status--show-commit (hash)
  "Show commit HASH in a read-only buffer."
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
                (insert-buffer-substring out))
              (special-mode)
              (goto-char (point-min))
              (dolist (window (get-buffer-window-list buffer nil t))
                (set-window-point window (point-min))))
            (pop-to-buffer buffer)))
      (kill-buffer out))))

(defun xueliang-git-status-visit ()
  "Visit the file or show the commit at point."
  (interactive)
  (cond
   ((xueliang-git-status--property 'xueliang-git-status-file)
    (let* ((file (xueliang-git-status--property 'xueliang-git-status-file))
           (path (expand-file-name file default-directory)))
      (unless (file-exists-p path)
        (user-error "File does not exist: %s" file))
      (find-file path)))
   ((xueliang-git-status--property 'xueliang-git-status-commit)
    (xueliang-git-status--show-commit
     (xueliang-git-status--property 'xueliang-git-status-commit)))
   (t (user-error "Nothing to visit at point"))))

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

(defun xueliang-git-status--render (buffer log status log-code status-code)
  "Render LOG and STATUS into BUFFER.
LOG-CODE and STATUS-CODE are the exit codes of the two git commands."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (entries (xueliang-git-status--entries status))
            (branch (xueliang-git-status--branch status))
            (subject (xueliang-git-status--subject log)))
        (erase-buffer)
        (insert (propertize "Head:" 'face 'bold)
                "     "
                (propertize branch 'face 'xueliang-git-status-branch)
                (if (string= subject "") "" (concat " " subject))
                "\n\n")
        (xueliang-git-status--insert-files "Untracked files" 'untracked
                                           (nth 2 entries))
        (xueliang-git-status--insert-files "Unstaged changes" 'unstaged
                                           (nth 1 entries))
        (xueliang-git-status--insert-files "Staged changes" 'staged
                                           (nth 0 entries))
        (when (and (null (nth 0 entries))
                   (null (nth 1 entries))
                   (null (nth 2 entries)))
          (insert (propertize "Working tree clean\n\n"
                              'face 'xueliang-git-status-comment)))
        (insert (propertize "Recent commits" 'face 'xueliang-git-status-heading)
                "\n")
        (if (string= log "")
            (insert (propertize "No commits yet\n"
                                'face 'xueliang-git-status-comment))
          (dolist (line (split-string (string-trim-right log) "\n" t))
            (xueliang-git-status--insert-commit line)))
        (unless (and (zerop status-code) (zerop log-code))
          (insert (propertize
                   (format "\nGit exited with %d (status) / %d (log)\n"
                           status-code log-code)
                   'face 'error))))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-point window (point-min)))
      (message "Git status ready: %s" (buffer-name buffer)))))

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

(defvar-keymap xueliang-git-status-mode-map
  :doc "Keymap for `xueliang-git-status-mode'."
  :parent special-mode-map
  "s" #'xueliang-git-status-stage
  "u" #'xueliang-git-status-unstage
  "RET" #'xueliang-git-status-visit
  "<return>" #'xueliang-git-status-visit)

(define-derived-mode xueliang-git-status-mode special-mode "Git-Status"
  "Major mode for `xueliang-git-status' buffers."
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (xueliang-git-status))))

(defun xueliang-git-status--root ()
  "Return the expanded Git root for `default-directory', or nil.
Dired and some other buffers use an abbreviated directory such as
\"~/workspace/repo/\"; `git -C' does not expand \"~\"."
  (when-let* ((root (locate-dominating-file default-directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(defun xueliang-git-status ()
  "Show a simple Git status and recent log asynchronously.
The buffer lists the head, staged, unstaged and untracked files, and
the ten most recent commits.  There are no diffs or folding; `g'
refreshes the buffer and `q' quits it.  `s' stages and `u' unstages
the file at point; `RET' visits the file or commit at point."
  (interactive)
  (let* ((root (xueliang-git-status--root))
         (repo (and root (file-name-nondirectory (directory-file-name root))))
         (buffer (and root (get-buffer-create (format "*git-status: %s*" repo)))))
    (unless root
      (user-error "Not inside a Git repository"))
    (unless (executable-find "git")
      (user-error "Git is not installed"))
    (with-current-buffer buffer
      ;; A refresh replaces the previous run instead of racing it.
      (dolist (proc xueliang-git-status--processes)
        (when (process-live-p proc) (delete-process proc)))
      (setq xueliang-git-status--processes nil)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (xueliang-git-status-mode)
      (setq buffer-read-only nil
            default-directory root))
    (display-buffer buffer)
    (xueliang-git-status--git
     buffer "git-status-log" root
     '("log" "--oneline" "--decorate" "--no-color" "-n" "10")
     (lambda (log log-code)
       (xueliang-git-status--git
        buffer "git-status-status" root
        '("status" "--porcelain=v1" "--branch")
        (lambda (status status-code)
          (xueliang-git-status--render buffer log status log-code status-code)))))))

;; Evil's normal state would otherwise take `s', `u' and RET first.
(with-eval-after-load 'evil
  (evil-define-key 'normal xueliang-git-status-mode-map
    "s" #'xueliang-git-status-stage
    "u" #'xueliang-git-status-unstage
    (kbd "RET") #'xueliang-git-status-visit
    (kbd "<return>") #'xueliang-git-status-visit))

(provide 'git-status)
;;; git-status.el ends here
