;;; tuned-git-core.el --- Git processes and parsing for the status views -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tuned-async "0.1.0"))
;; Keywords: tools, vc

;;; Commentary:
;; The part of the Git status feature that does not draw anything: parse
;; porcelain and diff output, build the command lines, run the write queue
;; of a repository, and look up small pieces of configuration.  Keeping it
;; free of buffers and faces makes it the easiest part to test and lets the
;; status and commit views depend on it without a cycle.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'tuned-async)

(defun tuned-git-core--label (char)
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

(defun tuned-git-core--head-line (status)
  "Return the `## branch' line of porcelain STATUS output.
The output is NUL-separated (`-z'); a newline also ends the line."
  (car (split-string status "[\0\n]")))

(defun tuned-git-core-branch (status)
  "Return the branch name from porcelain STATUS output."
  (let ((line (tuned-git-core--head-line status)))
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

(defun tuned-git-core-upstream (status)
  "Return the upstream ref from porcelain STATUS output, or nil."
  (let ((line (tuned-git-core--head-line status)))
    (when (string-match "\\`## [^ \n]+\\.\\.\\.\\([^ \n]+\\)" line)
      (match-string 1 line))))

(defun tuned-git-core-subject (log)
  "Return the newest commit's subject from LOG output."
  (let ((line (car (split-string log "\n"))))
    (if (string-match "\\`[0-9a-f]+ \\(?:([^)]*) \\)?\\(.*\\)\\'" line)
        (match-string 1 line)
      "")))

(defun tuned-git-core-entries (status)
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
                (push (cons (tuned-git-core--label x) path) staged))
              (unless (memq y '(?\s ?! ??))
                (push (cons (tuned-git-core--label y) path) unstaged)))))))
    (list (nreverse staged) (nreverse unstaged) (nreverse untracked))))

(defun tuned-git-core-queue-key (&optional root)
  "Return the key of the write queue shared by ROOT's repository.
Every Git command that writes the index or the refs of one repository
takes this key, so they run one at a time: a pull and a Gwrite cannot
fight over `.git/index.lock', and a Gwrite followed by a Gcommit runs in
that order.  ROOT defaults to `default-directory'.  This is the only
place that builds the key."
  (cons (file-truename (file-name-as-directory (expand-file-name
                                               (or root default-directory))))
        'git-writes))

(defun tuned-git-core-git-command (args)
  "Return the full git command line for ARGS.
`--no-optional-locks' stops a read such as `status' from rewriting
.git/index.  That rewrite is a change the auto refresh watches for, so
without the flag each refresh would trigger the next one.  Commands that
must write still lock as usual."
  (append '("git" "--no-pager" "--no-optional-locks" "-c" "core.quotepath=false")
          args))

(defun tuned-git-core-git-string (root &rest args)
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
;; The write queue
;; ============================================================

(defvar tuned-git-core--refresh-function nil
  "Function that refreshes the status buffer of a repository root.
The status view sets it when it loads; without one, a queued write still
runs, it just has no status buffer to refresh.")

(defun tuned-git-core-set-refresh-function (function)
  "Make FUNCTION refresh a repository's status after its write queue drains.
The status view calls this when it loads; the core never writes the
status view's private state, and the view never writes the core's."
  (setq tuned-git-core--refresh-function function))

(defun tuned-git-core-refresh (root)
  "Refresh the status buffer of ROOT through the registered function."
  (when (functionp tuned-git-core--refresh-function)
    (funcall tuned-git-core--refresh-function root)))

(defun tuned-git-core-enqueue (job &optional on-drain)
  "Queue JOB on the write queue of its repository, given as :root.
JOB is a plist with :root, :attempts (git argument lists, tried in
order), :action (the message on success) and, for `git apply', :patch
\(the text to apply, sent on standard input).  It may also give :owner
and :on-start.  ON-DRAIN defaults to refreshing the repository's status
through `tuned-git-core-refresh'.  See `tuned-async-enqueue' for the queue."
  (let* ((root (plist-get job :root))
         (action (plist-get job :action)))
    (tuned-async-enqueue
     (tuned-git-core-queue-key root)
     (list :directory root
           :owner (plist-get job :owner)
           :attempts (mapcar #'tuned-git-core-git-command
                             (plist-get job :attempts))
           :stdin (plist-get job :patch)
           :on-start (plist-get job :on-start)
           :on-done (lambda (output code)
                      (if (zerop code)
                          (message "%s" action)
                        (message "%s failed: %s" action (string-trim output)))))
     (or on-drain (lambda () (tuned-git-core-refresh root))))))

(defun tuned-git-core-write (job)
  "Queue JOB and refresh the repository's status when the queue drains."
  (tuned-git-core-enqueue job))

(provide 'tuned-git-core)
;;; tuned-git-core.el ends here
