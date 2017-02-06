; xueliang's
; Git helper functions/commands.

;; for windows to display diff, put them far-right.
;; for windows for the user to do select, put them very-bottom.

(setq-default shell-output-buffer-name "*Shell Command Output*")

(defun xueliang-gcommit-current-file ()
  "run git commit on current file.
   *** HANDLE WITH CARE !!! only used after gwrite & gstatus ***" (interactive)
  (shell-command (message "git commit -m \"improve %s\"" (file-name-nondirectory buffer-file-name))))

(defun xueliang-gstatus ()
  "run git status" (interactive) (shell-command "git status"))

(defun xueliang-gbranch()
  "run git branch" (interactive) (shell-command "git branch"))

(defun xueliang-glog ()
  "run git log" (interactive)
  (shell-command "git log -n 100 --pretty=oneline")
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-very-bottom) (evil-beginning-of-line))

(defun xueliang-gdiff-current-buffer ()
  "run git diff on current buffer;
   use C-M-i to browse diff hunks; C-c C-c to jump to source code." (interactive)
  (xueliang-cd-current-buffer-directory)
  (shell-command (concat "git diff " (buffer-file-name)))
  (if (= (buffer-size (switch-to-buffer-other-window shell-output-buffer-name)) 0)
    (kill-buffer-and-window) ;; kill the buffer that we just switched to, should be the shell output buffer window.
    (evil-window-move-far-right) (diff-mode) (toggle-truncate-lines 1)))

(defun xueliang-gdiff-revision-at-point ()
  "run 'git diff' using the revision number at point.
   workflow: get git revision in output, browse revisions, apply this function." (interactive)
  (if (null (buffer-file-name (current-buffer)))
      (funcall (lambda () ;; already in shell command output buffer.
                 (evil-beginning-of-line)
                 (shell-command (message "git diff %s " (thing-at-point 'word)))
                 (evil-window-move-far-right) (diff-mode) (toggle-truncate-lines 1)))
      (funcall (lambda () ;; in some other file.
                 (xueliang-glog) (message "try apply this function in glog.")))))

(defun xueliang-gshow ()
  "run git show" (interactive)
  (shell-command "git show")
  (switch-to-buffer-other-window shell-output-buffer-name)
  (evil-window-move-far-right) (diff-mode) (evil-end-of-line))

(defun xueliang-gshow-revision-at-point()
  "run 'git show' using the revision number at point.
   workflow: get git revision in output, browse revisions, apply this function." (interactive)
   (if (null (buffer-file-name (current-buffer)))
       (funcall (lambda () ;; already in shell command output buffer.
                  (shell-command (message "git show %s " (thing-at-point 'word)))
                  (evil-window-move-far-right) (diff-mode) (toggle-truncate-lines 1)))
       (funcall (lambda () ;; in some other file.
                  (xueliang-glog) (message "try apply this function in glog.")))))

(defun xueliang-gread-current-buffer ()
  "run git checkout on current buffer" (interactive)
  (shell-command (concat "git checkout " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm)
  (message "git checkout: " (buffer-file-name)))

(defun xueliang-gwrite-current-buffer ()
  "run git add on current buffer" (interactive)
  (shell-command (concat "git add " (buffer-file-name)))
  (xueliang-gread-current-buffer) ;; gread it again to refresh git-gutter.
  (message "git add: %s" (buffer-file-name)))

(defun xueliang-gblame-current-buffer ()
  "run git blame on current buffer, esp. current line" (interactive)
  (setq-local gblame-line (line-number-at-pos))
  (shell-command (concat "git blame " (buffer-file-name)))
  (goto-line gblame-line (switch-to-buffer-other-window shell-output-buffer-name))
  (evil-window-move-very-bottom) (hl-line-mode) (toggle-truncate-lines 1))
