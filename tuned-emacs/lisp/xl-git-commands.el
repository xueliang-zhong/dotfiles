;;; xl-git-commands.el --- Async Vim-fugitive style Git commands  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-git-status "0.1.0"))
;; Keywords: tools, vc, convenience

;;; Commentary:
;; The G* commands, in the spirit of vim-fugitive:
;;   Gstatus  show the status buffer from xl-git-status.el
;;   Gblame   toggle the blame window on the left of the current file
;;   Gwrite   stage the current file
;;   Gcommit  commit the current file with an "Update <file>" message
;; All of them are also available as Ex commands (`:Gstatus', `:Gblame',
;; `:Gwrite', `:Gcommit').  The status buffer itself lives in xl-git-status.el.

;;; Code:

(require 'subr-x)     ; truncate-string-to-width
(require 'xl-git-status) ; status buffer, its faces and `--show-commit'
(require 'xl-async)    ; the loader

;; ============================================================
;; GBlame - git blame on the left, like vim's :Gblame
;; ============================================================
;; `xl-gblame' (SPC g b, or :Gblame) shows one line of blame for
;; every line of the current file in a narrow window on the left:
;; abbreviated commit, author and date, aligned with the file.  Different
;; commits get different hash colours, so the boundaries between commits
;; stand out; uncommitted lines stay dim.  The window follows the source
;; window while scrolling, highlights the line at point, refreshes after
;; saving, and closes with `q'.  RET on a blame line shows that commit.
;;
;; `git blame' runs asynchronously: the window is filled at once with one
;; placeholder line per source line, so scrolling and point still line up,
;; and the mode-line loader of `xl-spinner' says the data is on its way.
;; A refresh keeps the old lines visible until the new ones arrive, so
;; saving never makes the blame window flicker.  The process runs in the
;; GBlame buffer's `blame' slot of `xl-async', so a newer request or a
;; killed buffer throws an older answer away.
(defface xl-gblame-current-line
  '((t :inherit highlight))
  "Face for the line at point in the GBlame window."
  :group 'faces)

(defvar-local xl-gblame--buffer nil
  "GBlame buffer of this source buffer, when GBlame is open.")
(defvar-local xl-gblame--root nil
  "Repository root of the source buffer's file.")
(defvar-local xl-gblame--file nil
  "Absolute name of the file being blamed.")

(defvar-local xl-gblame--source nil
  "Source buffer of this GBlame buffer.")
(defvar-local xl-gblame--highlight nil
  "Overlay marking the source line in this GBlame buffer.")

(defvar-local xl-gblame--blank nil
  "Non-nil while this GBlame buffer holds only placeholder lines.")

(defvar-local xl-gblame--center nil
  "Overlay drawing the loader in the middle of a blank GBlame window.")

(defun xl-gblame--draw-center (frame)
  "Draw FRAME in the middle of this GBlame window while it is blank.
nil, or a buffer that already shows blame, removes it.  The loader is a
`before-string' on an empty placeholder line, so no line is added and
the window stays aligned with the source file."
  (if (not (and frame xl-gblame--blank))
      (when (overlayp xl-gblame--center)
        (delete-overlay xl-gblame--center)
        (setq xl-gblame--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  git blame…"))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (pos (save-excursion
                    (goto-char (window-start window))
                    (forward-line (/ (window-body-height window) 2))
                    (line-beginning-position))))
        (unless (overlayp xl-gblame--center)
          (setq xl-gblame--center (make-overlay pos pos)))
        (move-overlay xl-gblame--center pos pos)
        (overlay-put xl-gblame--center 'before-string
                     (concat (make-string pad ?\s) text))))))

(defun xl-gblame--start-spinner ()
  "Show the loader in this GBlame buffer's mode-line slot.
The blame window is narrow, so the loader sits at the left edge of the
mode line, which is not truncated.  It runs until the blame answers.
While the window is still blank, the same frames are drawn in its middle
too, on the slot's clock."
  (xl-spinner-mode-line-start "git blame" :also #'xl-gblame--draw-center))

(defun xl-gblame--stop-spinner ()
  "End the loader in this GBlame buffer's mode-line slot."
  (xl-spinner-mode-line-stop))

(defun xl-gblame--placeholders (lines)
  "Fill this GBlame buffer with LINES empty lines.
One line per source line keeps scrolling and point aligned while the
blame is still loading.  A later render replaces them."
  (setq xl-gblame--blank t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (_ lines) (insert "\n"))
    (when (overlayp xl-gblame--highlight)
      (delete-overlay xl-gblame--highlight))
    (setq xl-gblame--highlight (make-overlay (point-min) (point-min)))
    (overlay-put xl-gblame--highlight 'face 'xl-gblame-current-line)))

(defun xl-gblame--render (records)
  "Replace this GBlame buffer with blame RECORDS.
Keep point on the same line, so a refresh does not jump."
  ;; The blame is in: the middle loader goes at once, even while the
  ;; mode line keeps its loader for its minimum time.
  (setq xl-gblame--blank nil)
  (xl-gblame--draw-center nil)
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (xl-gblame--insert records)
    (goto-char (point-min))
    (when (overlayp xl-gblame--highlight)
      (delete-overlay xl-gblame--highlight))
    (setq xl-gblame--highlight (make-overlay (point) (point)))
    (overlay-put xl-gblame--highlight 'face 'xl-gblame-current-line)
    (goto-char (point-min))
    (forward-line (1- (max 1 line)))))

(defun xl-gblame--resync ()
  "Align the GBlame window with its source window after a render."
  (when (and (buffer-live-p xl-gblame--source)
             (get-buffer-window (current-buffer) t))
    (with-current-buffer xl-gblame--source
      (xl-gblame--sync (get-buffer-window xl-gblame--source t)))))

(defun xl-gblame--fail (output)
  "Report a failed `git blame' in this GBlame buffer, showing OUTPUT.
The mode line keeps the error, so the message stays visible while the
placeholders remain for alignment."
  (let* ((text (string-trim output))
         (reason (if (string-empty-p text)
                     "unknown error"
                   (car (split-string text "\n")))))
    (xl-spinner-mode-line-stop
     (propertize (format "git blame failed: %s" reason) 'face 'error))))

(defun xl-gblame--teardown ()
  "Stop the loader and the process of this GBlame buffer."
  (xl-gblame--stop-spinner)
  (xl-async-cancel 'blame))

(defun xl-gblame--parse (output)
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

(defun xl-gblame--pad (text width)
  "Return TEXT padded with spaces to WIDTH display columns."
  (concat text (make-string (max 0 (- width (string-width text))) ?\s)))

(defvar xl-gblame-hash-faces
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
`xl-git-status-hash'.")

(defun xl-gblame--format (record face)
  "Return the display line for one blame RECORD.
Its hash is coloured with FACE."
  (pcase-let ((`(,_ ,hash ,author ,time) record))
    (let ((short (substring hash 0 (min 8 (length hash))))
          (author (truncate-string-to-width author 18 nil nil "…"))
          (date (if time (format-time-string "%Y-%m-%d" time) "")))
      (concat (propertize short 'face face)
              " "
              (xl-gblame--pad author 18)
              " "
              (propertize date 'face 'xl-git-status-comment)))))

(defun xl-gblame--insert (records)
  "Insert one line per source line for blame RECORDS.
Different commits get different hash colours, so the boundaries between
commits are easy to see."
  (let ((table (make-hash-table :test #'eql))
        (faces (vconcat xl-gblame-hash-faces))
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
                     'xl-git-status-comment)
                    ((gethash hash face-of))
                    ((zerop (length faces))
                     'xl-git-status-hash)
                    (t
                     (let ((face (aref faces (mod next (length faces)))))
                       (setq next (1+ next))
                       (puthash hash face face-of)
                       face))))
             (start (point)))
        (insert (if record (xl-gblame--format record face) "") "\n")
        (when record
          (add-text-properties start (1- (point))
                               (list 'xl-gblame-hash hash)))))))

(defun xl-gblame--fill (buffer root file)
  "Start filling BUFFER with the blame of FILE in ROOT.
The git process runs asynchronously: the buffer shows one placeholder
line per source line and a loader until the output arrives."
  (with-current-buffer buffer
    (unless (> (buffer-size) 0)
      (xl-gblame--placeholders
       (if (buffer-live-p xl-gblame--source)
           (with-current-buffer xl-gblame--source
             (count-lines (point-min) (point-max)))
         0)))
    (xl-gblame--start-spinner)
    (xl-async-run
     :name "xl-gblame"
     :command (append '("git" "--no-pager" "-c" "core.quotepath=false"
                        "blame" "--line-porcelain" "--")
                      (list (file-relative-name file root)))
     :directory root
     :slot 'blame
     :on-done (lambda (output code)
                (xl-gblame--stop-spinner)
                (if (zerop code)
                    (progn
                      (xl-gblame--render (xl-gblame--parse output))
                      (xl-gblame--resync))
                  (xl-gblame--fail output))))))

(defun xl-gblame--sync (&optional window _new-start)
  "Align the GBlame window with WINDOW and follow its point.
WINDOW defaults to the selected window; `window-scroll-functions' also
passes the new window start."
  (let* ((source-window (or window (selected-window)))
         (source (window-buffer source-window))
         (blame (buffer-local-value 'xl-gblame--buffer source))
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
            (when xl-gblame--highlight
              (goto-char (point-min))
              (forward-line (1- line))
              (move-overlay xl-gblame--highlight
                            (line-beginning-position)
                            (line-end-position)))
            (goto-char (point-min))
            (forward-line (1- top))
            (set-window-start blame-window (point) t)
            (goto-char (point-min))
            (forward-line (1- target))
            (set-window-point blame-window (point))))))))

(defun xl-gblame--refresh ()
  "Redraw this source buffer's GBlame window, if it is open."
  (when (and (buffer-live-p xl-gblame--buffer)
             xl-gblame--root
             xl-gblame--file)
    (xl-gblame--fill xl-gblame--buffer
                           xl-gblame--root xl-gblame--file)
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (xl-gblame--sync window))))

(defun xl-gblame--cleanup ()
  "Forget this GBlame buffer in its source buffer."
  (when (buffer-live-p xl-gblame--source)
    (with-current-buffer xl-gblame--source
      (setq xl-gblame--buffer nil)
      (remove-hook 'post-command-hook #'xl-gblame--sync t)
      (remove-hook 'window-scroll-functions #'xl-gblame--sync t)
      (remove-hook 'after-save-hook #'xl-gblame--refresh t)
      (remove-hook 'kill-buffer-hook #'xl-gblame--close t))))

(defun xl-gblame--close ()
  "Close this source buffer's GBlame window.
Return non-nil when a GBlame buffer was open."
  (when (buffer-live-p xl-gblame--buffer)
    (let ((window (get-buffer-window xl-gblame--buffer t)))
      (when (window-live-p window)
        (delete-window window)))
    (kill-buffer xl-gblame--buffer)
    t))

(defun xl-gblame-quit ()
  "Close the GBlame window."
  (interactive)
  (let ((source xl-gblame--source))
    (if (buffer-live-p source)
        (with-current-buffer source
          (xl-gblame--close))
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (delete-window window))
      (kill-buffer (current-buffer)))))

(defun xl-gblame-refresh ()
  "Re-run git blame for the current GBlame window."
  (interactive)
  (let ((source xl-gblame--source))
    (unless (buffer-live-p source)
      (user-error "The source buffer is gone"))
    (with-current-buffer source
      (xl-gblame--refresh))))

(defun xl-gblame-show-commit ()
  "Show the commit that last touched the line at point."
  (interactive)
  (let ((hash (get-text-property (line-beginning-position)
                                 'xl-gblame-hash)))
    (cond
     ((null hash) (user-error "No blame information on this line"))
     ((string-match-p "\\`0+\\'" hash)
      (user-error "This line is not committed yet"))
     ((fboundp 'xl-git-status--show-commit)
      (let ((default-directory (or xl-gblame--root default-directory)))
        (xl-git-status--show-commit hash)))
     (t (message "Commit %s" hash)))))

(defun xl-gblame ()
  "Toggle a git blame window on the left of the current file.
Each line shows the abbreviated commit, author and date, aligned with
the file; the window follows the file's scrolling and point.  It is an
ordinary split, so `C-w C-w' focuses it and `:only'/`C-w c' close it.
`git blame' runs in the background: the window opens at once with a
loader at the left of its mode line, and saving refreshes it without
blocking.
`RET' shows the commit at point, `g' refreshes, `q' closes the window."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (if (and (buffer-live-p xl-gblame--buffer)
           (get-buffer-window xl-gblame--buffer t))
      (progn
        (xl-gblame--close)
        (message "GBlame closed"))
    ;; The buffer may survive a window closed with :only, C-w c or :q.
    ;; Drop it before opening a fresh one.
    (xl-gblame--close)
    (let* ((source (current-buffer))
           (file (file-truename buffer-file-name))
           (root (locate-dominating-file file ".git")))
      (unless root
        (user-error "Not inside a Git repository"))
      (setq root (file-name-as-directory (expand-file-name root)))
      (let ((blame (get-buffer-create
                    (format "*GBlame: %s*" (file-name-nondirectory file)))))
        (with-current-buffer blame
          (xl-gblame-mode)
          (setq xl-gblame--source source
                xl-gblame--root root
                xl-gblame--file file)
          (add-hook 'kill-buffer-hook #'xl-gblame--cleanup nil t)
          (add-hook 'kill-buffer-hook #'xl-gblame--teardown nil t))
        (xl-gblame--fill blame root file)
        (with-current-buffer source
          (setq xl-gblame--root root
                xl-gblame--file file
                xl-gblame--buffer blame)
          (add-hook 'post-command-hook #'xl-gblame--sync nil t)
          (add-hook 'window-scroll-functions #'xl-gblame--sync nil t)
          (add-hook 'after-save-hook #'xl-gblame--refresh nil t)
          (add-hook 'kill-buffer-hook #'xl-gblame--close nil t))
        (display-buffer blame
                        '((display-buffer-in-direction)
                          (direction . left)
                          (window-width . 44)))
        (with-current-buffer source
          (xl-gblame--sync (get-buffer-window source t)))))))

(defvar-keymap xl-gblame-mode-map
  :doc "Keymap for `xl-gblame-mode'."
  :parent special-mode-map
  "RET" #'xl-gblame-show-commit
  "<return>" #'xl-gblame-show-commit
  "g" #'xl-gblame-refresh
  "q" #'xl-gblame-quit)

(define-derived-mode xl-gblame-mode special-mode "GBlame"
  "Major mode for `xl-gblame' buffers."
  (setq-local truncate-lines t)
  ;; The blame window is narrow: keep the loader at the left edge of the
  ;; mode line, where it is not truncated.
  (xl-spinner-mode-line-install))

;; Evil's normal state would otherwise take RET, g and q first.
(with-eval-after-load 'evil
  (evil-define-key 'normal xl-gblame-mode-map
    (kbd "RET") #'xl-gblame-show-commit
    (kbd "<return>") #'xl-gblame-show-commit
    "g" #'xl-gblame-refresh
    "q" #'xl-gblame-quit)
  ;; Same command as vim-fugitive's :Gblame
  (evil-ex-define-cmd "Gblame" #'xl-gblame))

;; ============================================================
;; Vim-fugitive style commands
;; ============================================================
;; The G* names match vim-fugitive's :Gwrite and :Gcommit, which are also
;; available as Ex commands.  Both act on the current file's directory, so
;; they work from any buffer that visits a tracked file.
(defun xl-git-commands--file-directory ()
  "Return the current file's directory, or `default-directory'."
  (if buffer-file-name
      (file-name-directory (expand-file-name buffer-file-name))
    default-directory))

(defun xl-git-commands--refresh-status (root)
  "Refresh the status buffer showing ROOT, when it is open."
  (when-let* ((repo (file-name-nondirectory (directory-file-name root)))
              (buffer (get-buffer (format "*git-status: %s*" repo))))
    (with-current-buffer buffer
      (xl-git-status))))

(defun xl-git-commands--run (args action)
  "Run git ARGS in the current file's folder in the background.
Message ACTION on success, git's output on failure, and refresh the
status buffer of the same repository.  A pre-commit hook that takes
seconds no longer blocks Emacs.  The run is unowned: closing the file
buffer must not kill a commit halfway through its hooks."
  (let* ((dir (xl-git-commands--file-directory))
         (root (or (locate-dominating-file dir ".git")
                   (file-name-as-directory (expand-file-name dir)))))
    (xl-async-run
     :name "xl-git-command"
     :command (append '("git" "--no-pager" "-c" "core.quotepath=false") args)
     :directory root
     :owner nil
     :on-done (lambda (output code)
                (if (zerop code)
                    (progn
                      (message "%s" action)
                      (xl-git-commands--refresh-status root))
                  (message "%s failed: %s" action (string-trim output)))))))

(defun Gwrite ()
  "Stage the current file, like vim-fugitive's :Gwrite."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (let ((file (expand-file-name buffer-file-name)))
    (xl-git-commands--run (list "add" "--" file)
                          (format "Staged %s" (file-name-nondirectory file)))))

(defun Gcommit ()
  "Commit the current file, like vim-fugitive's :Gcommit."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (let ((name (file-name-nondirectory buffer-file-name)))
    (xl-git-commands--run (list "commit" "-m" (format "Update %s" name))
                          (format "Committed %s" name))))

(defalias 'Gstatus 'xl-git-status)

(with-eval-after-load 'evil
  (evil-ex-define-cmd "Gstatus" #'Gstatus)
  (evil-ex-define-cmd "Gwrite" #'Gwrite)
  (evil-ex-define-cmd "Gcommit" #'Gcommit))

(provide 'xl-git-commands)
;;; xl-git-commands.el ends here
