;;; git-commands.el --- Vim-fugitive style Git commands -*- lexical-binding: t; -*-

;;; Commentary:
;; The G* commands, in the spirit of vim-fugitive:
;;   Gstatus  show the status buffer from git-status.el
;;   Gblame   toggle the blame window on the left of the current file
;;   Gwrite   stage the current file
;;   Gcommit  commit the current file with an "Update <file>" message
;; All of them are also available as Ex commands (`:Gstatus', `:Gblame',
;; `:Gwrite', `:Gcommit').  The status buffer itself lives in git-status.el.

;;; Code:

(require 'subr-x)     ; truncate-string-to-width
(require 'git-status) ; status buffer, its faces and `--show-commit'

;; ============================================================
;; GBlame - git blame on the left, like vim's :Gblame
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
(defun xueliang-git-commands--file-directory ()
  "Return the current file's directory, or `default-directory'."
  (if buffer-file-name
      (file-name-directory (expand-file-name buffer-file-name))
    default-directory))

(defun Gwrite ()
  "Stage the current file, like vim-fugitive's :Gwrite."
  (interactive)
  (unless (fboundp 'evil-ex-execute)
    (user-error "Gwrite needs Evil"))
  (let ((default-directory (xueliang-git-commands--file-directory)))
    (evil-ex-execute "!git add %")))

(defun Gcommit ()
  "Commit the current file, like vim-fugitive's :Gcommit."
  (interactive)
  (unless (fboundp 'evil-ex-execute)
    (user-error "Gcommit needs Evil"))
  (let ((default-directory (xueliang-git-commands--file-directory)))
    (evil-ex-execute (format "!git commit -m \"Update %s\""
                             (file-name-nondirectory buffer-file-name)))))

(defalias 'Gstatus 'xueliang-git-status)

(with-eval-after-load 'evil
  (evil-ex-define-cmd "Gstatus" #'Gstatus)
  (evil-ex-define-cmd "Gwrite" #'Gwrite)
  (evil-ex-define-cmd "Gcommit" #'Gcommit))

(provide 'git-commands)
;;; git-commands.el ends here
