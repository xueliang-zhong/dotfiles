;;; xl-dired.el --- Async dired-style browser with async previews -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))
;; Keywords: files, convenience

;;; Commentary:
;; `xl-dired' keeps the look and keys of Dired, but every listing comes from
;; an asynchronous `ls' process.  Opening a directory shows an empty window
;; with the same animated loader as GBlame in the middle of the window and in
;; the mode-line slot; a refresh keeps the old listing on screen until the new
;; one arrives, with only the mode-line loader.  Nothing about listing a
;; directory blocks Emacs.
;;
;; The buffer is a derived `dired-mode', so all of Dired's file operations,
;; marks, sorting and font-lock still work.  Only the reading of a directory
;; is replaced: `revert-buffer-function' points at the async loader, so `g'
;; and Dired's own refresh paths are asynchronous too.
;;
;; Navigation follows the old configuration:
;;   h           up a directory, in this window
;;   l / RET     open the file in the other window, or cd here for a directory
;;   TAB         preview the entry at point in the other window
;;
;; Pointing at a file for `xl-dired-preview-idle' seconds (0.3 s by default)
;; starts the same preview.  File previews run `file' and then `head' in the
;; background, so they are bounded work even for a huge file.  Folders, `.'
;; and `..' have no preview: there is nothing to show, and entering them is
;; what RET and `l' do.  The preview buffer carries the GBlame spinner while a
;; file loads.  A byte-compiled
;; `.elc' file is shown as text, because its comments and docstrings are still
;; readable.  A binary file or a special file says "No preview available"
;; instead of dumping bytes into the buffer.  The preview is a view, never a
;; place to edit: selecting its window replaces it with the entry's own buffer,
;; so point and typing land in the file.  If the entry has disappeared, the
;; preview is emptied rather than left with stale text that could be mistaken
;; for the file.

;;; Code:

(require 'cl-lib)        ; cl-letf around `insert-directory'
(require 'dired)
(require 'seq)           ; seq-find, seq-take
(require 'subr-x)        ; string-trim, string-empty-p
(require 'xl-async)     ; owned processes
(require 'xl-spinner)   ; the loader
(require 'evil nil t)   ; optional: extra normal-state bindings

(defgroup xl-dired nil
  "Asynchronous Dired-style file browser."
  :group 'dired
  :group 'convenience)

(defcustom xl-dired-listing-switches "-alh"
  "Switches passed to `ls' when `xl-dired' lists a directory.
Human-readable sizes and hidden files are on by default."
  :type 'string
  :group 'xl-dired)

(defcustom xl-dired-preview-lines 400
  "Number of lines shown in an `xl-dired' preview."
  :type 'natnum
  :group 'xl-dired)

(defcustom xl-dired-preview-bytes 262144
  "Maximum number of bytes read for one preview.
`head' stops after this many bytes, so a very long single line cannot
make Emacs wait for the whole file."
  :type 'natnum
  :group 'xl-dired)

(defcustom xl-dired-preview-idle 60
  "Seconds point must rest on an entry before its preview starts."
  :type 'number
  :group 'xl-dired)

(defcustom xl-dired-hex-bytes 512
  "Bytes shown, as a hex dump, when a preview is of a binary file.
The dump is bounded like the text preview, so a huge binary costs the same."
  :type 'natnum
  :group 'xl-dired)

(defcustom xl-dired-preview-timeout 5
  "Seconds a preview process may run before `xl-async' stops it."
  :type 'number
  :group 'xl-dired)

(defcustom xl-dired-preview-buffer-name "*xl-dired-preview*"
  "Name of the buffer `xl-dired' previews files in."
  :type 'string
  :group 'xl-dired)

(defface xl-dired-preview-note
  '((t :inherit font-lock-comment-face))
  "Face for messages such as \"No preview available\"."
  :group 'xl-dired)

;;; ============================================================
;;; Dired buffer: async listing, same look
;;; ============================================================

(defvar-local xl-dired--blank nil
  "Non-nil while this Dired buffer is still waiting for its first listing.")

(defvar-local xl-dired--center nil
  "Overlay drawing the loader in the middle of a blank Dired window.")

(defvar-local xl-dired--hover-timer nil
  "Idle timer that starts the preview at point, or nil.")

(defvar-local xl-dired--preview-file nil
  "Entry whose preview this Dired buffer last requested.")

(defun xl-dired--directory ()
  "Return this buffer's directory as a directory name."
  (file-name-as-directory (expand-file-name default-directory)))

(defun xl-dired--draw-center (frame)
  "Draw FRAME in the middle of this Dired window while it is blank.
nil, or a buffer that already has a listing, removes it.  The loader is
a `before-string' on an overlay at `point-min', with newlines that bring
it down to the middle row; it adds no text to the listing."
  (if (not (and frame xl-dired--blank))
      (when (overlayp xl-dired--center)
        (delete-overlay xl-dired--center)
        (setq xl-dired--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  loading directory…"))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (rows (max 0 (/ (window-body-height window) 2))))
        (unless (overlayp xl-dired--center)
          (setq xl-dired--center (make-overlay (point-min) (point-min))))
        (overlay-put xl-dired--center 'before-string
                     (concat (make-string rows ?\n)
                             (make-string pad ?\s)
                             text))))))

(defun xl-dired--start-spinner ()
  "Show this Dired buffer's loader until its listing arrives.
A first load also draws the loader in the middle of the empty window,
like GBlame; a refresh keeps the old listing visible."
  (setq xl-dired--blank (zerop (buffer-size)))
  (xl-spinner-mode-line-start "dired"
                              :also (and xl-dired--blank
                                         #'xl-dired--draw-center)))

(defun xl-dired--stop-spinner ()
  "Remove this Dired buffer's loaders after a render."
  (setq xl-dired--blank nil)
  (xl-dired--draw-center nil)
  (xl-spinner-mode-line-stop))

(defun xl-dired--insert-listing (output)
  "Insert `ls' OUTPUT as a Dired listing in this buffer.
Dired's own insertion code adds the header, text properties and the
directory alist; only the call to `insert-directory', which would run
`ls' synchronously, is replaced with the already collected OUTPUT."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (dired-use-ls-dired nil)
        (directory (xl-dired--directory)))
    (erase-buffer)
    (cl-letf (((symbol-function 'insert-directory)
               (lambda (&rest _) (insert output))))
      (dired-insert-directory directory dired-actual-switches nil nil t))
    (setq dired-subdir-alist nil)
    (dired-build-subdir-alist)
    (set-buffer-modified-p nil)))

(defun xl-dired--render (output)
  "Replace this Dired buffer with the directory listing in OUTPUT."
  (let ((positions (ignore-errors (dired-save-positions)))
        (marks (ignore-errors (dired-remember-marks (point-min) (point-max)))))
    (xl-dired--insert-listing output)
    (when marks (ignore-errors (dired-mark-remembered marks)))
    (when positions (ignore-errors (dired-restore-positions positions)))
    (when (fboundp 'dired--align-all-files)
      (ignore-errors (dired--align-all-files)))
    (ignore-errors
      (set-visited-file-modtime
       (file-attribute-modification-time (file-attributes (xl-dired--directory)))))
    (run-hooks 'dired-after-readin-hook)
    (xl-dired--stop-spinner)
    (force-mode-line-update)))

(defun xl-dired--fail (output)
  "Report a failed `ls' in this Dired buffer, showing OUTPUT."
  (let* ((text (string-trim output))
         (reason (if (string-empty-p text)
                     "unknown error"
                   (car (split-string text "\n")))))
    (xl-spinner-mode-line-stop
     (propertize (format "dired failed: %s" reason) 'face 'error))))

(defun xl-dired--ls-command ()
  "Return the asynchronous `ls' command for this buffer."
  (append (list (or insert-directory-program "ls"))
          (split-string-and-unquote (or dired-actual-switches
                                        xl-dired-listing-switches))
          (list "--" (xl-dired--directory))))

(defun xl-dired--ls-missing-p ()
  "Non-nil when there is no `ls' to list with (Windows without Git tools)."
  (not (executable-find (or insert-directory-program "ls"))))

(defun xl-dired--load ()
  "Start an asynchronous listing of this buffer's directory.
The buffer keeps its old contents, if any, until `ls' answers; a first
load is empty and shows the loader in both the mode line and its middle."
  (when (xl-dired--ls-missing-p)
    (user-error "xl-dired needs `ls' (on Windows, Git for Windows provides it)"))
  (xl-dired--start-spinner)
  (xl-async-run
   :name "xl-dired"
   :command (xl-dired--ls-command)
   :directory (xl-dired--directory)
   :slot 'listing
   :on-done (lambda (output code)
              (if (zerop code)
                  (xl-dired--render output)
                (xl-dired--fail output)))))

(defun xl-dired--revert (&optional _arg _noconfirm)
  "Refresh this Dired buffer without blocking; it is `revert-buffer-function'."
  (xl-dired--load))

(defun xl-dired--teardown ()
  "Forget this Dired buffer's timers, loader and process."
  (when (timerp xl-dired--hover-timer)
    (cancel-timer xl-dired--hover-timer))
  (setq xl-dired--hover-timer nil)
  (when (overlayp xl-dired--center)
    (delete-overlay xl-dired--center)
    (setq xl-dired--center nil))
  (xl-spinner-stop)
  (xl-async-cancel 'listing))

;;; ============================================================
;;; Navigation: cd here, open and preview over there
;;; ============================================================

(defun xl-dired--entry-at-point ()
  "Return the file or directory on the current line, or nil."
  (when (derived-mode-p 'xl-dired-mode)
    (when-let* ((file (ignore-errors (dired-get-filename nil t))))
      (expand-file-name file))))

(defun xl-dired--buffer-name (directory)
  "Return a buffer name for DIRECTORY."
  (format "*xl-dired: %s*" (abbreviate-file-name directory)))

;;;###autoload
(defun xl-dired-noselect (directory)
  "Return an `xl-dired-mode' buffer for DIRECTORY without selecting it.
The listing starts asynchronously.  An existing `xl-dired' buffer for
the same directory is reused."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (or (seq-find
                      (lambda (candidate)
                        (with-current-buffer candidate
                          (and (derived-mode-p 'xl-dired-mode)
                               (equal (xl-dired--directory) directory))))
                      (buffer-list))
                     (generate-new-buffer (xl-dired--buffer-name directory)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'xl-dired-mode)
        (setq default-directory directory)
        (xl-dired-mode))
      (setq default-directory directory
            dired-directory directory
            dired-actual-switches xl-dired-listing-switches
            list-buffers-directory directory)
      (dired-sort-set-mode-line)
      (xl-dired--load))
    buffer))

(defun xl-dired--cd (directory)
  "Show DIRECTORY in the selected window, reusing this Dired buffer.
This is the `cd' of `xl-dired': moving between directories never adds a
window and does not leave a trail of buffers behind."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (if (not (derived-mode-p 'xl-dired-mode))
        (switch-to-buffer (xl-dired-noselect directory))
      (let ((old (xl-dired--directory)))
        (unless (equal old directory)
          (dired-unadvertise old)
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq dired-subdir-alist nil
                xl-dired--preview-file nil
                default-directory directory
                dired-directory directory
                dired-actual-switches xl-dired-listing-switches
                list-buffers-directory directory)
          (dired-advertise)
          (rename-buffer (xl-dired--buffer-name directory) t))
        (dired-sort-set-mode-line)
        (xl-dired--load)))))

;;;###autoload
(defun xl-dired (directory)
  "Visit DIRECTORY in this window with asynchronous Dired listings."
  (interactive (list (read-directory-name "XL Dired: " nil nil t)))
  (xl-dired--cd (or directory default-directory)))

;;;###autoload
(defun xl-dired-jump ()
  "Open the current file's directory, or this Dired buffer's directory."
  (interactive)
  (xl-dired (or (and buffer-file-name (file-name-directory buffer-file-name))
                default-directory)))

;;;###autoload
(defun xl-dired-up-directory ()
  "Move to the parent directory in this window."
  (interactive)
  (xl-dired--cd (file-name-directory
                 (directory-file-name (xl-dired--directory)))))

(defun xl-dired--open-other-window (file)
  "Visit FILE in another window and select it."
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window display-buffer-use-some-window)
           (inhibit-same-window . t))))
    (find-file-other-window file)))

;;;###autoload
(defun xl-dired-open ()
  "Open the entry at point.
A file goes to the other window.  A directory is `cd' in this window."
  (interactive)
  (let ((entry (xl-dired--entry-at-point)))
    (unless entry (user-error "No file or directory on this line"))
    (if (file-directory-p entry)
        (xl-dired--cd entry)
      (xl-dired--open-other-window entry))))

;;; ============================================================
;;; Preview buffer: bounded async head, GBlame loader
;;; ============================================================

(defvar-local xl-dired-preview--file nil
  "Entry this preview buffer shows, or is loading.")

(defvar-local xl-dired-preview--blank nil
  "Non-nil while this preview buffer has no text yet.")

(defvar-local xl-dired-preview--center nil
  "Overlay drawing the loader in the middle of a blank preview window.")

(defun xl-dired-preview--buffer ()
  "Return the preview buffer, creating it if needed."
  (let ((buffer (get-buffer-create xl-dired-preview-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'xl-dired-preview-mode)
        (xl-dired-preview-mode)))
    buffer))

(defun xl-dired-preview--draw-center (frame)
  "Draw FRAME in the middle of this blank preview window; nil clears it."
  (if (not (and frame xl-dired-preview--blank))
      (when (overlayp xl-dired-preview--center)
        (delete-overlay xl-dired-preview--center)
        (setq xl-dired-preview--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  loading preview…"))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (rows (max 0 (/ (window-body-height window) 2))))
        (unless (overlayp xl-dired-preview--center)
          (setq xl-dired-preview--center (make-overlay (point-min) (point-min))))
        (overlay-put xl-dired-preview--center 'before-string
                     (concat (make-string rows ?\n)
                             (make-string pad ?\s)
                             text))))))

(defun xl-dired-preview--start-spinner (entry)
  "Show this preview buffer's loader, labelled with ENTRY."
  (setq xl-dired-preview--blank (zerop (buffer-size)))
  (xl-spinner-mode-line-start
   (format "preview %s" (file-name-nondirectory (directory-file-name entry)))
   :also (and xl-dired-preview--blank #'xl-dired-preview--draw-center)))

(defun xl-dired-preview--stop-spinner ()
  "Remove this preview buffer's loaders."
  (setq xl-dired-preview--blank nil)
  (xl-dired-preview--draw-center nil)
  (xl-spinner-mode-line-stop))

(defun xl-dired-preview--empty ()
  "Leave this preview buffer blank.
A preview with no entry must not keep showing text that could be taken
for the file the user thinks they are editing.  Any load still running
is cancelled, so it cannot fill the buffer again after this."
  (xl-async-cancel 'preview)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (set-buffer-modified-p nil))
  (goto-char (point-min))
  (xl-dired-preview--stop-spinner))

(defun xl-dired-preview--entry-buffer (entry)
  "Return the buffer visiting the file ENTRY.  Folders are never previewed."
  (find-file-noselect entry))

(defun xl-dired-preview--enter (&optional window)
  "Show this preview's entry in WINDOW once WINDOW is selected.
WINDOW defaults to the selected window.  The preview buffer is only a
view, so selecting its window opens the entry's own buffer instead and
puts point there.  If the entry cannot be found, or cannot be opened,
the preview is emptied rather than left showing stale text."
  (let ((window (or window (selected-window))))
    (when (and (window-live-p window)
               (eq window (selected-window))
               (eq (window-buffer window) (current-buffer))
               (derived-mode-p 'xl-dired-preview-mode))
      (let ((target (and xl-dired-preview--file
                         (file-exists-p xl-dired-preview--file)
                         (file-regular-p xl-dired-preview--file)
                         (condition-case nil
                             (xl-dired-preview--entry-buffer
                              xl-dired-preview--file)
                           (error nil)))))
        (if (buffer-live-p target)
            (let ((preview (current-buffer)))
              (switch-to-buffer target t)
              ;; The preview is no longer the window's view; drop its text so
              ;; a later preview cannot start by showing this entry again.
              (when (buffer-live-p preview)
                (with-current-buffer preview
                  (xl-dired-preview--empty))))
          (xl-dired-preview--empty))))))

(defun xl-dired-preview--show (text &optional note)
  "Replace the preview buffer's text with TEXT; NOTE marks it as a message.
Works on the preview buffer whatever buffer is current."
  (with-current-buffer (xl-dired-preview--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (if note (propertize text 'face 'xl-dired-preview-note) text))
      (unless (or (bolp) (string-empty-p text))
        (insert "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (xl-dired-preview--stop-spinner)))

(defun xl-dired-preview--no-preview (reason)
  "Say that no preview is available, because of REASON."
  (xl-dired-preview--show
   (format "No preview available (%s)" reason)
   t))

(defun xl-dired-preview--text-mime-p (mime)
  "Return non-nil when MIME names a text-like file.
`file' reports Lisp, shell and JSON under `application/...' on some
systems, so those types are listed explicitly."
  (or (string-prefix-p "text/" mime)
      (member mime '("application/json"
                     "application/xml"
                     "application/javascript"
                     "application/x-javascript"
                     "application/ecmascript"
                     "application/x-shellscript"
                     "application/x-sh"
                     "application/x-empty"
                     "inode/x-empty"
                     "application/x-subrip"
                     "application/x-yaml"
                     "application/yaml"
                     "application/toml"))))

(defun xl-dired-preview--binary-text-p (text)
  "Return non-nil when TEXT looks binary despite what `file' said.
A NUL byte is binary.  So is a text in which more than one control
character in twenty is not a tab, newline, return, form feed or
backspace.  The count stops as soon as it passes the limit, and the
scan is a regexp search, so a 256 KB preview costs milliseconds."
  (or (and (string-search "\0" text) t)
      (let* ((total (length text))
             (limit (/ total 20))
             (controls 0)
             (pos 0))
        (and (> total 0)
             (progn
               (while (and (<= controls limit)
                           (string-match "[\x01-\x07\x0b\x0e-\x1f]" text pos))
                 (setq controls (1+ controls)
                       pos (match-end 0)))
               (> controls limit))))))

(defun xl-dired-preview--byte-code-file-p (file)
  "Return non-nil when FILE is Emacs Lisp byte code.
A `.elc' file is written to be read: its comments and docstrings stay
text even though the byte code part contains control bytes.  So it gets
a text preview instead of a hex dump."
  (string-suffix-p ".elc" file t))

(defun xl-dired-preview--clip (text)
  "Return the first `xl-dired-preview-lines' lines of TEXT, bounded.
A line cut short gets an ellipsis.  A limit of 0 or less keeps nothing."
  (let* ((limit (max 0 xl-dired-preview-lines))
         (lines (split-string text "\n"))
         (kept (seq-take lines limit)))
    (cond
     ((zerop limit) "")
     ((> (length lines) limit)
      (mapconcat #'identity
                 (append (butlast kept) (list (concat (car (last kept)) "…")))
                 "\n"))
     (t (mapconcat #'identity kept "\n")))))

(defun xl-dired-preview--read-bytes (file count)
  "The first COUNT bytes of FILE, unibyte, read inside Emacs.
Bounded by COUNT, so it is cheap; used where no external program exists."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally file nil 0 count))
    (buffer-string)))

(defun xl-dired-preview--text-result (file output code)
  "Show the text preview of FILE from OUTPUT, read with exit CODE."
  (cond
   ((not (zerop code))
    (xl-dired-preview--no-preview "could not read the file"))
   ((string-empty-p output)
    (xl-dired-preview--show "(empty file)" t))
   ((and (not (xl-dired-preview--byte-code-file-p file))
         (xl-dired-preview--binary-text-p output))
    (xl-dired-preview--load-hex file))
   (t
    (xl-dired-preview--show (xl-dired-preview--clip output)))))

(defun xl-dired-preview--load-text (file)
  "Preview FILE's first bytes as text.
`head' runs asynchronously when it exists (Unix, macOS).  Without it, as
on Windows, Emacs reads the same bounded number of bytes itself."
  (if-let* ((head (executable-find "head")))
      (xl-async-run
       :name "xl-dired-preview"
       :command (list head "-c" (number-to-string xl-dired-preview-bytes) "--" file)
       :slot 'preview
       :timeout xl-dired-preview-timeout
       :on-done (lambda (output code)
                  (xl-dired-preview--text-result file output code)))
    (let ((bytes (condition-case nil
                     (xl-dired-preview--read-bytes file xl-dired-preview-bytes)
                   (error nil))))
      (if bytes
          (xl-dired-preview--text-result file bytes 0)
        (xl-dired-preview--no-preview "could not read the file")))))

(defun xl-dired-preview--prepare (entry)
  "Show the preview buffer for ENTRY and start its loader.
Return the preview buffer, current and visible in the other window."
  (let ((buffer (xl-dired-preview--buffer)))
    (with-current-buffer buffer
      (setq xl-dired-preview--file entry)
      (xl-dired-preview--start-spinner entry))
    (display-buffer buffer
                    '((display-buffer-reuse-window
                       display-buffer-use-some-window)
                      (inhibit-same-window . t)))
    buffer))

(defun xl-dired-preview--hex-command (file)
  "The command for a hex dump of the first bytes of FILE, or nil.
`hexdump -C' gives an ASCII column too; `od' is the fallback, hex only.
Without either (Windows), `xl-dired-preview--hex-lines' makes the dump."
  (let ((count (number-to-string xl-dired-hex-bytes)))
    (cond
     ((executable-find "hexdump")
      (list (executable-find "hexdump") "-C" "-n" count "--" file))
     ((executable-find "od")
      (list (executable-find "od") "-A" "x" "-t" "x1" "-v" "-N" count "--" file)))))

(defun xl-dired-preview--hex-lines (bytes)
  "A hex dump of unibyte BYTES, 16 per line, with an ASCII column.
Made in Emacs, so it needs no external program."
  (let ((offset 0) (lines nil))
    (while (< offset (length bytes))
      (let* ((end (min (length bytes) (+ offset 16)))
             (chunk (substring bytes offset end))
             (hex (mapconcat (lambda (c) (format "%02x" c)) chunk " "))
             (ascii (apply #'string
                           (mapcar (lambda (c) (if (<= 32 c 126) c ?.)) chunk))))
        (push (format "%08x  %-47s  |%s|" offset hex ascii) lines)
        (setq offset end)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun xl-dired-preview--show-hex (bytes)
  "Show BYTES as a hex dump, with the note that says what it is."
  (xl-dired-preview--show
   (format "Binary file: first %d bytes in hex\n%s"
           xl-dired-hex-bytes (xl-dired-preview--hex-lines bytes))
   t))

(defun xl-dired-preview--load-hex (file)
  "Show FILE as a bounded hex dump, with a note saying so.
A dump program (`hexdump' or `od') runs asynchronously when present;
otherwise Emacs reads the bytes and makes the dump itself.  A file that
cannot be read says so instead."
  (if-let* ((command (xl-dired-preview--hex-command file)))
      (xl-async-run
       :name "xl-dired-hex"
       :command command
       :slot 'preview
       :timeout xl-dired-preview-timeout
       :on-done (lambda (output code)
                  (if (or (not (zerop code)) (string-empty-p output))
                      (xl-dired-preview--no-preview "could not read the file")
                    (xl-dired-preview--show
                     (format "Binary file: first %d bytes in hex\n%s"
                             xl-dired-hex-bytes output)
                     t))))
    (let ((bytes (condition-case nil
                     (xl-dired-preview--read-bytes file xl-dired-hex-bytes)
                   (error nil))))
      (if bytes
          (xl-dired-preview--show-hex bytes)
        (xl-dired-preview--no-preview "could not read the file")))))

(defun xl-dired-preview--load-file (file)
  "Preview FILE asynchronously, naming its MIME type first.
A byte-compiled `.elc' file skips the MIME test: its comments and
docstrings are readable, so it is shown as text rather than as data.
Text-like files then run `head' for a bounded number of bytes.  Both
processes are in the same `xl-async' slot, so a newer preview cancels
the older one."
  (xl-dired-preview--prepare file)
  (with-current-buffer (xl-dired-preview--buffer)
    (let ((program (executable-find "file")))
      (cond
       ((xl-dired-preview--byte-code-file-p file)
        (xl-dired-preview--load-text file))
       (program
        (xl-async-run
         :name "xl-dired-mime"
         :command (list program "--mime-type" "-b" "--" file)
         :slot 'preview
         :timeout xl-dired-preview-timeout
         :on-done (lambda (output code)
                    (if (and (zerop code)
                             (not (xl-dired-preview--text-mime-p
                                   (string-trim output))))
                        (xl-dired-preview--load-hex file)
                      (xl-dired-preview--load-text file)))))
       (t
        (xl-dired-preview--load-text file))))))

(defun xl-dired-preview--load (entry)
  "Start loading a preview of the file ENTRY in the preview buffer.
A file first has its MIME type named, then a bounded `head'.  A
byte-compiled `.elc' file skips the MIME test and is read as text.  All
of it is asynchronous.  A folder has no preview, so nothing is loaded."
  (unless (file-directory-p entry)
    (xl-dired-preview--load-file entry)))

(defun xl-dired-preview--revert (&optional _arg _noconfirm)
  "Reload this preview buffer's entry asynchronously."
  (when xl-dired-preview--file
    (xl-dired-preview--load xl-dired-preview--file)))

(defun xl-dired-preview--teardown ()
  "Stop this preview buffer's process and loader."
  (when (overlayp xl-dired-preview--center)
    (delete-overlay xl-dired-preview--center)
    (setq xl-dired-preview--center nil))
  (xl-spinner-stop)
  (xl-async-cancel 'preview))

;;;###autoload
(defun xl-dired-preview-quit ()
  "Close the preview window."
  (interactive)
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (window-live-p window)
        (quit-window nil window)
      (kill-buffer (current-buffer)))))

(defun xl-dired-preview--maybe-load (entry &optional force)
  "Preview the file ENTRY, unless this Dired buffer already previewed it.
A folder is never previewed.  With FORCE, start a fresh preview even for
the same ENTRY.  After the preview window has been turned into the
entry's own window, the idle timer must not replace that file with
another preview, so this check does not depend on the preview window
still being visible."
  (unless (or (file-directory-p entry)
              (and (not force) (equal entry xl-dired--preview-file)))
    (setq xl-dired--preview-file entry)
    (xl-dired-preview--load entry)))

;;;###autoload
(defun xl-dired-preview ()
  "Preview the file at point in the other window, asynchronously.
This is the TAB command.  A file shows its first lines in the preview
buffer, with the GBlame loader while it loads.  A folder, `.' and `..'
have no preview; TAB says so and leaves the windows alone."
  (interactive)
  (let ((entry (xl-dired--entry-at-point)))
    (cond
     ((null entry) (user-error "No file or directory on this line"))
     ((file-directory-p entry)
      (message "No preview for a folder (RET opens it)"))
     (t (xl-dired-preview--maybe-load entry t)))))

(defun xl-dired--idle-preview (buffer)
  "Preview the entry at point in BUFFER after the idle delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq xl-dired--hover-timer nil)
      (when (and (derived-mode-p 'xl-dired-mode)
                 (eq (window-buffer (selected-window)) buffer))
        (when-let* ((entry (xl-dired--entry-at-point)))
          (xl-dired-preview--maybe-load entry))))))

(defun xl-dired--schedule-preview ()
  "Start the idle timer that previews what point rests on.
Every command restarts it, so only a still cursor triggers the load."
  (when (timerp xl-dired--hover-timer)
    (cancel-timer xl-dired--hover-timer))
  (setq xl-dired--hover-timer
        (run-with-idle-timer xl-dired-preview-idle nil
                             #'xl-dired--idle-preview (current-buffer))))

(defun xl-dired--post-command ()
  "Schedule the idle preview after a command in this Dired buffer."
  (when (derived-mode-p 'xl-dired-mode)
    (xl-dired--schedule-preview)))

;;; ============================================================
;;; Modes and keys
;;; ============================================================

(defvar-keymap xl-dired-mode-map
  :doc "Keymap for `xl-dired-mode'."
  :parent dired-mode-map
  "TAB"      #'xl-dired-preview
  "<tab>"    #'xl-dired-preview
  "h"        #'xl-dired-up-directory
  "l"        #'xl-dired-open
  "RET"      #'xl-dired-open
  "<return>" #'xl-dired-open
  "q"        #'quit-window
  "g"        #'revert-buffer)

;;;###autoload
(define-derived-mode xl-dired-mode dired-mode "XL-Dired"
  "Dired with asynchronous listings and asynchronous previews.
The buffer uses Dired's own insertion, faces and file operations; only
reading a directory and refreshing it happen in a subprocess."
  (setq-local revert-buffer-function #'xl-dired--revert)
  (setq-local dired-actual-switches xl-dired-listing-switches)
  (setq-local xl-dired--preview-file nil)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'xl-dired--post-command nil t)
  (add-hook 'kill-buffer-hook #'xl-dired--teardown nil t))

(define-derived-mode xl-dired-preview-mode special-mode "XL-Preview"
  "Mode for the asynchronous preview shown by `xl-dired'.
Selecting the preview window opens the entry itself in that window;
if the entry is gone, the preview is left blank.  `q' closes the
preview window and `g' loads the file again."
  (setq-local revert-buffer-function #'xl-dired-preview--revert)
  ;; Selecting the preview window must land in the real entry.  The window
  ;; hooks catch selection and `switch-to-buffer'; the command hook also
  ;; covers a redisplay that pending input interrupted.
  (add-hook 'post-command-hook #'xl-dired-preview--enter nil t)
  (add-hook 'window-selection-change-functions #'xl-dired-preview--enter nil t)
  (add-hook 'window-buffer-change-functions #'xl-dired-preview--enter nil t)
  (add-hook 'kill-buffer-hook #'xl-dired-preview--teardown nil t))

(defvar-keymap xl-dired-preview-mode-map
  :doc "Keymap for `xl-dired-preview-mode'."
  :parent special-mode-map
  "q" #'xl-dired-preview-quit
  "g" #'revert-buffer)

(with-eval-after-load 'evil
  (evil-set-initial-state 'xl-dired-mode 'normal)
  (evil-set-initial-state 'xl-dired-preview-mode 'normal)
  (evil-define-key 'normal xl-dired-mode-map
    "h"             #'xl-dired-up-directory
    "l"             #'xl-dired-open
    (kbd "RET")     #'xl-dired-open
    (kbd "<return>") #'xl-dired-open
    (kbd "TAB")     #'xl-dired-preview
    "q"             #'quit-window
    "g"             #'revert-buffer)
  (evil-define-key 'normal xl-dired-preview-mode-map
    "q" #'xl-dired-preview-quit
    "g" #'revert-buffer))

(provide 'xl-dired)
;;; xl-dired.el ends here
