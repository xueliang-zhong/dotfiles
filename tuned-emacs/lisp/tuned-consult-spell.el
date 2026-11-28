;;; tuned-consult-spell.el --- Async ispell in a consult list -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1") (consult "3.0"))
;; Keywords: tools, spell

;;; Commentary:

;; A consult front-end over the ispell-compatible spell checker that is
;; installed: aspell, hunspell or ispell.  The checker runs
;; asynchronously on the buffer text in list mode, and its words are
;; turned into a consult list.  The list can be narrowed, previewed and
;; jumped to with RET.
;;
;; List mode reports words without positions.  The words are found again
;; in the buffer in the order the checker printed them.  A word that
;; cannot be found is skipped; the rest keep their positions.  A
;; correction is made with the checker's own UI (`ispell-word') at the
;; selected occurrence, not with a correction candidate.
;;
;; The supported scopes are the current buffer and the active region.
;; There is no project scan: one checker process per file is slow and
;; the reconstructed positions become less reliable across many files.
;;
;; Everything uses argument lists rather than shell strings,
;; `executable-find', `temporary-file-directory' indirectly through
;; `tuned-async', and UTF-8 process coding.  There is no `sh -c', `xargs',
;; `grep', `/tmp' or unexpanded "~".  See doc/tuned-consult-spell.md.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'consult)
(require 'tuned-async)
(require 'tuned-spinner)

(defvar ispell-program-name)

(defgroup tuned-consult-spell nil
  "Asynchronous spell checking with a consult interface."
  :group 'tools
  :prefix "tuned-consult-spell-")

(defcustom tuned-consult-spell-programs
  '("aspell" "hunspell" "ispell")
  "Programs tried when `ispell-program-name' is not usable.
The variable `ispell-program-name' is tried first when it is set."
  :type '(repeat string)
  :group 'tuned-consult-spell)

(defvar tuned-consult-spell--history nil
  "Input history for `tuned-consult-spell' commands.")

;;; Program

(defun tuned-consult-spell--program ()
  "Return the absolute path of the first available checker, or nil."
  (seq-some #'executable-find
            (delete-dups
             (delq nil
                   (cons (bound-and-true-p ispell-program-name)
                         tuned-consult-spell-programs)))))

(defun tuned-consult-spell--command ()
  "Return the list-mode command for the selected program."
  (let* ((program (tuned-consult-spell--program))
         (name (file-name-sans-extension
                (file-name-nondirectory program))))
    (cons program
          (pcase name
            ("aspell" '("list" "--encoding=utf-8"))
            ("hunspell" '("-l" "-i" "utf-8"))
            (_ '("-l"))))))

(defun tuned-consult-spell--install-hint ()
  "Return a platform-neutral installation hint."
  (concat
   "Install an ispell-compatible spell checker:\n"
   "  macOS:   brew install aspell\n"
   "  Linux:   sudo apt install aspell aspell-en (or hunspell)\n"
   "  Windows: install Aspell or Hunspell and put its .exe on PATH;\n"
   "           set `ispell-program-name' if Emacs does not find it\n"
   "M-x tuned-consult-spell-doctor shows what Emacs found."))

(defun tuned-consult-spell--missing ()
  "Signal a user-error explaining that no checker was found."
  (user-error "No usable spell checker.\n%s"
              (tuned-consult-spell--install-hint)))

;;; Words and positions

(defun tuned-consult-spell--lines (output)
  "Split OUTPUT into lines, removing a trailing carriage return.
Windows tools write CRLF even when Emacs asks for UTF-8; the checker
output must not keep the CR in a word."
  (mapcar (lambda (line) (string-remove-suffix "\r" line))
          (split-string output "\n" t)))

(defun tuned-consult-spell--whole-lines (beg end)
  "Return BEG and END widened to whole lines."
  (save-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (list beg end)))

(defun tuned-consult-spell--locate-words (words buffer beg end)
  "Return entries for WORDS between BEG and END in BUFFER.
WORDS must be in the order the checker printed them.  A word that
cannot be found is skipped; the rest keep their positions."
  (with-current-buffer buffer
    (save-excursion
      (goto-char beg)
      (let ((case-fold-search t)
            entries)
        (dolist (word words)
          (let ((from (point))
                (re (concat "\\_<" (regexp-quote word) "\\_>")))
            (if (re-search-forward re end t)
                (let ((pos (match-beginning 0)))
                  (push (list :buffer buffer
                              :pos pos
                              :line (line-number-at-pos pos)
                              :word word
                              :display (buffer-name buffer))
                        entries))
              ;; A word made only of punctuation has no word boundaries.
              (goto-char from)
              (when (search-forward word end t)
                (let ((pos (match-beginning 0)))
                  (push (list :buffer buffer
                              :pos pos
                              :line (line-number-at-pos pos)
                              :word word
                              :display (buffer-name buffer))
                        entries))))))
        (nreverse entries)))))

(defun tuned-consult-spell--entry-candidate (entry)
  "Return a propertized consult candidate for ENTRY."
  (let ((text (format "%s:%d: %s"
                      (or (plist-get entry :display) "")
                      (or (plist-get entry :line) 0)
                      (or (plist-get entry :word) ""))))
    (propertize text
                'tuned-consult-spell-entry entry
                'consult--group (plist-get entry :display))))

;;; Consult front-end

(defun tuned-consult-spell--lookup (selected candidates &rest _)
  "Return the entry for SELECTED from CANDIDATES.
Consult strips text properties from the candidate before preview and
before returning it, so the entry cannot be read from SELECTED.  Find
the original, propertized candidate in CANDIDATES instead."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'tuned-consult-spell-entry candidate)))

(defun tuned-consult-spell--entry-position (data)
  "Return the marker stored in DATA, or nil.
DATA is an entry plist from `tuned-consult-spell--lookup', or a candidate
string when called directly.  Consult calls the state function with an
empty string when the minibuffer preview exits; `get-text-property'
would signal `args-out-of-range' on that string."
  (let ((entry (if (and (stringp data) (> (length data) 0))
                   (get-text-property 0 'tuned-consult-spell-entry data)
                 data)))
    (when (and (listp entry)
               (plist-get entry :buffer)
               (plist-get entry :pos))
      (let ((buffer (plist-get entry :buffer))
            (pos (plist-get entry :pos)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (copy-marker pos)))))))

(defun tuned-consult-spell--state ()
  "Return the consult state function for spell candidates."
  (let ((jump (consult--jump-state)))
    (lambda (action data)
      (funcall jump action
               (tuned-consult-spell--entry-position data)))))

(defun tuned-consult-spell--read-entries (entries prompt)
  "Read one of ENTRIES with PROMPT."
  (if (null entries)
      (message "No misspellings found")
    (consult--read
     (mapcar #'tuned-consult-spell--entry-candidate entries)
     :prompt prompt
     :state (tuned-consult-spell--state)
     :lookup #'tuned-consult-spell--lookup
     :category 'tuned-consult-spell
     :group #'consult--prefix-group
     :sort nil
     :require-match t
     :history '(:input tuned-consult-spell--history))))

;;; Asynchronous scan

(defun tuned-consult-spell--scan (buffer beg end on-done on-error)
  "Scan BUFFER between BEG and END, then call ON-DONE with entries.
ON-ERROR receives a message when the checker exits non-zero."
  (let ((command (tuned-consult-spell--command))
        (text (buffer-substring-no-properties beg end)))
    (tuned-async-run
     :name "tuned-consult-spell"
     :command command
     :directory (buffer-local-value 'default-directory buffer)
     :owner buffer
     :stdin text
     :slot 'spell
     :on-done
     (lambda (output code)
       (if (zerop code)
           (funcall on-done
                    (tuned-consult-spell--locate-words
                     (tuned-consult-spell--lines output) buffer beg end))
         (funcall on-error
                  (format "%s exited %d: %s"
                          (car command) code (string-trim output))))))))

(defun tuned-consult-spell--start (beg end)
  "Scan the current buffer between BEG and END."
  (let* ((buffer (current-buffer))
         (program (tuned-consult-spell--program))
         (name (and program
                    (file-name-sans-extension
                     (file-name-nondirectory program))))
         (prompt (format "Spell [%s]: " (or name "ispell"))))
    (unless program
      (tuned-consult-spell--missing))
    (tuned-spinner-mode-line-start "spell")
    (condition-case err
        (tuned-consult-spell--scan
         buffer beg end
         (lambda (entries)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (tuned-spinner-mode-line-stop)))
           (tuned-consult-spell--read-entries entries prompt))
         (lambda (message)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (tuned-spinner-mode-line-stop))
             (message "%s" message))))
      (error
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (tuned-spinner-mode-line-stop)))
       (signal (car err) (cdr err))))))

;;; Entry points

;;;###autoload
(defun tuned-consult-spell ()
  "Spell-check the current buffer asynchronously in a consult list.
The checker runs in list mode; its words become consult candidates.
RET jumps to an occurrence; correct it with `ispell-word'."
  (interactive)
  (tuned-consult-spell--start (point-min) (point-max)))

;;;###autoload
(defun tuned-consult-spell-region (beg end)
  "Spell-check the region between BEG and END."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((lines (tuned-consult-spell--whole-lines beg end)))
    (tuned-consult-spell--start (car lines) (cadr lines))))

;;; Doctor

(defun tuned-consult-spell--version (program &rest args)
  "Return the version output of PROGRAM, or \"unknown\"."
  (condition-case nil
      (string-trim
       (with-temp-buffer
         (apply #'call-process program nil t nil args)
         (buffer-string)))
    (error "unknown")))

(defun tuned-consult-spell--doctor-line (name program &rest args)
  "Return a doctor line for NAME and PROGRAM."
  (if-let* ((path (and program (executable-find program))))
      (format "%-10s %s\n           %s"
              name path
              (replace-regexp-in-string
               "\n" "\n           "
               (apply #'tuned-consult-spell--version path args)))
    (format "%-10s not found" name)))

;;;###autoload
(defun tuned-consult-spell-doctor ()
  "Show which ispell-compatible spell checker Emacs can use."
  (interactive)
  (let ((buffer (get-buffer-create "*tuned-consult-spell-doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "tuned-consult-spell doctor\n\n")
        (insert (tuned-consult-spell--doctor-line
                 "ispell" (tuned-consult-spell--program) "--version")
                "\n\n")
        (insert (format "backend    %s\n"
                        (if-let* ((program (tuned-consult-spell--program)))
                            (file-name-nondirectory program)
                          "none")))
        (insert "scope      buffer, region\n\n")
        (insert (tuned-consult-spell--install-hint) "\n"))
      (special-mode))
    (display-buffer buffer)))

(provide 'tuned-consult-spell)

;;; tuned-consult-spell.el ends here
