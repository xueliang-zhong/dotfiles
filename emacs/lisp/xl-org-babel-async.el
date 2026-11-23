;;; xl-org-babel-async.el --- Run shell source blocks asynchronously  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (org "9.6") (xl-async "0.1.0"))
;; Keywords: tools, outlines

;;; Commentary:
;; A shell source block with the header argument `:async yes' runs in the
;; background, so Emacs stays responsive while it works.  Without the
;; argument, a block runs as org-babel always did, and blocks keep their
;; results and order.
;;
;; The output is layered, so its length does not matter:
;;
;; - While the block runs, every chunk of output goes to a buffer of its own,
;;   `*Org Babel Output: FILE:LINE*'.  The results under the block show only
;;   the last `xl-org-babel-live-lines' lines.
;; - When it ends, the results written into the file are the first
;;   `xl-org-babel-result-lines' lines, with a note where the rest is.  The
;;   full output stays in the output buffer.
;;
;; The results are kept by markers, so text added above the block while it
;; runs does not misplace them.  If the block is gone when the output
;; arrives, the output stays in its buffer.  `C-c C-k' cancels the block at
;; point.  Running the same block again cancels the run that is still going.
;;
;; Blocks with a `:session' cannot be asynchronous: they run as usual.
;; Blocks run in parallel, not in a queue: order them yourself if they
;; depend on each other.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ansi-color)
(require 'xl-async)
(require 'xl-spinner)

(declare-function org-babel-get-src-block-info "ob-core")
(declare-function org-babel-where-is-src-block-head "ob-core")
(defvar org-mode-map)

(defgroup xl-org-babel-async nil
  "Asynchronous shell source blocks."
  :group 'org)

(defcustom xl-org-babel-live-lines 20
  "Lines of output shown under a running block."
  :type 'integer
  :group 'xl-org-babel-async)

(defcustom xl-org-babel-result-lines 200
  "Lines of output written into the file when a block ends."
  :type 'integer
  :group 'xl-org-babel-async)

(defcustom xl-org-babel-render-delay 0.2
  "Seconds between two redraws of the live output of a running block."
  :type 'number
  :group 'xl-org-babel-async)

(defconst xl-org-babel-async-languages '("sh" "bash" "shell" "zsh")
  "Source block languages that can run asynchronously.")

(cl-defstruct (xl-org-babel-run
               (:constructor xl-org-babel-run--make))
  "A block running in the background."
  slot head start end out timer overlay)

(defvar-local xl-org-babel--runs nil
  "The blocks of this buffer that are running, as `xl-org-babel-run' objects.")

;; ============================================================
;; The results region
;; ============================================================

(defun xl-org-babel--results-region ()
  "Return (START . END) markers around the `: ' lines of the block at point.
The `#+RESULTS:' line is made if the block has none."
  (save-excursion
    (goto-char (org-babel-where-is-src-block-head))
    (re-search-forward "^[ \t]*#\\+end_src[ \t]*$" nil t)
    (end-of-line)
    (forward-line 1)
    (unless (looking-at "[ \t]*#\\+RESULTS:")
      (when (and (eobp) (not (bolp)))
        (insert "\n"))
      (insert "#+RESULTS:\n")
      (forward-line -1))
    (forward-line 1)
    (let ((start (point)))
      (while (looking-at "[ \t]*:\\( \\|$\\)")
        (forward-line 1))
      (cons (copy-marker start) (copy-marker (point) t)))))

(defun xl-org-babel--format-lines (lines)
  "Return LINES as org fixed-width results, each ending in a newline."
  (mapconcat (lambda (line)
               (if (string-empty-p line) ":\n" (concat ": " line "\n")))
             lines ""))

(defun xl-org-babel--output-lines (out)
  "Return the lines of the output buffer OUT as a list of strings."
  (with-current-buffer out
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (split-string text "\n"))))

(defun xl-org-babel--tail (out count)
  "Return the last COUNT lines of the output buffer OUT."
  (let ((lines (xl-org-babel--output-lines out)))
    (when (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    (last lines count)))

(defun xl-org-babel--write-results (run lines &optional note)
  "Replace the results of RUN with LINES, then NOTE, a line of text, if any."
  (let* ((start (xl-org-babel-run-start run))
         (end (xl-org-babel-run-end run))
         (buffer (and start (marker-buffer start))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert (xl-org-babel--format-lines lines))
            (when note
              (insert (xl-org-babel--format-lines (list note))))))))))

;; ============================================================
;; Running a block
;; ============================================================

(defun xl-org-babel--run-at-head (head)
  "Return the running block of this buffer whose head is at HEAD, or nil."
  (cl-find-if (lambda (run)
                (let ((h (xl-org-babel-run-head run)))
                  (and h (= (marker-position h) head))))
              xl-org-babel--runs))

(defun xl-org-babel--mode-line-text (frame count)
  "Return the mode-line text for COUNT running blocks, drawn with FRAME."
  (format " %s %s " frame
          (if (= count 1) "1 block running" (format "%d blocks running" count))))

(defun xl-org-babel--spinner-frame (frame)
  "Show FRAME on every running block of this buffer and in its mode line.
Each block's #+RESULTS: line has an overlay for it; the mode line uses
the `xl-spinner' slot at its left edge, so a buffer with blocks running in
the background says so even when the blocks are off screen.  A nil FRAME,
sent when the spinner stops, removes the overlays and empties the slot."
  (dolist (run xl-org-babel--runs)
    (let ((overlay (xl-org-babel-run-overlay run)))
      (when (overlayp overlay)
        (if frame
            (overlay-put overlay 'display
                         (concat frame " running… (C-c C-k cancels)"))
          (delete-overlay overlay)))))
  (setq xl-spinner-mode-line
        (and frame xl-org-babel--runs
             (xl-org-babel--mode-line-text frame (length xl-org-babel--runs))))
  (force-mode-line-update))

(defun xl-org-babel--forget (run)
  "Stop the timer of RUN, remove its overlay, and drop it from the running blocks.
The buffer's spinner stops with its last running block."
  (when (timerp (xl-org-babel-run-timer run))
    (cancel-timer (xl-org-babel-run-timer run)))
  (setf (xl-org-babel-run-timer run) nil)
  (let ((overlay (xl-org-babel-run-overlay run)))
    (when (overlayp overlay) (delete-overlay overlay)))
  (setq xl-org-babel--runs (delq run xl-org-babel--runs))
  (unless xl-org-babel--runs
    (xl-spinner-stop (current-buffer))))

(defun xl-org-babel--finish (run note)
  "End RUN: write its final results, with NOTE if given, and forget it."
  (let* ((out (xl-org-babel-run-out run))
         (lines (and (buffer-live-p out) (xl-org-babel--output-lines out)))
         (lines (and lines (if (string-empty-p (car (last lines)))
                               (butlast lines)
                             lines)))
         (shown (if (> (length lines) xl-org-babel-result-lines)
                    (take xl-org-babel-result-lines lines)
                  lines))
         (more (- (length lines) (length shown)))
         (note-line (concat (and (> more 0)
                                 (format "… %d more lines not written to the file; full output in %s"
                                         more (buffer-name out)))
                            (and note (concat (and (> more 0) " ") note)))))
    (xl-org-babel--write-results
     run (or shown (list "(no output)")) (and (not (string-empty-p (or note-line ""))) note-line))
    (let ((start (xl-org-babel-run-start run))
          (end (xl-org-babel-run-end run)))
      (when (markerp start) (set-marker start nil))
      (when (markerp end) (set-marker end nil)))
    (xl-org-babel--forget run)))

(defun xl-org-babel--render-live (run)
  "Show the last lines of RUN's output under the block, while it runs."
  (setf (xl-org-babel-run-timer run) nil)
  (let ((out (xl-org-babel-run-out run)))
    (when (and (buffer-live-p out) (marker-buffer (xl-org-babel-run-start run)))
      (xl-org-babel--write-results
       run (or (xl-org-babel--tail out xl-org-babel-live-lines) '("…"))))))

(defun xl-org-babel--schedule-render (run)
  "Redraw RUN's live output soon, unless a redraw is already waiting."
  (unless (timerp (xl-org-babel-run-timer run))
    (setf (xl-org-babel-run-timer run)
          (run-with-timer xl-org-babel-render-delay nil
                          #'xl-org-babel--render-live run))))

(defun xl-org-babel--on-output (run chunk)
  "Add CHUNK, output of RUN, to its output buffer and show the new tail."
  (let ((out (xl-org-babel-run-out run)))
    (when (buffer-live-p out)
      (with-current-buffer out
        (save-excursion
          (goto-char (point-max))
          (insert (ansi-color-filter-apply chunk)))))
    (xl-org-babel--schedule-render run)))

(defun xl-org-babel--on-done (run _output code)
  "Finish RUN when its process ends with exit CODE."
  (xl-org-babel--finish
   run
   (cond ((eql code xl-async-timeout-code) "[timed out, stopped]")
         ((and code (not (zerop code))) (format "[exit code %d]" code))
         (t nil))))

(defun xl-org-babel--command (lang body)
  "Return the command line that runs BODY as shell language LANG."
  (list (if (member lang '("shell" "sh")) "sh" lang) "-c" body))

(defun xl-org-babel--start (info params)
  "Run the source block described by INFO with PARAMS in the background."
  (let* ((lang (nth 0 info))
         (body (nth 1 info))
         (head (org-babel-where-is-src-block-head))
         (line (line-number-at-pos head))
         (old (xl-org-babel--run-at-head head)))
    (when old
      (xl-async-cancel (xl-org-babel-run-slot old) (current-buffer))
      (xl-org-babel--finish old "[replaced by a new run]"))
    (let* ((region (xl-org-babel--results-region))
           (out (get-buffer-create (format "*Org Babel Output: %s:%d*"
                                           (buffer-name) line)))
           (dir (let ((d (cdr (assq :dir params))))
                  (if d (expand-file-name d) default-directory)))
           (timeout (let* ((value (cdr (assq :timeout params)))
                           ;; org reads header values as Lisp: 0.3 is a number.
                           (seconds (cond ((numberp value) value)
                                          ((stringp value)
                                           (string-to-number value)))))
                      (and seconds (> seconds 0) seconds)))
           (slot (make-symbol "org-babel-block"))
           run)
      (with-current-buffer out
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (setq run (xl-org-babel-run--make
                 :slot slot
                 :head (copy-marker head)
                 :start (car region)
                 :end (cdr region)
                 :out out))
      (xl-org-babel--write-results run '("running… (C-c C-k cancels)"))
      ;; The spinner sits on the #+RESULTS: line, which the live output
      ;; never rewrites; the file keeps the static placeholder.
      (let* ((header (save-excursion
                       (goto-char (car region))
                       (forward-line -1)
                       (line-beginning-position)))
             (header-end (save-excursion (goto-char header) (line-end-position))))
        (setf (xl-org-babel-run-overlay run) (make-overlay header header-end)))
      (push run xl-org-babel--runs)
      ;; One clock draws both the overlays and the mode line: a second
      ;; `xl-spinner' in this buffer would stop the first.
      (xl-spinner-mode-line-install)
      (if (xl-spinner-running-p)
          (xl-org-babel--spinner-frame (xl-spinner-current-frame))
        (xl-spinner-start #'xl-org-babel--spinner-frame))
      (xl-async-run
       :name "xl-org-babel"
       :owner (current-buffer)
       :slot slot
       :directory dir
       :command (xl-org-babel--command lang body)
       :timeout timeout
       :on-output (lambda (chunk) (xl-org-babel--on-output run chunk))
       :on-done (lambda (output code) (xl-org-babel--on-done run output code))))))

;; ============================================================
;; Entry points
;; ============================================================

(defun xl-org-babel--async-p (lang params)
  "Return non-nil when the block with LANG and PARAMS runs asynchronously.
Only a block with `:async yes' whose results are its output can run in
the background.  Any other block runs as org-babel does, without a word:
a `:session', or `:results' that are not output (value, file, silent,
...), since the background path writes output only.  A block without
`:results' counts as output when `org-babel-shell-results-defaults-to-output'."
  (and (member lang xl-org-babel-async-languages)
       (equal (cdr (assq :async params)) "yes")
       (let* ((session (cdr (assq :session params)))
              ;; org always fills in `:results replace'; placement words say
              ;; where results go, not what they are.
              (kinds (seq-remove (lambda (word)
                                   (member word '("replace" "append" "prepend")))
                                 (split-string (or (cdr (assq :results params)) "")
                                               "[ \t]+" t))))
         (and (or (null session) (equal session "none"))
              (if kinds
                  (and (member "output" kinds)
                       (not (member "file" kinds)))
                (bound-and-true-p org-babel-shell-results-defaults-to-output))))))

(defun xl-org-babel--execute-around (orig &optional arg info params executor-type)
  "Run the block asynchronously when it asks for it; else ORIG.
ARG, INFO, PARAMS and EXECUTOR-TYPE are those of `org-babel-execute-src-block'."
  (let* ((info (or info (org-babel-get-src-block-info)))
         (params (or params (nth 2 info))))
    (if (xl-org-babel--async-p (nth 0 info) params)
        (progn
          (xl-org-babel--start info params)
          nil)
      (funcall orig arg info params executor-type))))

(defun xl-org-babel-cancel-at-point ()
  "Cancel the background block at point, keeping the output so far."
  (interactive)
  (let* ((head (or (ignore-errors (org-babel-where-is-src-block-head))
                   (user-error "No source block at point")))
         (run (or (xl-org-babel--run-at-head head)
                  (user-error "This block is not running"))))
    (xl-async-cancel (xl-org-babel-run-slot run) (current-buffer))
    (xl-org-babel--finish run "[cancelled]")))

(with-eval-after-load 'ob-core
  (advice-add 'org-babel-execute-src-block :around
              #'xl-org-babel--execute-around))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-k") #'xl-org-babel-cancel-at-point))

(provide 'xl-org-babel-async)
;;; xl-org-babel-async.el ends here
