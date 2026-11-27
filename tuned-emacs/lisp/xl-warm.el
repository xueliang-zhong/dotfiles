;;; xl-warm.el --- Native-compile what you used, in a child Emacs -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))
;; Keywords: lisp, compile, performance

;;; Commentary:
;;
;;   M-x xl-warm-start    compile the files of your history, in a child Emacs
;;
;; The child reads savehist and recentf (as data, never evaluated), turns the
;; commands and modes into their source files, and native-compiles each one
;; with `native-compile'.  Files that fail are recorded and skipped until their
;; source changes.  Advice added while the child loads the profile makes Emacs
;; build the trampolines of advised C primitives, so those are ready too.
;; The main Emacs does no compiling; the .eln files are loaded at the next start.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'xl-async)

(defgroup xl-warm nil
  "Native-compile the files of your history in a child Emacs."
  :group 'lisp)

(defvar xl-warm-state-dir (expand-file-name "xl-warm/" user-emacs-directory)
  "Folder holding the failed list and the pid of the running child.")

(defun xl-warm--state (name)
  (expand-file-name name xl-warm-state-dir))

(defun xl-warm--read-setq (file var)
  "The value of VAR in FILE, a file of `setq' forms written by savehist or
recentf.  Read, never evaluated; nil when absent."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((read-eval nil) (found nil))
        (condition-case nil
            (while (not found)
              (let ((form (read (current-buffer))))
                (when (and (consp form) (eq (car form) 'setq))
                  (let ((pairs (cdr form)))
                    (while pairs
                      (when (eq (car pairs) var)
                        (let ((v (cadr pairs)))
                          ;; savehist and recentf write 'VALUE: unquote it.
                          (setq found (list (if (eq (car-safe v) 'quote) (cadr v) v)))))
                      (setq pairs (cddr pairs)))))))
          (end-of-file nil))
        (car found)))))

(defun xl-warm--roots ()
  "Folders whose sources may be compiled: Emacs's library folder, the package
folder, the profile folder, and the whole distro (tuned-emacs/)."
  (delq nil
        (list (and (boundp 'lisp-directory)
                   (file-name-as-directory (expand-file-name lisp-directory)))
              (and (boundp 'package-user-dir)
                   (file-name-as-directory (expand-file-name package-user-dir)))
              (and (boundp 'user-emacs-directory)
                   (file-name-as-directory (expand-file-name user-emacs-directory)))
              (and (bound-and-true-p xl-dotfiles)
                   (file-name-as-directory
                    (expand-file-name "tuned-emacs" xl-dotfiles))))))

(defun xl-warm--trusted-p (file)
  "FILE is a regular, readable .el under one of the roots, not a link."
  ;; Compare real paths: ~/.emacs.d may be a link to the profile folder.
  (let* ((raw (expand-file-name file))
         (f (or (ignore-errors (file-truename raw)) raw)))
    (and (or (string-suffix-p ".el" f) (string-suffix-p ".el.gz" f))
         (file-regular-p f) (not (file-symlink-p raw))
         (seq-some (lambda (r)
                     (string-prefix-p (or (ignore-errors (file-truename r)) r) f))
                   (xl-warm--roots)))))

(defun xl-warm--source-of-symbol (sym)
  "The .el file that defines the Lisp function SYM, or nil."
  ;; A C primitive has no Lisp source; a native Lisp function is a subr too,
  ;; so ask `subr-native-elisp-p' to tell the two apart.
  (when (and (symbolp sym) (fboundp sym)
             (let ((fn (indirect-function sym)))
               (or (not (subrp fn)) (subr-native-elisp-p fn))))
    (let* ((file (symbol-file sym 'defun))
           (el (and file (replace-regexp-in-string "\\.elc\\'" ".el" file)))
           (src (and el (cond ((file-exists-p el) el)
                              ((file-exists-p (concat el ".gz")) (concat el ".gz"))))))
      (and src (xl-warm--trusted-p src) src))))

(defun xl-warm--commands (raw)
  "The command names in RAW, savehist's list of strings, as symbols."
  (mapcar (lambda (c) (if (stringp c) (intern c) c)) (and (listp raw) raw)))

(defvar xl-warm-minor-modes nil
  "Minor modes that were enabled when savehist last saved, as strings.
Saved by savehist (see `xl-warm--remember-minor-modes'), so that the
child can find their source files the same way it finds commands.")

(defun xl-warm--remember-minor-modes ()
  "Record the minor modes enabled now, globally or in some buffer.
Runs in `savehist-save-hook', so the list is written with the history."
  (let ((modes (append global-minor-modes
                       (cl-loop for buf in (buffer-list)
                                append (buffer-local-value 'local-minor-modes buf)))))
    (dolist (mode (delete-dups (delq nil modes)))
      (when (and (symbolp mode) (fboundp mode))
        (add-to-history 'xl-warm-minor-modes (symbol-name mode) 200 t)))))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'xl-warm-minor-modes)
  (add-hook 'savehist-save-hook #'xl-warm--remember-minor-modes))

(defun xl-warm--minor-modes (raw)
  "The minor mode names in RAW, savehist's list of strings, as symbols."
  (mapcar (lambda (m) (if (stringp m) (intern m) m)) (and (listp raw) raw)))

(defun xl-warm--recent-rows (recent)
  "One row per recentf file: (LABEL SOURCE WHY).  A file is a source when it is
Emacs Lisp in a trusted root; its package, if any, is in the label."
  (mapcar (lambda (file)
            (let* ((f (expand-file-name file))
                   (label "recentf")
                   (src (and (or (string-suffix-p ".el" f) (string-suffix-p ".el.gz" f))
                             (xl-warm--trusted-p f) f)))
              (list label src
                    (cond (src nil)
                          ((not (or (string-suffix-p ".el" f) (string-suffix-p ".el.gz" f)))
                           "not Emacs Lisp")
                          (t "outside the trusted roots")))))
          (seq-filter (lambda (f)
                        (and (stringp f)
                             (or (string-suffix-p ".el" f) (string-suffix-p ".el.gz" f))))
                      (and (listp recent) recent))))

(defun xl-warm--history-sources ()
  "Source files of the commands and minor modes in savehist, and of the modes
of recentf files."
  (let* ((commands (xl-warm--read-setq
                    (or (and (boundp 'savehist-file) savehist-file)
                        (expand-file-name "history" user-emacs-directory))
                    'extended-command-history))
         (minors (xl-warm--read-setq
                  (or (and (boundp 'savehist-file) savehist-file)
                      (expand-file-name "history" user-emacs-directory))
                  'xl-warm-minor-modes))
         (recent (xl-warm--read-setq
                  (or (and (boundp 'recentf-save-file) recentf-save-file)
                      (expand-file-name "recentf.eld" user-emacs-directory))
                  'recentf-list))
         (modes (delete-dups
                 (delq nil
                       (mapcar (lambda (file)
                                 (cl-loop for (re . mode) in auto-mode-alist
                                          when (and (stringp file) (string-match-p re file))
                                          return mode))
                               recent))))
         (sources (append (mapcar #'xl-warm--source-of-symbol (xl-warm--commands commands))
                          (mapcar #'xl-warm--source-of-symbol (xl-warm--minor-modes minors))
                          (mapcar #'xl-warm--source-of-symbol modes)
                          (mapcar #'cadr (xl-warm--recent-rows recent)))))
    (delete-dups (delq nil sources))))

(defun xl-warm--failed ()
  "The failed list: an alist (SOURCE . MTIME-IT-FAILED-AT)."
  (xl-warm--read-setq (xl-warm--state "failed.eld") 'xl-warm-failed-list))

(defun xl-warm--save-failed (list)
  (make-directory xl-warm-state-dir t)
  (with-temp-file (xl-warm--state "failed.eld")
    (prin1 `(setq xl-warm-failed-list ',list) (current-buffer))))

(defun xl-warm--mtime (file)
  (float-time (file-attribute-modification-time (file-attributes file))))

(defcustom xl-warm-max-source-bytes (* 1024 1024)
  "Largest source, uncompressed, that xl-warm compiles.  Bigger ones are skipped
and the report says so; a very large file takes long and may not pay off."
  :type 'natnum
  :group 'xl-warm)

(defun xl-warm--source-bytes (src)
  "SRC's size in bytes, uncompressed when SRC is an .el.gz."
  (if (string-suffix-p ".gz" src)
      (or (ignore-errors
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (insert-file-contents src))
              (buffer-size)))
          0)
    (or (ignore-errors (file-attribute-size (file-attributes src))) 0)))

(defun xl-warm--opted-out-p (src)
  "The cookie that opts SRC out, as a string (`no-native-compile' or
`no-byte-compile'), or nil.  The cookie is either on the -*- first line or a
line of the local variables.  A file that says so is never compiled."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents src)
      (goto-char (point-min))
      (let ((first (buffer-substring (line-beginning-position) (line-end-position))))
        (or (and (string-match "-\\*-.*\\(no-native-compile\\|no-byte-compile\\):[ \t]*t" first)
                 (match-string 1 first))
            (progn
              (goto-char (point-min))
              (and (re-search-forward
                    "^[ \t;]*\\(no-native-compile\\|no-byte-compile\\):[ \t]*t" nil t)
                   (match-string 1))))))))

(defun xl-warm--failed-entry (src)
  "The failed record of SRC as (MTIME REASON TIME), or nil."
  (let ((e (cdr (assoc src (xl-warm--failed)))))
    (cond ((numberp e) (list e "earlier run" "?"))
          (e e))))

(defun xl-warm--verdict (src)
  "What happens to SRC, as (STATUS . WHY).  STATUS is `todo', `native',
`failed' or `skip'; WHY says why, with the numbers that decided it."
  (require 'comp nil t)
  (let ((eln (comp-el-to-eln-filename src)))
    (cond
     ((not (xl-warm--trusted-p src)) (cons 'skip "outside the trusted roots"))
     ((xl-warm--opted-out-p src)
      (cons 'skip (format "opts out with %s" (xl-warm--opted-out-p src))))
     ((> (xl-warm--source-bytes src) xl-warm-max-source-bytes)
      (cons 'skip (format "source is %d KB, over the %d KB limit"
                          (/ (xl-warm--source-bytes src) 1024)
                          (/ xl-warm-max-source-bytes 1024))))
     ((and (file-exists-p eln) (not (file-newer-than-file-p src eln)))
      (cons 'native "native already"))
     ((let ((e (xl-warm--failed-entry src)))
        (and e (= (car e) (xl-warm--mtime src))))
      (let ((e (xl-warm--failed-entry src)))
        (cons 'failed (format "failed at %s: %s" (nth 2 e) (nth 1 e)))))
     (t (cons 'todo "to compile")))))

(defun xl-warm--hard-skip-p (src)
  "Non-nil when SRC may never be compiled: outside the trusted roots, or opted
out.  The size limit is not hard: an explicit force request overrides it."
  (or (not (xl-warm--trusted-p src)) (xl-warm--opted-out-p src)))

(defun xl-warm--todo (sources)
  "SOURCES whose verdict is to compile."
  (seq-filter (lambda (src) (eq (car (xl-warm--verdict src)) 'todo)) sources))

(defun xl-warm--lock ()
  "Take the pid file.  Nil when another run is alive."
  (make-directory xl-warm-state-dir t)
  (let* ((pidfile (xl-warm--state "pid"))
         (other (and (file-exists-p pidfile)
                     (ignore-errors (string-to-number
                                     (with-temp-buffer (insert-file-contents pidfile)
                                                       (buffer-string)))))))
    (unless (xl-warm--alive-p other)
      (with-temp-file pidfile (insert (number-to-string (emacs-pid))))
      t)))

(defun xl-warm--unlock ()
  (ignore-errors (delete-file (xl-warm--state "pid"))))

(defun xl-warm-run (&optional files force)
  "In the child: compile the todo list one file at a time; record failures.
With FILES, compile those instead of the history; with FORCE, do not skip
the files on the failed list.  Returns the number of files compiled."
  (require 'comp)
  (if (not (xl-warm--lock))
      (progn (message "xl-warm: another run is alive") 0)
    (unwind-protect
        (let* ((todo (cond ((and files force)
                            (seq-remove #'xl-warm--hard-skip-p files))
                           (files (xl-warm--todo files))
                           (t (xl-warm--todo (xl-warm--history-sources)))))
               (failed (xl-warm--failed))
               (done 0))
          (dolist (src todo)
            (condition-case err
                (progn
                  (native-compile src (comp-el-to-eln-filename src))
                  (if (file-exists-p (comp-el-to-eln-filename src))
                      (cl-incf done)
                    (push (list src (xl-warm--mtime src) "no .eln was written"
                                (format-time-string "%Y-%m-%d %H:%M"))
                          failed)))
              (error
               (let ((reason (error-message-string err)))
                 (message "xl-warm: %s failed: %s" src reason)
                 (push (list src (xl-warm--mtime src) reason
                             (format-time-string "%Y-%m-%d %H:%M"))
                       failed)))))
          (xl-warm--save-failed failed)
          (message "xl-warm: %d compiled, %d todo, %d failed" done (length todo) (length failed))
          done)
      (xl-warm--unlock))))

;;; ---- Automatic: once per session, after Emacs has been idle ----

(defcustom xl-warm-mode 1
  "How xl-warm runs by itself.
0: only when you call `xl-warm-start' (M-x).
1: also once per session, after `xl-warm-idle' seconds of idle time, if the
history has files not yet native."
  :type 'natnum
  :group 'xl-warm)

(defcustom xl-warm-idle 5.0
  "Seconds of idle time before the once-per-session check starts a child."
  :type 'number
  :group 'xl-warm)

(defvar xl-warm--timer nil "The idle timer, or nil once it has run or been disarmed.")
(defvar xl-warm--checked nil "Non-nil once this session has done its check.")

(defun xl-warm--alive-p (pid)
  "Non-nil if PID is a running process.  A zombie (exited, not yet reaped by
its parent) is not running; `signal-process' alone cannot tell the two apart."
  (and (integerp pid) (> pid 0)
       (zerop (or (ignore-errors (signal-process pid 0)) -1))
       (not (equal (cdr (assq 'state (ignore-errors (process-attributes pid)))) "Z"))))

(defun xl-warm--running-p ()
  "Non-nil while a child of an earlier run is alive (its pid file names it)."
  (let* ((pidfile (xl-warm--state "pid"))
         (pid (and (file-exists-p pidfile)
                   (ignore-errors (string-to-number
                                   (with-temp-buffer (insert-file-contents pidfile)
                                                     (buffer-string)))))))
    (xl-warm--alive-p pid)))

(defun xl-warm--idle ()
  "The idle timer's function: start a child only if there is something to do."
  (setq xl-warm--timer nil)
  (unless (or xl-warm--checked (xl-warm--running-p))
    (setq xl-warm--checked t)
    (when (xl-warm--todo (xl-warm--history-sources))
      (xl-warm-start))))

;;;###autoload
(defun xl-warm-arm ()
  "When `xl-warm-mode' is 1: once per session, after `xl-warm-idle' seconds of
idle time, start a child if the history has files not yet native.  Does
nothing in batch, or when the mode is 0."
  (when (and (> xl-warm-mode 0) (not noninteractive)
             (not xl-warm--checked) (not (timerp xl-warm--timer)))
    (setq xl-warm--timer (run-with-idle-timer xl-warm-idle nil #'xl-warm--idle))))

;;; ---- The report: one line per history entry, what happens to it ----

(defun xl-warm--why-no-source (sym)
  "Why SYM has no source xl-warm can compile, as a string."
  (let ((fn (and (symbolp sym) (fboundp sym) (indirect-function sym))))
    (cond ((null fn) "not loaded in this Emacs (lazy, or not used yet)")
          ((and (subrp fn) (not (subr-native-elisp-p fn))) "C primitive: no Lisp source")
          (t (let* ((file (symbol-file sym 'defun))
                    (el (and file (replace-regexp-in-string "\\.elc\\'" ".el" file))))
               (cond ((null el) "no file")
                     ((not (or (file-exists-p el) (file-exists-p (concat el ".gz"))))
                      "no .el or .el.gz next to it")
                     (t "outside the trusted roots")))))))

(defun xl-warm--items ()
  "The history entries as rows: (LABEL SOURCE WHY) with SOURCE nil when none."
  (let* ((commands (xl-warm--read-setq
                    (or (and (boundp 'savehist-file) savehist-file)
                        (expand-file-name "history" user-emacs-directory))
                    'extended-command-history))
         (recent (xl-warm--read-setq
                  (or (and (boundp 'recentf-save-file) recentf-save-file)
                      (expand-file-name "recentf.eld" user-emacs-directory))
                  'recentf-list))
         (minors (xl-warm--read-setq
                  (or (and (boundp 'savehist-file) savehist-file)
                      (expand-file-name "history" user-emacs-directory))
                  'xl-warm-minor-modes))
         (modes (delete-dups
                 (delq nil
                       (mapcar (lambda (file)
                                 (cl-loop for (re . mode) in auto-mode-alist
                                          when (and (stringp file) (string-match-p re file))
                                          return mode))
                               recent)))))
    (append
     (mapcar (lambda (sym)
               (let ((src (xl-warm--source-of-symbol sym)))
                 (list (format "command %s" sym) src
                       (if src nil (xl-warm--why-no-source sym)))))
             (xl-warm--commands commands))
     (mapcar (lambda (sym)
               (let ((src (xl-warm--source-of-symbol sym)))
                 (list (format "mode %s" sym) src
                       (if src nil (xl-warm--why-no-source sym)))))
             (xl-warm--minor-modes minors))
     (mapcar (lambda (mode)
               (let ((src (xl-warm--source-of-symbol mode)))
                 (list (format "mode %s" mode) src
                       (if src nil (xl-warm--why-no-source mode)))))
             modes)
     (xl-warm--recent-rows recent))))

(defun xl-warm--status-word (verdict)
  (pcase (car verdict)
    ('native "native")
    ('todo "to compile")
    ('failed "failed")
    (_ "skipped")))

(defun xl-warm--paint ()
  "Give the report its faces.  Only faces are added; the text stays as it is."
  (save-excursion
    (goto-char (point-min))
    (let ((section nil))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring bol eol)))
          (cond
           ((string-match-p "\\`xl-warm report" line)
            (add-face-text-property bol eol 'bold))
           ((string-match-p "\\`Native compilation is not" line)
            (add-face-text-property bol eol 'warning))
           ((string-match-p "\\`\\(Status:\\|NOTE:\\|Not yet compiled:\\|Not compiled, and why:\\)" line)
            (add-face-text-property bol eol 'bold)
            (setq section (cond ((string-prefix-p "Not yet" line) 'missing)
                                ((string-prefix-p "Not compiled" line) 'why)
                                (t nil))))
           ((string-match-p "\\`source +KB" line)
            (add-face-text-property bol eol 'bold))
           ((string-match "\\`  \\([0-9]+\\) " line)
            (add-face-text-property (+ bol (match-beginning 1)) (+ bol (match-end 1))
                                    'font-lock-constant-face))
           ((and (eq section 'missing) (string-match "\\`  \\(command\\|mode\\|recent\\) \\(\\S-+\\)" line))
            (add-face-text-property (+ bol (match-beginning 2)) (+ bol (match-end 2))
                                    'font-lock-function-name-face)
            (when (> (length line) 41)
              (add-face-text-property (+ bol 41) eol 'shadow)))
           ((and (eq section 'why) (string-match "\\`  \\(\\S-+\\)" line))
            (add-face-text-property (+ bol (match-beginning 1)) (+ bol (match-end 1))
                                    'font-lock-type-face)
            (when (> (length line) 27)
              (add-face-text-property (+ bol 27) eol 'shadow)))
           ((string-match-p "\\`\\S-" line)
            (let ((name-end (or (string-match " " line) (length line))))
              (add-face-text-property bol (+ bol name-end) 'font-lock-type-face))
            (when (string-match "\\bnative\\b\\|to compile\\|\\bfailed\\b\\|\\bskipped\\b" line)
              (add-face-text-property (+ bol (match-beginning 0)) (+ bol (match-end 0))
                                      (pcase (match-string 0 line)
                                        ("native" 'success)
                                        ("to compile" 'warning)
                                        ("skipped" 'shadow)
                                        (_ 'error))))
            (when (string-match "(xl-warm-compile-file" line)
              (add-face-text-property (+ bol (match-beginning 0)) eol 'shadow))))
          (forward-line 1))))))

(defun xl-warm--short-name (file)
  "FILE's base name without its .el or .el.gz suffix.  The full path is
printed in the last column of the report, so the suffix is not needed."
  (replace-regexp-in-string "\\.el\\(\\.gz\\)?\\'" "" (file-name-nondirectory file)))

(defun xl-warm--report-text ()
  "The report as a string.  Runs in the child; nothing is compiled."
  (let* ((items (xl-warm--items))
         (rows (seq-filter #'cadr items))
         (none (seq-filter (lambda (i) (null (cadr i))) items))
         (srcs (delete-dups (mapcar #'cadr rows)))
         (verdicts (mapcar (lambda (src) (cons src (xl-warm--verdict src))) srcs))
         (todo (seq-filter (lambda (v) (eq (cadr v) 'todo)) verdicts)))
    (with-output-to-string
      (princ "xl-warm report (report only; nothing is compiled here)\n")
      (unless (native-comp-available-p)
        (princ "\nNative compilation is not available in this Emacs: nothing here can be compiled.\n"))
      (princ (format "\nStatus:\n  %d source(s) from your history; %d entr(y/ies) without one; next run compiles %d file(s)\n\n"
                     (length srcs) (length none) (length todo)))
      (princ (format "%-28s %7s  %-10s  %-34s  %s\n" "source" "KB" "status" "from"
                     "force xl-warm-compile-file"))
      (dolist (src srcs)
        (let* ((v (cdr (assoc src verdicts)))
               (from (delete-dups
                      (mapcar (lambda (r) (replace-regexp-in-string "\\`\\(command\\|mode\\|recent\\) " "" (car r)))
                              (seq-filter (lambda (r) (equal (cadr r) src)) rows)))))
          (princ (format "%-28s %7.1f  %-10s  %-34s  (xl-warm-compile-file \"%s\")\n"
                         (xl-warm--short-name src)
                         (/ (xl-warm--source-bytes src) 1024.0)
                         (xl-warm--status-word v)
                         (truncate-string-to-width (mapconcat #'identity from ", ") 34 nil nil "…")
                         (abbreviate-file-name src)))))
      (let ((skipped (seq-remove (lambda (v) (memq (cadr v) '(todo native))) verdicts)))
        (when skipped
          (princ "\nNot compiled, and why:\n")
          (dolist (v skipped)
            (princ (format "  %-28s %s\n" (xl-warm--short-name (car v)) (cddr v))))))
      (when none
        (princ "\nNot yet compiled:\n")
        (princ (format "  %-40s %-52s%s\n" "" "reason" "force xl-warm-compile"))
        (dolist (r none)
          (let* ((why (nth 2 r))
                 (name (and (string-match "\\`\\(?:command\\|mode\\) \\(.+\\)" (car r))
                            (match-string 1 (car r))))
                 (sym (and name (intern name)))
                 ;; Only an autoload can be loaded by name, so only it gets a hint.
                 (hint (if (and sym (fboundp sym) (autoloadp (symbol-function sym)))
                           (format "(xl-warm-compile '%s)" name)
                         "")))
            (princ (format "  %-40s %-52s%s\n" (car r) why hint)))))
      (princ "\nNOTE:\n  The sources come from savehist (commands) and recentf (modes).\n")
      (princ "  Built-in libraries count too: their .el.gz is compiled.  A file is skipped\n")
      (princ "  when it is outside the trusted roots, opts out with no-native-compile, or is\n")
      (princ "  over the size limit.  A failure is kept until the file changes.\n")
      (princ "  To compile something now, use the force hints above or M-x xl-warm-start.\n"))))

(defun xl-warm--show-report (buf output)
  "Fill BUF with the report that a child printed into OUTPUT, after its marker."
  (xl-spinner-mode-line-stop)
  (when (buffer-live-p buf)
    (with-current-buffer buf (xl-warm--draw-center nil))
    (let* ((at (string-match "<<XL-WARM-REPORT>>\n" output))
           (text (if at (substring output (match-end 0))
                   (concat "The report failed:\n" output))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (replace-regexp-in-string "\r" "" text))
          (xl-warm--paint))
        (goto-char (point-min))))))

;; The loader: an overlay with a before-string on an empty placeholder line in
;; the middle of the window.  No text is added, so the window does not move.
(defvar-local xl-warm--center nil)
(defvar-local xl-warm--busy-text "computing the report")

(defun xl-warm--draw-center (frame)
  "Draw FRAME (the spinner's frame text) in the middle of this buffer's window.
nil removes it."
  (if (null frame)
      (when (overlayp xl-warm--center)
        (delete-overlay xl-warm--center)
        (setq xl-warm--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  " xl-warm--busy-text))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (pos (save-excursion
                    (goto-char (window-start window))
                    (forward-line (/ (window-body-height window) 2))
                    (line-beginning-position))))
        (unless (overlayp xl-warm--center)
          (setq xl-warm--center (make-overlay pos pos)))
        (move-overlay xl-warm--center pos pos)
        (overlay-put xl-warm--center 'before-string
                     (concat (make-string pad ?\s) text))))))

;;;###autoload
(defun xl-warm-report ()
  "Show what xl-warm does with each source, in a table like `eln-opt-report'.
The report is computed in a child Emacs; the buffer shows a loader until it is
ready.  Nothing is compiled."
  (interactive)
  (let ((buf (get-buffer-create "*xl-warm report*")))
    (pop-to-buffer buf)
    (unless (xl-async-busy-p 'xl-warm-report buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Empty placeholder lines: the loader sits on one of them.
          (insert (make-string 400 ?\n)))
        (setq-local xl-warm--busy-text "computing the report")
        (special-mode))
      (xl-spinner-mode-line-start "xl-warm report" :also #'xl-warm--draw-center)
      (let* ((exe (expand-file-name invocation-name invocation-directory))
             (profile (file-name-as-directory user-emacs-directory))
             (proc (xl-async-start
                    (list exe "--batch" "--init-directory" profile
                          "-l" (expand-file-name "early-init.el" profile)
                          "--eval" "(run-hooks (quote before-init-hook))"
                          "-l" (expand-file-name "init.el" profile)
                          "--eval" "(progn (require 'xl-warm) (princ \"\\n<<XL-WARM-REPORT>>\\n\") (princ (xl-warm--report-text)))")
                    :name "xl-warm-report")))
        (xl-async-own proc buf)
        (xl-async-slot proc 'xl-warm-report)
        (xl-async-on-exit proc (lambda (p) (xl-warm--show-report buf (xl-async-output p))))))
    buf))

;;; ---- Starting a child: the history, or an explicit list of files ----

(defun xl-warm--spawn (files force)
  "Start a child that compiles FILES (or the history when nil)."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory))
         (proc (xl-async-start
                (list exe "--batch" "--init-directory" profile
                      "-l" (expand-file-name "early-init.el" profile)
                      "--eval" "(run-hooks (quote before-init-hook))"
                      "-l" (expand-file-name "init.el" profile)
                      "--eval" (format "(progn (require 'xl-warm) (xl-warm-run '%S %s))"
                                       files (if force "t" "nil")))
                :name "xl-warm")))
    (xl-async-on-exit proc (lambda (p) (message "xl-warm: finished (%s)" (xl-async-exit-code p))))
    (message "xl-warm: started in the background")
    proc))

;;;###autoload
(defun xl-warm-compile-file (file)
  "Native-compile FILE now, in the background, even if it is native already or
failed before."
  (interactive (list (read-file-name "Compile file: " nil nil t)))
  (if (xl-warm--trusted-p file)
      (xl-warm--spawn (list (expand-file-name file)) t)
    (user-error "xl-warm: %s is not in a trusted folder" file)))

;;;###autoload
(defun xl-warm-compile (function)
  "Native-compile now, in the background, the source of FUNCTION, even if it is
not loaded yet (an autoload is loaded first) and even if it failed before."
  (interactive (list (intern (completing-read "Compile the source of: " obarray
                                              #'fboundp t nil nil (thing-at-point 'symbol)))))
  (when (and (fboundp function) (autoloadp (symbol-function function)))
    (autoload-do-load (symbol-function function) function))
  (let ((src (xl-warm--source-of-symbol function)))
    (if src
        (xl-warm--spawn (list src) t)
      (user-error "xl-warm: no source for %s" function))))

;;; ---- Clean: remove what this module compiled, keep the trampolines ----

(defun xl-warm--cache-dir ()
  "The user's eln folder: the first entry of `native-comp-eln-load-path'."
  (and (boundp 'native-comp-eln-load-path) (car native-comp-eln-load-path)))

(defun xl-warm--clean-list ()
  "Every .eln in the user's eln folder except the subr trampolines, which are
costly to rebuild and are not compiled Lisp."
  (let ((dir (xl-warm--cache-dir)))
    (and dir (file-directory-p dir)
         (seq-remove (lambda (f) (string-prefix-p "subr--trampoline-"
                                                  (file-name-nondirectory f)))
                     (directory-files-recursively dir "\\.eln\\'")))))

;;;###autoload
(defun xl-warm-clean ()
  "Delete the .eln files xl-warm built (every eln in the user's eln folder
except the subr trampolines), and the failed list.  Refuses while a child is
running.  Interactive: asks first; the answer must be yes."
  (interactive)
  (let ((files (xl-warm--clean-list)))
    (cond
     ((xl-warm--running-p) (user-error "xl-warm: a child is running; try again later"))
     ((and (not noninteractive)
           (not (yes-or-no-p (format "Delete %d eln file(s) (trampolines are kept)? "
                                     (length files)))))
      (message "xl-warm: clean cancelled") nil)
     (t
      (dolist (f files) (ignore-errors (delete-file f)))
      (ignore-errors (delete-file (xl-warm--state "failed.eld")))
      (unless noninteractive
        (message "xl-warm: deleted %d eln file(s); trampolines kept" (length files)))
      (length files)))))

;;;###autoload
(defun xl-warm-start ()
  "Compile the files of your history in a background child Emacs.
The child loads this profile, so the trampolines of advice are built too."
  (interactive)
  (xl-warm--spawn nil nil))

(provide 'xl-warm)
;;; xl-warm.el ends here
