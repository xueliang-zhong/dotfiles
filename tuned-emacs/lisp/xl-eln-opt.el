;;; xl-eln-opt.el --- Native-compile what you used, in a child Emacs -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))
;; Keywords: lisp, compile, performance

;;; Commentary:
;;
;;   M-x xl-eln-opt-start    compile the files of your history, in a child Emacs
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
(require 'xl-spinner)
(require 'xl-async-evaluate)

;; The trace macro and the follower are used in the child run; the child loads
;; the profile, which autoloads them too.  Byte-compiling needs them defined.
(eval-when-compile (require 'xl-perfetto-trace))
(declare-function xl-perfetto-follow-marker "xl-perfetto-trace" (prefix))
(declare-function xl-perfetto-follow-finish "xl-perfetto-trace" ())

(defgroup xl-eln-opt nil
  "Native-compile the files of your history in a child Emacs."
  :group 'lisp)

(defvar xl-eln-opt-state-dir (expand-file-name "xl-eln-opt/" user-emacs-directory)
  "Folder holding the failed list and the pid of the running child.")

(defun xl-eln-opt--state (name)
  (expand-file-name name xl-eln-opt-state-dir))

(defun xl-eln-opt--read-setq (file var)
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
          (error nil))
        (car found)))))

(defun xl-eln-opt--roots ()
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

(defun xl-eln-opt--trusted-p (file)
  "FILE is a regular, readable .el under one of the roots, not a link."
  ;; Compare real paths: ~/.emacs.d may be a link to the profile folder.
  (let* ((raw (expand-file-name file))
         (f (or (ignore-errors (file-truename raw)) raw)))
    (and (or (string-suffix-p ".el" f) (string-suffix-p ".el.gz" f))
         (file-regular-p f) (not (file-symlink-p raw))
         (seq-some (lambda (r)
                     (string-prefix-p (or (ignore-errors (file-truename r)) r) f))
                   (xl-eln-opt--roots)))))

(defun xl-eln-opt--source-of-symbol (sym)
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
      (and src (xl-eln-opt--trusted-p src) src))))

(defun xl-eln-opt--commands (raw)
  "The command names in RAW, savehist's list of strings, as symbols."
  (mapcar (lambda (c) (if (stringp c) (intern c) c)) (and (listp raw) raw)))

(defvar xl-eln-opt-minor-modes nil
  "Minor modes that were enabled when savehist last saved, as strings.
Saved by savehist (see `xl-eln-opt--remember-minor-modes'), so that the
child can find their source files the same way it finds commands.")

(defun xl-eln-opt--remember-minor-modes ()
  "Record the minor modes enabled now, globally or in some buffer.
Runs in `savehist-save-hook', so the list is written with the history."
  (let ((modes (append global-minor-modes
                       (cl-loop for buf in (buffer-list)
                                append (buffer-local-value 'local-minor-modes buf)))))
    (dolist (mode (delete-dups (delq nil modes)))
      (when (and (symbolp mode) (fboundp mode))
        (add-to-history 'xl-eln-opt-minor-modes (symbol-name mode) 200)))))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'xl-eln-opt-minor-modes)
  (add-hook 'savehist-save-hook #'xl-eln-opt--remember-minor-modes))

(defun xl-eln-opt--minor-modes (raw)
  "The minor mode names in RAW, savehist's list of strings, as symbols."
  (mapcar (lambda (m) (if (stringp m) (intern m) m)) (and (listp raw) raw)))

(defcustom xl-eln-opt-scope '(savehist minor-modes recentf packages distro)
  "Which sources xl-eln-opt compiles by itself.
Where a source comes from: `savehist' (the commands you ran), `minor-modes' (the
minor modes that were enabled), `recentf' (the major modes of your recent files
and the recent Emacs Lisp files).  Where a file lies: `packages' (Emacs's own
libraries, installed packages and the profile folder) or `distro' (tuned-emacs/).
The explicit commands `xl-eln-opt-compile' and `xl-eln-opt-compile-file' ignore
this option."
  :type '(set (const :tag "Commands in savehist" savehist)
              (const :tag "Minor modes that were enabled" minor-modes)
              (const :tag "Major modes and Lisp files in recentf" recentf)
              (const :tag "Emacs's libraries, installed packages, profile folder" packages)
              (const :tag "tuned-emacs/ distro" distro))
  :group 'xl-eln-opt)

(defun xl-eln-opt--distro-file-p (src)
  "Non-nil when SRC lies under the distro folder tuned-emacs/."
  (let ((root (and (bound-and-true-p xl-dotfiles)
                   (file-name-as-directory (expand-file-name "tuned-emacs" xl-dotfiles)))))
    (and root
         (ignore-errors
           (string-prefix-p (file-truename root) (file-truename src))))))

(defun xl-eln-opt--scoped (label src why)
  "The row (LABEL SOURCE WHY).  A source outside `xl-eln-opt-scope' is dropped
from the row, and WHY says which scope it is outside."
  (cond ((null src) (list label nil why))
        ((memq (if (xl-eln-opt--distro-file-p src) 'distro 'packages) xl-eln-opt-scope)
         (list label src nil))
        (t (list label nil (format "outside xl-eln-opt-scope (%s)"
                                   (if (xl-eln-opt--distro-file-p src) "distro" "packages"))))))

(defun xl-eln-opt--recent-rows (recent)
  "Rows for the recentf files: (LABEL SOURCE WHY).  A file is a source when it is
Emacs Lisp in a trusted root."
  (mapcar (lambda (file)
            (let* ((f (expand-file-name file))
                   (src (and (xl-eln-opt--trusted-p f) f)))
              (xl-eln-opt--scoped "recentf" src
                                  (cond (src nil)
                                        (t "outside the trusted roots")))))
          (seq-filter (lambda (f)
                        (and (stringp f)
                             (or (string-suffix-p ".el" f) (string-suffix-p ".el.gz" f))))
                      (and (listp recent) recent))))

(defun xl-eln-opt--symbol-row (label sym)
  "The row for the function or mode SYM, labelled LABEL."
  (let ((src (xl-eln-opt--source-of-symbol sym)))
    (xl-eln-opt--scoped label src (and (null src) (xl-eln-opt--why-no-source sym)))))

(defun xl-eln-opt--distro-rows ()
  "A row for every Emacs Lisp file of the distro (tuned-emacs/*.el and
lisp/*.el), whether or not the history mentions it."
  (let* ((dir (and (bound-and-true-p xl-dotfiles)
                   (file-name-as-directory (expand-file-name "tuned-emacs" xl-dotfiles))))
         (files (and dir (file-directory-p dir)
                     (append (directory-files dir t "\\.el\\'")
                             (let ((lisp (expand-file-name "lisp" dir)))
                               (and (file-directory-p lisp)
                                    (directory-files lisp t "\\.el\\'")))))))
    (mapcar (lambda (f)
              (let ((src (and (xl-eln-opt--trusted-p f) f)))
                (xl-eln-opt--scoped "distro" src
                                    (if src nil "outside the trusted roots"))))
            files)))

(defun xl-eln-opt--rows ()
  "Every history entry in `xl-eln-opt-scope' as a row (LABEL SOURCE WHY).
SOURCE is nil when the entry has no source; WHY then says why."
  (let* ((savehist-path (or (and (boundp 'savehist-file) savehist-file)
                            (expand-file-name "history" user-emacs-directory)))
         (recentf-path (or (and (boundp 'recentf-save-file) recentf-save-file)
                           (expand-file-name "recentf.eld" user-emacs-directory)))
         (commands (and (memq 'savehist xl-eln-opt-scope)
                        (xl-eln-opt--read-setq savehist-path 'extended-command-history)))
         (minors (and (memq 'minor-modes xl-eln-opt-scope)
                      (xl-eln-opt--read-setq savehist-path 'xl-eln-opt-minor-modes)))
         (recent (and (memq 'recentf xl-eln-opt-scope)
                      (xl-eln-opt--read-setq recentf-path 'recentf-list)))
         (modes (delete-dups
                 (delq nil
                       (mapcar (lambda (file)
                                 (cl-loop for (re . mode) in auto-mode-alist
                                          when (and (stringp file) (string-match-p re file))
                                          return mode))
                               recent)))))
    (append
     (when (memq 'savehist xl-eln-opt-scope)
       (mapcar (lambda (sym) (xl-eln-opt--symbol-row (format "command %s" sym) sym))
               (xl-eln-opt--commands commands)))
     (when (memq 'minor-modes xl-eln-opt-scope)
       (mapcar (lambda (sym) (xl-eln-opt--symbol-row (format "mode %s" sym) sym))
               (xl-eln-opt--minor-modes minors)))
     (when (memq 'recentf xl-eln-opt-scope)
       (append (mapcar (lambda (sym) (xl-eln-opt--symbol-row (format "mode %s" sym) sym))
                       modes)
               (xl-eln-opt--recent-rows recent)))
     (when (memq 'distro xl-eln-opt-scope)
       (xl-eln-opt--distro-rows)))))

(defun xl-eln-opt--history-sources ()
  "The source files of the history entries in scope, each once."
  (delete-dups (delq nil (mapcar #'cadr (xl-eln-opt--rows)))))

(defun xl-eln-opt--failed ()
  "The failed list: an alist (SOURCE . MTIME-IT-FAILED-AT)."
  (xl-eln-opt--read-setq (xl-eln-opt--state "failed.eld") 'xl-eln-opt-failed-list))

(defun xl-eln-opt--save-failed (list)
  (make-directory xl-eln-opt-state-dir t)
  (with-temp-file (xl-eln-opt--state "failed.eld")
    (prin1 `(setq xl-eln-opt-failed-list ',list) (current-buffer))))

(defun xl-eln-opt--mtime (file)
  (float-time (file-attribute-modification-time (file-attributes file))))

(defcustom xl-eln-opt-max-source-bytes (* 1024 1024)
  "Largest source, uncompressed, that xl-eln-opt compiles.  Bigger ones are skipped
and the report says so; a very large file takes long and may not pay off."
  :type 'natnum
  :group 'xl-eln-opt)

(defun xl-eln-opt--source-bytes-compute (src)
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

(defun xl-eln-opt--opted-out-compute (src)
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

(defvar xl-eln-opt--facts-table nil
  "Per-source facts read this session: SRC -> (MTIME SIZE OPTOUT BYTES).")
(defvar xl-eln-opt--facts-used nil
  "The sources whose facts were asked for in this process; only these are saved.")

(defun xl-eln-opt--facts-file ()
  (xl-eln-opt--state "facts.eld"))

(defun xl-eln-opt--facts-load ()
  "The table of saved facts, read once per process."
  (unless xl-eln-opt--facts-table
    (setq xl-eln-opt--facts-table (make-hash-table :test 'equal))
    (dolist (row (xl-eln-opt--read-setq (xl-eln-opt--facts-file) 'xl-eln-opt-facts))
      (when (and (consp row) (stringp (car row)))
        (puthash (car row) (cdr row) xl-eln-opt--facts-table))))
  (unless xl-eln-opt--facts-used
    (setq xl-eln-opt--facts-used (make-hash-table :test 'equal)))
  xl-eln-opt--facts-table)

(defun xl-eln-opt--facts (src)
  "The opt-out cookie and the uncompressed size of SRC, as (OPTOUT . BYTES).
Read from the file only when its modification time or size has changed, so a
source that has not changed is not opened again."
  (let* ((attrs (file-attributes src))
         (table (xl-eln-opt--facts-load)))
    (if (not attrs)
        (cons nil 0)
      (let* ((mtime (float-time (file-attribute-modification-time attrs)))
             (size (file-attribute-size attrs))
             (hit (gethash src table)))
        (puthash src t xl-eln-opt--facts-used)
        (if (and hit (equal (car hit) mtime) (equal (cadr hit) size))
            (cons (nth 2 hit) (nth 3 hit))
          (let* ((optout (xl-eln-opt--opted-out-compute src))
                 (bytes (xl-eln-opt--source-bytes-compute src)))
            (puthash src (list mtime size optout bytes) table)
            (cons optout bytes)))))))

(defun xl-eln-opt--facts-save ()
  "Write the facts of the sources asked for in this process, and no others."
  (when xl-eln-opt--facts-table
    (let (rows)
      (maphash (lambda (src _)
                 (let ((v (gethash src xl-eln-opt--facts-table)))
                   (when (and v (gethash src xl-eln-opt--facts-used))
                     (push (cons src v) rows))))
               xl-eln-opt--facts-table)
      (make-directory xl-eln-opt-state-dir t)
      (with-temp-file (xl-eln-opt--facts-file)
        (prin1 `(setq xl-eln-opt-facts ',rows) (current-buffer))))))

(defun xl-eln-opt--opted-out-p (src)
  "The cookie that opts SRC out, or nil.  Cached; see `xl-eln-opt--facts'."
  (car (xl-eln-opt--facts src)))

(defun xl-eln-opt--source-bytes (src)
  "SRC's uncompressed size in bytes.  Cached; see `xl-eln-opt--facts'."
  (cdr (xl-eln-opt--facts src)))

(defun xl-eln-opt--failed-entry (src)
  "The failed record of SRC as (MTIME REASON TIME), or nil."
  (let ((e (cdr (assoc src (xl-eln-opt--failed)))))
    (cond ((numberp e) (list e "earlier run" "?"))
          (e e))))

(defun xl-eln-opt--existing-eln (src)
  "The .eln Emacs would load for SRC, or nil.  Looks in the user's eln folder
and in every folder of `native-comp-eln-load-path', where Emacs keeps its own
preloaded elns under <version>/preloaded/."
  (let* ((base (file-name-nondirectory (comp-el-to-eln-filename src)))
         (version comp-native-version-dir))
    (seq-some (lambda (dir)
                (seq-find #'file-exists-p
                          (list (expand-file-name base (expand-file-name version dir))
                                (expand-file-name base
                                                  (expand-file-name
                                                   (concat version "/preloaded") dir)))))
              native-comp-eln-load-path)))

(defun xl-eln-opt--ensure-elc (src)
  "Byte-compile SRC to its .elc when that is missing or older than SRC.
Emacs loads an .eln only beside a .elc older than it (as it does for the
system's own libraries); a bare .el never picks the .eln up.  So the .elc is
made first, and the .eln after it."
  (when (string-suffix-p ".el" src)
    (let ((elc (concat src "c")))
      (when (or (not (file-exists-p elc)) (file-newer-than-file-p src elc))
        (require 'bytecomp)
        (let ((byte-compile-verbose nil))
          (byte-compile-file src))))))

(defun xl-eln-opt--remove-stale (src)
  "Delete the .eln files of SRC made from earlier contents of SRC.  The name is
a hash of the path plus a hash of the content, so an edited file leaves its old
.eln behind under another name."
  (let* ((new (comp-el-to-eln-filename src))
         (prefix (replace-regexp-in-string "-[0-9a-f]+\\.eln\\'" "-"
                                           (file-name-nondirectory new))))
    (dolist (f (directory-files (file-name-directory new) t
                                (concat "\\`" (regexp-quote prefix) "[0-9a-f]+\\.eln\\'")))
      (unless (equal f new)
        (ignore-errors (delete-file f))))))

(defun xl-eln-opt--native-current-p (src)
  "Non-nil when SRC is native and Emacs will use it: an .eln not older than
SRC and, for a .el, a .elc not older than SRC with the .eln newer than the
.elc (the order `load' needs; see `xl-eln-opt--ensure-elc')."
  (let ((eln (xl-eln-opt--existing-eln src)))
    (and eln
         (not (file-newer-than-file-p src eln))
         (or (not (string-suffix-p ".el" src))
             (let ((elc (concat src "c")))
               (and (file-exists-p elc)
                    (not (file-newer-than-file-p src elc))
                    (not (file-newer-than-file-p elc eln))))))))

(defun xl-eln-opt--verdict (src)
  "What happens to SRC, as (STATUS . WHY).  STATUS is `todo', `native',
`failed' or `skip'; WHY says why, with the numbers that decided it.  The file
is checked first, so a missing file is skipped and never signals an error."
  (require 'comp nil t)
  (cond
   ((not (xl-eln-opt--trusted-p src)) (cons 'skip "outside the trusted roots"))
   ((xl-eln-opt--opted-out-p src)
    (cons 'skip (format "opts out with %s" (xl-eln-opt--opted-out-p src))))
   ((> (xl-eln-opt--source-bytes src) xl-eln-opt-max-source-bytes)
    (cons 'skip (format "source is %d KB, over the %d KB limit"
                        (/ (xl-eln-opt--source-bytes src) 1024)
                        (/ xl-eln-opt-max-source-bytes 1024))))
   ((xl-eln-opt--native-current-p src)
    (cons 'native "eln already"))
   ((let ((e (xl-eln-opt--failed-entry src)))
      (and e (= (car e) (xl-eln-opt--mtime src))))
    (let ((e (xl-eln-opt--failed-entry src)))
      (cons 'failed (format "failed at %s: %s" (nth 2 e) (nth 1 e)))))
   (t (cons 'todo "to compile"))))

(defun xl-eln-opt--hard-skip-p (src)
  "Non-nil when SRC may never be compiled: outside the trusted roots, or opted
out.  The size limit is not hard: an explicit force request overrides it."
  (or (not (xl-eln-opt--trusted-p src)) (xl-eln-opt--opted-out-p src)))

(defun xl-eln-opt--todo (sources)
  "SOURCES whose verdict is to compile."
  (seq-filter (lambda (src) (eq (car (xl-eln-opt--verdict src)) 'todo)) sources))

(defun xl-eln-opt--lock ()
  "Take the pid file.  Nil when another run is alive."
  (make-directory xl-eln-opt-state-dir t)
  (let* ((pidfile (xl-eln-opt--state "pid"))
         (other (and (file-exists-p pidfile)
                     (ignore-errors (string-to-number
                                     (with-temp-buffer (insert-file-contents pidfile)
                                                       (buffer-string)))))))
    (unless (xl-eln-opt--alive-p other)
      (with-temp-file pidfile (insert (number-to-string (emacs-pid))))
      t)))

(defun xl-eln-opt--unlock ()
  (ignore-errors (delete-file (xl-eln-opt--state "pid"))))

(defvar savehist-file)
(defvar recentf-save-file)

(defun xl-eln-opt--quiet-child ()
  "In the child: write no state when it exits.  The child has loaded the
profile, so savehist, recentf and save-place are on, and they would save this
process's copies over the user's files.  Called first in every child."
  (when (fboundp 'savehist-mode) (savehist-mode -1))
  (when (fboundp 'recentf-mode) (recentf-mode -1))
  (when (fboundp 'save-place-mode) (save-place-mode -1))
  ;; Belt and braces: with no file, nothing can be written, even by a timer.
  (setq savehist-file nil
        recentf-save-file nil
        kill-emacs-hook nil))

(defun xl-eln-opt-run (&optional files force)
  "In the child: compile the todo list one file at a time; record failures.
With FILES, compile those instead of the history; with FORCE, do not skip
the files on the failed list.  Returns the number of files compiled.
While the main Emacs records with `xl-perfetto-trace-start', the run is
recorded too: it follows the marker between files (see
`xl-perfetto-follow-marker'), and each file is a span."
  (require 'comp)
  ;; Only the child runs this: load the follower here, not at init.
  (require 'xl-perfetto-trace)
  (if (not (xl-eln-opt--lock))
      (progn (message "xl-eln-opt: another run is alive") 0)
    (unwind-protect
        ;; Start following before the run span: the span checks at entry
        ;; whether a recording is on, so the marker must be read first.
        (progn
         (xl-perfetto-follow-marker "xl-eln-opt-trace")
         (xl-perfetto-with-span "xl-eln-opt run"
          (let* ((todo (cond ((and files force)
                              (seq-remove #'xl-eln-opt--hard-skip-p files))
                             (files (xl-eln-opt--todo files))
                             (t (xl-eln-opt--todo (xl-eln-opt--history-sources)))))
                 (failed (xl-eln-opt--failed))
                 (done 0))
            (dolist (src todo)
              ;; Between files: start or stop following the main recording.
              (xl-perfetto-follow-marker "xl-eln-opt-trace")
              (xl-perfetto-with-span (format "native %s" (xl-eln-opt--short-name src))
                (condition-case err
                    (progn
                      (xl-eln-opt--ensure-elc src)
                      (native-compile src (comp-el-to-eln-filename src))
                      (if (file-exists-p (comp-el-to-eln-filename src))
                          (progn (xl-eln-opt--remove-stale src) (cl-incf done))
                        (push (list src (xl-eln-opt--mtime src) "no .eln was written"
                                    (format-time-string "%Y-%m-%d %H:%M"))
                              failed)))
                  (error
                   (let ((reason (error-message-string err)))
                     (message "xl-eln-opt: %s failed: %s" src reason)
                     (push (list src (xl-eln-opt--mtime src) reason
                                 (format-time-string "%Y-%m-%d %H:%M"))
                           failed))))))
            (xl-eln-opt--save-failed failed)
            (xl-eln-opt--facts-save)
            (message "xl-eln-opt: %d compiled, %d todo, %d failed" done (length todo) (length failed))
            done)))
      (xl-perfetto-follow-finish)
      (xl-eln-opt--unlock))))

;;; ---- Automatic: once per session, after Emacs has been idle ----

(defcustom xl-eln-opt-mode 1
  "How xl-eln-opt runs by itself.
0: only when you call `xl-eln-opt-start' (M-x).
1: also once per session, after `xl-eln-opt-idle' seconds of idle time."
  :type 'natnum
  :group 'xl-eln-opt)

(defcustom xl-eln-opt-idle 5.0
  "Seconds of idle time before the once-per-session check starts a child."
  :type 'number
  :group 'xl-eln-opt)

(defvar xl-eln-opt--timer nil "The idle timer, or nil once it has run or been disarmed.")
(defvar xl-eln-opt--checked nil "Non-nil once this session has done its check.")

(defun xl-eln-opt--alive-p (pid)
  "Non-nil if PID is a running process.  A zombie (exited, not yet reaped by
its parent) is not running; `signal-process' alone cannot tell the two apart."
  (and (integerp pid) (> pid 0)
       (zerop (or (ignore-errors (signal-process pid 0)) -1))
       (not (equal (cdr (assq 'state (ignore-errors (process-attributes pid)))) "Z"))))

(defun xl-eln-opt--running-p ()
  "Non-nil while a child of an earlier run is alive (its pid file names it)."
  (let* ((pidfile (xl-eln-opt--state "pid"))
         (pid (and (file-exists-p pidfile)
                   (ignore-errors (string-to-number
                                   (with-temp-buffer (insert-file-contents pidfile)
                                                     (buffer-string)))))))
    (xl-eln-opt--alive-p pid)))

(defun xl-eln-opt--idle ()
  "The idle timer's function: start a child, once per session.  The main Emacs
reads no file here; the child decides what there is to compile, and exits at
once when there is nothing."
  (setq xl-eln-opt--timer nil)
  (unless (or xl-eln-opt--checked (xl-eln-opt--running-p))
    (setq xl-eln-opt--checked t)
    (xl-eln-opt-start)))

;;;###autoload
(defun xl-eln-opt-arm ()
  "When `xl-eln-opt-mode' is 1: once per session, after `xl-eln-opt-idle' seconds of
idle time, start a child that compiles what the history needs.  Does nothing
in batch, or when the mode is 0."
  (when (and (> xl-eln-opt-mode 0) (not noninteractive)
             (not xl-eln-opt--checked) (not (timerp xl-eln-opt--timer)))
    (setq xl-eln-opt--timer (run-with-idle-timer xl-eln-opt-idle nil #'xl-eln-opt--idle))))

;;; ---- The report: one line per history entry, what happens to it ----

(defun xl-eln-opt--why-no-source (sym)
  "Why SYM has no source xl-eln-opt can compile, as a string."
  (let ((fn (and (symbolp sym) (fboundp sym) (indirect-function sym))))
    (cond ((null fn) "not loaded in this Emacs (lazy, or not used yet)")
          ((and (subrp fn) (not (subr-native-elisp-p fn))) "C primitive: no Lisp source")
          (t (let* ((file (symbol-file sym 'defun))
                    (el (and file (replace-regexp-in-string "\\.elc\\'" ".el" file))))
               (cond ((null el) "no file")
                     ((not (or (file-exists-p el) (file-exists-p (concat el ".gz"))))
                      "no .el or .el.gz next to it")
                     (t "outside the trusted roots")))))))

(defun xl-eln-opt--items ()
  "The history entries as rows: (LABEL SOURCE WHY), in scope."
  (xl-eln-opt--rows))

(defun xl-eln-opt--status-word (verdict src)
  "What Emacs runs for SRC, as the status word: \"eln\" (native), \"elc\"
(a current .elc, bytecode), \"lisp\" (the source, interpreted), \"failed\" or
\"skipped\"."
  (pcase (car verdict)
    ('native "eln")
    ('failed "failed")
    ('todo (let ((elc (concat (replace-regexp-in-string "\\.el\\(\\.gz\\)?\\'" "" src) ".elc")))
             (if (and (file-exists-p elc) (not (file-newer-than-file-p src elc)))
                 "elc"
               "lisp")))
    (_ "skipped")))

(defun xl-eln-opt--paint ()
  "Give the report its faces.  Only faces are added; the text stays as it is."
  (save-excursion
    (goto-char (point-min))
    (let ((section nil))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring bol eol)))
          (cond
           ((string-match-p "\\`xl-eln-opt report" line)
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
            (when (string-match "\\`\\S-+ +[0-9.]+ +\\(eln\\|elc\\|lisp\\|failed\\|skipped\\) " line)
              (let ((word (match-beginning 1)))
                (add-face-text-property (+ bol word) (+ bol (match-end 1))
                                        (pcase (match-string 1 line)
                                          ("eln" 'success)
                                          ("elc" 'font-lock-keyword-face)
                                          ("lisp" 'warning)
                                          ("skipped" 'shadow)
                                          (_ 'error)))))
            (when (string-match "(xl-eln-opt-compile-file" line)
              (add-face-text-property (+ bol (match-beginning 0)) eol 'shadow))))
          (forward-line 1))))))

(defun xl-eln-opt--short-name (file)
  "FILE's base name without its .el or .el.gz suffix.  The full path is
printed in the last column of the report, so the suffix is not needed."
  (replace-regexp-in-string "\\.el\\(\\.gz\\)?\\'" "" (file-name-nondirectory file)))

(defun xl-eln-opt--report-text ()
  "The report as a string.  Runs in the child; nothing is compiled."
  (let* ((items (xl-eln-opt--items))
         (rows (seq-filter #'cadr items))
         (none (seq-filter (lambda (i) (null (cadr i))) items))
         (srcs (delete-dups (mapcar #'cadr rows)))
         (verdicts (mapcar (lambda (src) (cons src (xl-eln-opt--verdict src))) srcs))
         (todo (seq-filter (lambda (v) (eq (cadr v) 'todo)) verdicts)))
    (with-output-to-string
      (princ "xl-eln-opt report (report only; nothing is compiled here)\n")
      (unless (native-comp-available-p)
        (princ "\nNative compilation is not available in this Emacs: nothing here can be compiled.\n"))
      (princ (format "\nStatus:\n  %d source(s) from your history; %d entr(y/ies) without one; next run compiles %d file(s)\n\n"
                     (length srcs) (length none) (length todo)))
      (princ (format "%-28s %7s  %-10s  %-34s  %s\n" "source" "KB" "status" "from"
                     "force xl-eln-opt-compile-file"))
      (dolist (src srcs)
        (let* ((v (cdr (assoc src verdicts)))
               (from (delete-dups
                      (mapcar (lambda (r) (replace-regexp-in-string "\\`\\(command\\|mode\\|recent\\) " "" (car r)))
                              (seq-filter (lambda (r) (equal (cadr r) src)) rows)))))
          (princ (format "%-28s %7.1f  %-10s  %-34s  (xl-eln-opt-compile-file \"%s\")\n"
                         (xl-eln-opt--short-name src)
                         (/ (xl-eln-opt--source-bytes src) 1024.0)
                         (xl-eln-opt--status-word v src)
                         (truncate-string-to-width (mapconcat #'identity from ", ") 34 nil nil "…")
                         (abbreviate-file-name src)))))
      (let ((skipped (seq-remove (lambda (v) (memq (cadr v) '(todo native))) verdicts)))
        (when skipped
          (princ "\nNot compiled, and why:\n")
          (dolist (v skipped)
            (princ (format "  %-28s %s\n" (xl-eln-opt--short-name (car v)) (cddr v))))))
      (when none
        (princ "\nNot yet compiled:\n")
        (princ (format "  %-40s %-52s%s\n" "" "reason" "force xl-eln-opt-compile"))
        (dolist (r none)
          (let* ((why (nth 2 r))
                 (name (and (string-match "\\`\\(?:command\\|mode\\) \\(.+\\)" (car r))
                            (match-string 1 (car r))))
                 (sym (and name (intern name)))
                 ;; Only an autoload can be loaded by name, so only it gets a hint.
                 (hint (if (and sym (fboundp sym) (autoloadp (symbol-function sym)))
                           (format "(xl-eln-opt-compile '%s)" name)
                         "")))
            (princ (format "  %-40s %-52s%s\n" (car r) why hint)))))
      (princ (format "\nScope (xl-eln-opt-scope): %s\n" xl-eln-opt-scope))
      (princ "\nNOTE:\n  The sources come from the scope above: savehist (commands), minor modes, and recentf (modes and Lisp files).\n")
      (princ "  Built-in libraries count too: their .el.gz is compiled.  A file is skipped\n")
      (princ "  when it is outside the trusted roots, opts out with no-native-compile, or is\n")
      (princ "  over the size limit.  A failure is kept until the file changes.\n")
      (princ "  To compile something now, use the force hints above or M-x xl-eln-opt-start.\n"))))

(defun xl-eln-opt--show-report (buf output)
  "Fill BUF with the report that a child printed into OUTPUT, after its marker."
  (xl-spinner-mode-line-stop)
  (when (buffer-live-p buf)
    (with-current-buffer buf (xl-eln-opt--draw-center nil))
    (let* ((at (string-match "<<XL-ELN-OPT-REPORT>>\n" output))
           (text (if at (substring output (match-end 0))
                   (concat "The report failed:\n" output))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (replace-regexp-in-string "\r" "" text))
          (xl-eln-opt--paint))
        (goto-char (point-min))))))

;; The loader: an overlay with a before-string on an empty placeholder line in
;; the middle of the window.  No text is added, so the window does not move.
(defvar-local xl-eln-opt--center nil)
(defvar-local xl-eln-opt--busy-text "computing the report")

(defun xl-eln-opt--draw-center (frame)
  "Draw FRAME (the spinner's frame text) in the middle of this buffer's window.
nil removes it."
  (if (null frame)
      (when (overlayp xl-eln-opt--center)
        (delete-overlay xl-eln-opt--center)
        (setq xl-eln-opt--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  " xl-eln-opt--busy-text))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (pos (save-excursion
                    (goto-char (window-start window))
                    (forward-line (/ (window-body-height window) 2))
                    (line-beginning-position))))
        (unless (overlayp xl-eln-opt--center)
          (setq xl-eln-opt--center (make-overlay pos pos)))
        (move-overlay xl-eln-opt--center pos pos)
        (overlay-put xl-eln-opt--center 'before-string
                     (concat (make-string pad ?\s) text))))))

;;;###autoload
(defun xl-eln-opt-report ()
  "Show what xl-eln-opt does with each source, in one table row per source.
The report is computed in a child Emacs; the buffer shows a loader until it is
ready.  Nothing is compiled."
  (interactive)
  (let ((buf (get-buffer-create "*xl-eln-opt report*")))
    (pop-to-buffer buf)
    (unless (xl-async-busy-p 'xl-eln-opt-report buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Empty placeholder lines: the loader sits on one of them.
          (insert (make-string 400 ?\n)))
        (setq-local xl-eln-opt--busy-text "computing the report")
        (special-mode))
      (xl-spinner-mode-line-start "xl-eln-opt report" :also #'xl-eln-opt--draw-center)
      (let* ((proc (xl-async-start
                    (xl-eln-opt--child-args
                     "--eval" "(progn (require 'xl-eln-opt) (xl-eln-opt--quiet-child) (princ \"\\n<<XL-ELN-OPT-REPORT>>\\n\") (princ (xl-eln-opt--report-text)))")
                    :name "xl-eln-opt-report")))
        (xl-async-own proc buf)
        (xl-async-slot proc 'xl-eln-opt-report)
        (xl-async-on-exit proc (lambda (p) (xl-eln-opt--show-report buf (xl-async-output p))))))
    buf))

;;; ---- Starting a child: the history, or an explicit list of files ----

(defconst xl-eln-opt--child-prefix
  '("--eval" "(setq savehist-file null-device recentf-save-file null-device save-place-file null-device persistent-scratch-save-file null-device project-list-file null-device)")
  "Arguments that come before init.el in a child: the state files are written to
the null device, so loading init.el (which writes recentf, for one) and exiting
cannot change the user's files.")

(defun xl-eln-opt--child-args (&rest tail)
  "The command line of a child Emacs with the profile, then TAIL: the forms
it evaluates after init.el."
  (xl-async-child-command
   :init-directory user-emacs-directory
   :before-init xl-eln-opt--child-prefix
   :after-init tail))

(defun xl-eln-opt--spawn (files force)
  "Start a child that compiles FILES (or the history when nil)."
  (let* ((proc (xl-async-start
                (xl-eln-opt--child-args
                 "--eval" (format "(progn (require 'xl-eln-opt) (xl-eln-opt--quiet-child) (xl-eln-opt-run '%S %s))"
                                  files (if force "t" "nil")))
                :name "xl-eln-opt")))
    (xl-async-on-exit proc (lambda (p) (message "xl-eln-opt: finished (%s)" (xl-async-exit-code p))))
    (message "xl-eln-opt: started in the background")
    proc))

;;;###autoload
(defun xl-eln-opt-compile-file (file)
  "Native-compile FILE now, in the background, even if it is native already or
failed before."
  (interactive (list (read-file-name "Compile file: " nil nil t)))
  (if (xl-eln-opt--trusted-p file)
      (xl-eln-opt--spawn (list (expand-file-name file)) t)
    (user-error "xl-eln-opt: %s is not in a trusted folder" file)))

;;;###autoload
(defun xl-eln-opt-compile (function)
  "Native-compile now, in the background, the source of FUNCTION, even if it is
not loaded yet (an autoload is loaded first) and even if it failed before."
  (interactive (list (intern (completing-read "Compile the source of: " obarray
                                              #'fboundp t nil nil (thing-at-point 'symbol)))))
  (when (and (fboundp function) (autoloadp (symbol-function function)))
    (autoload-do-load (symbol-function function) function))
  (let ((src (xl-eln-opt--source-of-symbol function)))
    (if src
        (xl-eln-opt--spawn (list src) t)
      (user-error "xl-eln-opt: no source for %s" function))))

;;; ---- Clean: remove what this module compiled, keep the trampolines ----

(defun xl-eln-opt--cache-dir ()
  "The user's eln folder: the first entry of `native-comp-eln-load-path'."
  (and (boundp 'native-comp-eln-load-path) (car native-comp-eln-load-path)))

(defun xl-eln-opt--clean-list ()
  "Every .eln in the user's eln folder except the subr trampolines, which are
costly to rebuild and are not compiled Lisp."
  (let ((dir (xl-eln-opt--cache-dir)))
    (and dir (file-directory-p dir)
         (seq-remove (lambda (f) (string-prefix-p "subr--trampoline-"
                                                  (file-name-nondirectory f)))
                     (directory-files-recursively dir "\\.eln\\'")))))

;;;###autoload
(defun xl-eln-opt-clean ()
  "Delete the .eln files xl-eln-opt built (every eln in the user's eln folder
except the subr trampolines), and the failed list.  Refuses while a child is
running.  Interactive: asks first; the answer must be yes."
  (interactive)
  (let ((files (xl-eln-opt--clean-list)))
    (cond
     ((xl-eln-opt--running-p) (user-error "xl-eln-opt: a child is running; try again later"))
     ((and (not noninteractive)
           (not (yes-or-no-p (format "Delete %d eln file(s) (trampolines are kept)? "
                                     (length files)))))
      (message "xl-eln-opt: clean cancelled") nil)
     (t
      (dolist (f files) (ignore-errors (delete-file f)))
      (ignore-errors (delete-file (xl-eln-opt--state "failed.eld")))
      (unless noninteractive
        (message "xl-eln-opt: deleted %d eln file(s); trampolines kept" (length files)))
      (length files)))))

;;;###autoload
(defun xl-eln-opt-start ()
  "Compile the files of your history in a background child Emacs.
The child loads this profile, so the trampolines of advice are built too."
  (interactive)
  (xl-eln-opt--spawn nil nil))

(provide 'xl-eln-opt)
;;; xl-eln-opt.el ends here
