;;; xl-async-evaluate.el --- Evaluate forms in a child Emacs -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))
;; Keywords: processes, compile

;;; Commentary:
;; A task is a child Emacs in batch mode.  The form is not put on the
;; command line: it travels over standard input as one ASCII line, so
;; there is no shell quoting and no command-line length limit.  The child
;; answers with one line per request:
;;
;;   XL-ASYNC-RESULT <ID> (:ok VALUE)
;;   XL-ASYNC-RESULT <ID> (:error MESSAGE)
;;
;; `xl-async-evaluate' sends one request and closes the input.
;; `xl-async-worker-start' keeps the child alive and answers requests in
;; order, so state persists between them.  `xl-async-native-compile' is
;; `(native-compile FILE)' sent to a fresh child.
;;
;; The output buffer is *async-buffer <pid>*, kept after the task ends.
;; Only the ten most recent such buffers are kept; older finished ones
;; are deleted, never a buffer whose child still runs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'xl-async)

(defvar read-eval)                      ; a special variable of the reader

(defvar xl-async-evaluate-program nil
  "The Emacs to run for each request.  Nil means this binary.")

(defconst xl-async-evaluate-result-marker "XL-ASYNC-RESULT "
  "What the child prints before a result.")

(defconst xl-async-evaluate--max-result-bytes (* 8 1024 1024)
  "Longest result line, in bytes, that the parent will read.")

(defcustom xl-async-evaluate-buffer-limit 10
  "Number of *async-buffer* buffers to keep.
Starting a new evaluate or worker deletes the oldest buffers that have
finished until this many remain.  A buffer whose process still runs is
never deleted."
  :type 'natnum
  :group 'processes)

(defconst xl-async-evaluate--bootstrap
  (prin1-to-string
   '(let ((print-escape-newlines t)
          (print-escape-multibyte t)
          (print-escape-nonascii t)
          (print-length nil)
          (print-level nil))
      (condition-case nil
          (while t
            (let* ((line (read-from-minibuffer ""))
                   (request (read line))
                   (id (car request))
                   (form (cdr request)))
              (let ((result
                     (condition-case err
                         (list :ok (eval form t))
                       (error (list :error (error-message-string err))))))
                (terpri)
                (princ "XL-ASYNC-RESULT ")
                (prin1 id)
                (princ " ")
                (prin1 result)
                (terpri)
                (when (fboundp 'flush-standard-output)
                  (flush-standard-output)))))
        (end-of-file nil))))
  "Elisp the child evaluates to serve requests on standard input.")

(defvar-local xl-async-evaluate--sequence nil
  "Creation order of an *async-buffer*, oldest first.")

(defvar xl-async-evaluate--sequence-counter 0
  "Number assigned to the next *async-buffer*.")

;;; ============================================================
;;; Starting children and pruning their buffers
;;; ============================================================

(defun xl-async-evaluate--program ()
  "The Emacs binary used for child processes."
  (or xl-async-evaluate-program
      (expand-file-name invocation-name invocation-directory)))

(defun xl-async-evaluate--command (&optional init-directory)
  "The command line of a child that serves requests on stdin.
Without INIT-DIRECTORY the child is bare (`-Q').  With it, the child starts
the way this Emacs does, with that profile's init, and then serves: the
profile's startup code needs the state a normal start sets up, which a bare
`-Q' load does not have."
  (if init-directory
      ;; Batch Emacs does not load the init files by itself, so they are
      ;; loaded here in the order a normal start uses (as eln-opt did).
      (let ((dir (directory-file-name (expand-file-name init-directory))))
        (list (xl-async-evaluate--program) "--batch"
              "--init-directory" dir
              "-l" (expand-file-name "early-init.el" dir)
              "--eval" "(run-hooks (quote before-init-hook))"
              "-l" (expand-file-name "init.el" dir)
              "--eval" xl-async-evaluate--bootstrap))
    (list (xl-async-evaluate--program) "-Q" "--batch" "--eval"
          xl-async-evaluate--bootstrap)))

(defun xl-async-evaluate--async-buffer-p (buffer)
  "Return non-nil when BUFFER is an *async-buffer <pid>*."
  (and (buffer-live-p buffer)
       (string-match-p "\\`\\*async-buffer [0-9]+\\*\\'" (buffer-name buffer))))

(defun xl-async-evaluate--buffer-finished-p (buffer)
  "Return non-nil when BUFFER's child has run its end flow.
A dead process whose sentinel has not run may still have output in the
pipe, so its buffer is not deleted yet."
  (let ((process (get-buffer-process buffer)))
    (or (null process)
        (and (not (process-live-p process))
             (process-get process 'xl-async-finished)))))

(defun xl-async-evaluate--prune-buffers ()
  "Delete the oldest finished *async-buffer* buffers above the limit."
  (let* ((limit (max 1 xl-async-evaluate-buffer-limit))
         (buffers (seq-filter #'xl-async-evaluate--async-buffer-p
                              (buffer-list)))
         (count (length buffers))
         (finished (seq-filter #'xl-async-evaluate--buffer-finished-p
                               buffers)))
    (dolist (buffer (sort finished
                          (lambda (one other)
                            (< (or (buffer-local-value
                                    'xl-async-evaluate--sequence one)
                                   0)
                               (or (buffer-local-value
                                    'xl-async-evaluate--sequence other)
                                   0)))))
      (when (>= count limit)
        (kill-buffer buffer)
        (setq count (1- count))))))

(defun xl-async-evaluate--mark-buffer (proc)
  "Give PROC's output buffer its place in the pruning order."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq xl-async-evaluate--sequence
              (setq xl-async-evaluate--sequence-counter
                    (1+ xl-async-evaluate--sequence-counter)))))))

;;; ============================================================
;;; Requests and results
;;; ============================================================

(defun xl-async-evaluate--request-string (id form)
  "Return one ASCII line holding ID and FORM."
  (let ((print-escape-newlines t)
        (print-escape-multibyte t)
        (print-escape-nonascii t)
        (print-length nil)
        (print-level nil))
    (concat (prin1-to-string (cons id form)) "\n")))

(defun xl-async-evaluate--with-load-path (proc form)
  "Wrap FORM in PROC's :load-path, when it has one."
  (let ((directories (process-get proc 'xl-async-evaluate-load-path)))
    (if directories
        `(let ((load-path (append ',directories load-path))) ,form)
      form)))

(defun xl-async-evaluate--request (proc id form)
  "Send FORM to PROC under ID."
  (xl-async-send proc
                 (xl-async-evaluate--request-string
                  id (xl-async-evaluate--with-load-path proc form))))

(defun xl-async-evaluate--register (proc id callback)
  "Call CALLBACK with PROC when ID's result arrives."
  (process-put proc 'xl-async-evaluate-requests
               (cons (cons id callback)
                     (process-get proc 'xl-async-evaluate-requests))))

(defun xl-async-evaluate--deliver (proc id result)
  "Store RESULT for ID on PROC and call its callback once."
  (process-put proc 'xl-async-evaluate-last-result result)
  (unless (assoc id (process-get proc 'xl-async-evaluate-results))
    (process-put proc 'xl-async-evaluate-results
                 (cons (cons id result)
                       (process-get proc 'xl-async-evaluate-results))))
  (let ((entry (assoc id (process-get proc 'xl-async-evaluate-requests))))
    (when entry
      (process-put proc 'xl-async-evaluate-requests
                   (assq-delete-all
                    id (process-get proc 'xl-async-evaluate-requests)))
      (condition-case err
          (funcall (cdr entry) proc)
        (error
         (message "xl-async-evaluate: callback for %s failed: %s"
                  id (error-message-string err)))))))

(defun xl-async-evaluate-result (proc &optional id)
  "Return PROC's result: (:ok VALUE) or (:error MESSAGE).
With ID, return that request's result.  Without it, return the result
delivered last, which is the one a worker callback is handling."
  (if id
      (cdr (assq id (process-get proc 'xl-async-evaluate-results)))
    (process-get proc 'xl-async-evaluate-last-result)))

(defun xl-async-evaluate--line-id (line)
  "Return the request id at the start of the result LINE, or nil."
  (let* ((rest (substring line (length xl-async-evaluate-result-marker)))
         (space (string-match " " rest)))
    (when space
      (let ((read-eval nil))
        (let ((id (ignore-errors (read (substring rest 0 space)))))
          (and (integerp id) id))))))

(defun xl-async-evaluate--handle-line (proc line)
  "Parse LINE from PROC and deliver any result it carries."
  (when (string-prefix-p xl-async-evaluate-result-marker line)
    (cond
     ((> (string-bytes line) xl-async-evaluate--max-result-bytes)
      (when-let* ((id (xl-async-evaluate--line-id line)))
        (xl-async-evaluate--deliver
         proc id
         (list :error
               (format "result line too long (%d bytes)"
                       (string-bytes line))))))
     (t
      (let* ((rest (substring line
                              (length xl-async-evaluate-result-marker)))
             (space (string-match " " rest)))
        (when space
          (let* ((read-eval nil)
                 (id (ignore-errors
                       (read (substring rest 0 space))))
                 (text (substring rest (1+ space))))
            (when (integerp id)
              (let ((result
                     (condition-case err
                         (read text)
                       (error
                        (list :error
                              (format "unreadable result %s: %s"
                                      text (error-message-string err)))))))
                (xl-async-evaluate--deliver proc id result))))))))))

(defun xl-async-evaluate--output (proc chunk)
  "Split CHUNK into lines and hand each complete line to PROC's parser."
  (let ((text (concat (or (process-get proc 'xl-async-evaluate-line) "")
                      chunk)))
    (while (string-match "\n" text)
      (let ((line (substring text 0 (match-beginning 0))))
        (setq text (substring text (match-end 0)))
        (xl-async-evaluate--handle-line proc line)))
    ;; Do not grow without bound when a line never ends.
    (when (> (string-bytes text) xl-async-evaluate--max-result-bytes)
      (when (string-prefix-p xl-async-evaluate-result-marker text)
        (when-let* ((id (xl-async-evaluate--line-id text)))
          (xl-async-evaluate--deliver
           proc id
           (list :error
                 (format "result line too long (over %d bytes)"
                         xl-async-evaluate--max-result-bytes)))))
      (setq text ""))
    (process-put proc 'xl-async-evaluate-line text)))

;;; ============================================================
;;; Ending
;;; ============================================================

(defun xl-async-evaluate--exit-reason (proc)
  "Describe why PROC ended, for an error result."
  (pcase (xl-async-status proc)
    ('timeout "timed out")
    ('cancelled "cancelled")
    (_ (format "exit code %s" (xl-async-exit-code proc)))))

(defun xl-async-evaluate--finish (proc)
  "Fail requests that PROC left unanswered when it ended."
  (process-put proc 'xl-async-evaluate-line "")
  (let ((kind (process-get proc 'xl-async-evaluate-kind))
        (reason (xl-async-evaluate--exit-reason proc))
        (pending (copy-sequence
                  (process-get proc 'xl-async-evaluate-requests))))
    ;; Stopping a worker on purpose drops its pending callbacks.
    (unless (process-get proc 'xl-async-evaluate-stop-requested)
      (dolist (entry pending)
        (xl-async-evaluate--deliver
         proc (car entry)
         (list :error
               (if (eq kind 'worker)
                   (format "worker exited: %s" reason)
                 (format "no result; the child ended: %s" reason)))))))
  (process-put proc 'xl-async-evaluate-requests nil))

(defun xl-async-evaluate--init (proc kind directories)
  "Prepare PROC, a child of KIND, to serve requests.
DIRECTORIES is prepended to every request's load-path."
  (xl-async-evaluate--mark-buffer proc)
  (process-put proc 'xl-async-evaluate-kind kind)
  (process-put proc 'xl-async-evaluate-load-path directories)
  (process-put proc 'xl-async-evaluate-requests nil)
  (process-put proc 'xl-async-evaluate-results nil)
  (process-put proc 'xl-async-evaluate-last-result nil)
  (process-put proc 'xl-async-evaluate-line "")
  (process-put proc 'xl-async-evaluate-next-id 0)
  (xl-async-on-output proc #'xl-async-evaluate--output)
  (xl-async-finally proc #'xl-async-evaluate--finish))

;;; ============================================================
;;; Evaluate
;;; ============================================================

(defun xl-async-evaluate (form &optional callback &rest args)
  "Evaluate FORM in a fresh child Emacs and return the process.
ARGS may give `:load-path', a list of directories prepended to the
child's load-path, and `:init-directory', a profile folder the child
starts from, its init running as in a normal start.  CALLBACK, when given, is called once with the
process; read the result with `xl-async-evaluate-result'.  The output
buffer is *async-buffer <pid>* and stays after the child ends."
  (xl-async-evaluate--prune-buffers)
  (let ((proc (xl-async-start
               (xl-async-evaluate--command (plist-get args :init-directory))
               :keep-input t
               :buffer-name "*async-buffer %d*"
               :name "xl-async-evaluate")))
    (xl-async-evaluate--init proc 'evaluate (plist-get args :load-path))
    (when callback
      (xl-async-evaluate--register proc 0 callback))
    (xl-async-evaluate--request proc 0 form)
    (xl-async-close-input proc)
    proc))

;;; ============================================================
;;; Worker
;;; ============================================================

(defun xl-async-worker-start (&rest args)
  "Start a child Emacs that keeps its state between requests.
ARGS may give `:load-path' (a list of directories), `:directory',
`:coding' and `:name'.  Return the process, so ownership and timeouts
apply to the worker like any other `xl-async-start' task."
  (xl-async-evaluate--prune-buffers)
  (let ((default-directory (or (plist-get args :directory)
                               default-directory)))
    (let ((proc (xl-async-start
                 (xl-async-evaluate--command)
                 :keep-input t
                 :buffer-name "*async-buffer %d*"
                 :coding (plist-get args :coding)
                 :name (or (plist-get args :name) "xl-async-worker"))))
      (xl-async-evaluate--init proc 'worker (plist-get args :load-path))
      proc)))

(defun xl-async-worker-eval (worker form &optional callback)
  "Evaluate FORM in WORKER and return the request ID.
CALLBACK, when given, is called once with WORKER; read that request's
result with `xl-async-evaluate-result'.  Requests are served in order."
  (let ((id (process-get worker 'xl-async-evaluate-next-id)))
    (process-put worker 'xl-async-evaluate-next-id (1+ id))
    (if (xl-async-live-p worker)
        (progn
          (when callback
            (xl-async-evaluate--register worker id callback))
          (xl-async-evaluate--request worker id form))
      (when callback
        (xl-async-evaluate--register worker id callback))
      (xl-async-evaluate--deliver
       worker id
       (list :error
             (format "worker exited: %s"
                     (xl-async-evaluate--exit-reason worker)))))
    id))

(defun xl-async-worker-stop (worker)
  "Close WORKER's input and drop its pending callbacks.
The child exits after the request it is running.  Return WORKER."
  (process-put worker 'xl-async-evaluate-stop-requested t)
  (process-put worker 'xl-async-evaluate-requests nil)
  (xl-async-close-input worker)
  worker)

;;; ============================================================
;;; Native compilation
;;; ============================================================

(declare-function xl-native-compile-watch "xl-native-compile" (process file))

(defun xl-async-evaluate--source-of (library)
  "The source file behind LIBRARY, a path from `locate-library'.
Returns the .el next to it, or the .el.gz Emacs ships for its own libraries;
nil when neither exists."
  (let ((base (replace-regexp-in-string
               "\\.\\(elc\\|el\\(\\.gz\\)?\\)\\'" "" library)))
    (cl-some (lambda (ext)
               (let ((file (concat base ext)))
                 (and (file-exists-p file) file)))
             '(".el" ".el.gz"))))

(defun xl-async-native-compile (file &optional callback &rest args)
  "Compile FILE with `native-compile' in a fresh child Emacs.
FILE is a source file, or a feature symbol such as `consult', which is
resolved with `locate-library' to its source (.el or .el.gz).  A symbol is
compiled with this Emacs's profile, so that package macros resolve.
ARGS may give `:load-path', a list of directories the child needs to
read FILE's dependencies, and `:profile', a profile folder whose
early-init and init the child loads first.  The profile is needed for
files that require other parts of the configuration or use package
macros.  CALLBACK, when given, is called once with
the process, as `xl-async-evaluate' does; `xl-async-evaluate-result'
then holds (:ok ELN-PATH) or (:error MESSAGE).  Without CALLBACK, the
compilation is shown in the monitor buffer `*xl-native-compile*', with a
spinner in its mode line (see xl-native-compile.el).

Without native compilation this signals a `user-error' and starts
nothing, so CALLBACK is never called with anything but a process.
Check `native-comp-available-p' first when that is possible."
  (unless (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
    (user-error "xl-async-native-compile: native compilation is not available"))
  (when (symbolp file)
    (let* ((library (or (locate-library (symbol-name file))
                        ;; Loaded by path, not through `load-path' (such as
                        ;; xueliang.el): find the file in `load-history'.
                        (cl-find-if
                         (lambda (f)
                           (string-match-p
                            (format "/%s\\.el\\(c\\|\\.gz\\)?\\'"
                                    (regexp-quote (symbol-name file)))
                            f))
                         (delq nil (mapcar #'car load-history)))))
           (source (and library (xl-async-evaluate--source-of library))))
      (unless source
        (user-error "xl-async-native-compile: no source file for `%s'" file))
      (setq file source)
      (unless (plist-get args :profile)
        (setq args (plist-put args :profile user-emacs-directory)))))
  ;; With :profile, the child starts from that profile, as a normal start
  ;; does, so the file's requires and package macros resolve.
  (let* ((profile (plist-get args :profile)))
    ;; The child must write its .eln where this Emacs looks for it, so the
    ;; child takes this Emacs's `native-comp-eln-load-path' (set after its
    ;; init, which may change it).
    (let ((process
           (apply #'xl-async-evaluate
                  (list 'progn
                        (list 'setq 'native-comp-eln-load-path
                              (list 'quote native-comp-eln-load-path))
                        (list 'native-compile file))
                  callback
                  (append (and profile (list :init-directory profile)) args))))
      ;; Without a callback, show the compilation in its monitor buffer.
      (unless callback
        (require 'xl-native-compile)
        (xl-native-compile-watch process file))
      process)))

(provide 'xl-async-evaluate)
;;; xl-async-evaluate.el ends here
