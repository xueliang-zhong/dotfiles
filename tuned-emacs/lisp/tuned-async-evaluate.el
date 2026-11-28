;;; tuned-async-evaluate.el --- Evaluate forms in a child Emacs -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tuned-async "0.1.0"))
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
;; `tuned-async-evaluate' sends one request and closes the input.
;; `tuned-async-worker-start' keeps the child alive and answers requests in
;; order, so state persists between them.  `tuned-async-child-command' builds
;; the command line every child Emacs of this config uses; native
;; compilation (tuned-native-compile.el) is `(native-compile FILE)' sent to a
;; fresh child.
;;
;; The output buffer is *async-buffer <pid>*, kept after the task ends.
;; Only the ten most recent such buffers are kept; older finished ones
;; are deleted, never a buffer whose child still runs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tuned-async)

(defvar read-eval)                      ; a special variable of the reader

(defvar tuned-async-evaluate-program nil
  "The Emacs to run for each request.  Nil means this binary.")

(defconst tuned-async-evaluate-result-marker "XL-ASYNC-RESULT "
  "What the child prints before a result.")

(defconst tuned-async-evaluate--max-result-bytes (* 8 1024 1024)
  "Longest result line, in bytes, that the parent will read.")

(defcustom tuned-async-evaluate-buffer-limit 10
  "Number of *async-buffer* buffers to keep.
Starting a new evaluate or worker deletes the oldest buffers that have
finished until this many remain.  A buffer whose process still runs is
never deleted."
  :type 'natnum
  :group 'processes)

(defconst tuned-async-evaluate--bootstrap
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

(defvar-local tuned-async-evaluate--sequence nil
  "Creation order of an *async-buffer*, oldest first.")

(defvar tuned-async-evaluate--sequence-counter 0
  "Number assigned to the next *async-buffer*.")

;;; ============================================================
;;; Starting children and pruning their buffers
;;; ============================================================

(defun tuned-async-child-command (&rest args)
  "Return the command line of a child Emacs.
ARGS is a plist:

  :program         the Emacs binary, default `tuned-async-evaluate-program'
                   and then this Emacs
  :init-directory  a profile folder; the child starts from it as a normal
                   start does, loading early-init.el and init.el.  Without
                   it the child is bare (`-Q').
  :before-init     arguments inserted before init.el (profile children only),
                   such as settings that must apply while init runs
  :after-init      arguments appended after the init files
  :form            a Lisp form string evaluated after the init files

Every child Emacs of this config is built here, so the command line and the
order of its parts exist once."
  (let* ((program (or (plist-get args :program)
                      tuned-async-evaluate-program
                      (expand-file-name invocation-name invocation-directory)))
         (init-directory (plist-get args :init-directory))
         (form (plist-get args :form))
         (tail (append (plist-get args :after-init)
                       (and form (list "--eval" form)))))
    (if init-directory
        ;; Batch Emacs does not load the init files by itself, so they are
        ;; loaded here in the order a normal start uses.
        (let ((dir (directory-file-name (expand-file-name init-directory))))
          (append (list program "--batch"
                        "--init-directory" dir
                        "-l" (expand-file-name "early-init.el" dir)
                        "--eval" "(run-hooks (quote before-init-hook))")
                  (plist-get args :before-init)
                  (list "-l" (expand-file-name "init.el" dir))
                  tail))
      (append (list program "-Q" "--batch") tail))))

(defun tuned-async-evaluate--command (&optional init-directory)
  "The command line of a child that serves requests on stdin.
Without INIT-DIRECTORY the child is bare (`-Q').  With it, the child starts
the way this Emacs does, with that profile's init, and then serves: the
profile's startup code needs the state a normal start sets up, which a bare
`-Q' load does not have."
  (tuned-async-child-command
   :init-directory init-directory
   :form tuned-async-evaluate--bootstrap))

(defun tuned-async-evaluate--async-buffer-p (buffer)
  "Return non-nil when BUFFER is an *async-buffer <pid>*."
  (and (buffer-live-p buffer)
       (string-match-p "\\`\\*async-buffer [0-9]+\\*\\'" (buffer-name buffer))))

(defun tuned-async-evaluate--buffer-finished-p (buffer)
  "Return non-nil when BUFFER's child has run its end flow.
A dead process whose sentinel has not run may still have output in the
pipe, so its buffer is not deleted yet."
  (let ((process (get-buffer-process buffer)))
    (or (null process)
        (and (not (process-live-p process))
             (process-get process 'tuned-async-finished)))))

(defun tuned-async-evaluate--prune-buffers ()
  "Delete the oldest finished *async-buffer* buffers above the limit."
  (let* ((limit (max 1 tuned-async-evaluate-buffer-limit))
         (buffers (seq-filter #'tuned-async-evaluate--async-buffer-p
                              (buffer-list)))
         (count (length buffers))
         (finished (seq-filter #'tuned-async-evaluate--buffer-finished-p
                               buffers)))
    (dolist (buffer (sort finished
                          (lambda (one other)
                            (< (or (buffer-local-value
                                    'tuned-async-evaluate--sequence one)
                                   0)
                               (or (buffer-local-value
                                    'tuned-async-evaluate--sequence other)
                                   0)))))
      (when (>= count limit)
        (kill-buffer buffer)
        (setq count (1- count))))))

(defun tuned-async-evaluate--mark-buffer (proc)
  "Give PROC's output buffer its place in the pruning order."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq tuned-async-evaluate--sequence
              (setq tuned-async-evaluate--sequence-counter
                    (1+ tuned-async-evaluate--sequence-counter)))))))

;;; ============================================================
;;; Requests and results
;;; ============================================================

(defun tuned-async-evaluate--request-string (id form)
  "Return one ASCII line holding ID and FORM."
  (let ((print-escape-newlines t)
        (print-escape-multibyte t)
        (print-escape-nonascii t)
        (print-length nil)
        (print-level nil))
    (concat (prin1-to-string (cons id form)) "\n")))

(defun tuned-async-evaluate--with-load-path (proc form)
  "Wrap FORM in PROC's :load-path, when it has one."
  (let ((directories (process-get proc 'tuned-async-evaluate-load-path)))
    (if directories
        `(let ((load-path (append ',directories load-path))) ,form)
      form)))

(defun tuned-async-evaluate--request (proc id form)
  "Send FORM to PROC under ID."
  (tuned-async-send proc
                 (tuned-async-evaluate--request-string
                  id (tuned-async-evaluate--with-load-path proc form))))

(defun tuned-async-evaluate--register (proc id callback)
  "Call CALLBACK with PROC when ID's result arrives."
  (process-put proc 'tuned-async-evaluate-requests
               (cons (cons id callback)
                     (process-get proc 'tuned-async-evaluate-requests))))

(defun tuned-async-evaluate--deliver (proc id result)
  "Store RESULT for ID on PROC and call its callback once."
  (process-put proc 'tuned-async-evaluate-last-result result)
  (unless (assoc id (process-get proc 'tuned-async-evaluate-results))
    (process-put proc 'tuned-async-evaluate-results
                 (cons (cons id result)
                       (process-get proc 'tuned-async-evaluate-results))))
  (let ((entry (assoc id (process-get proc 'tuned-async-evaluate-requests))))
    (when entry
      (process-put proc 'tuned-async-evaluate-requests
                   (assq-delete-all
                    id (process-get proc 'tuned-async-evaluate-requests)))
      (condition-case err
          (funcall (cdr entry) proc)
        (error
         (message "tuned-async-evaluate: callback for %s failed: %s"
                  id (error-message-string err)))))))

(defun tuned-async-evaluate-result (proc &optional id)
  "Return PROC's result: (:ok VALUE) or (:error MESSAGE).
With ID, return that request's result.  Without it, return the result
delivered last, which is the one a worker callback is handling."
  (if id
      (cdr (assq id (process-get proc 'tuned-async-evaluate-results)))
    (process-get proc 'tuned-async-evaluate-last-result)))

(defun tuned-async-evaluate--line-id (line)
  "Return the request id at the start of the result LINE, or nil."
  (let* ((rest (substring line (length tuned-async-evaluate-result-marker)))
         (space (string-match " " rest)))
    (when space
      (let ((read-eval nil))
        (let ((id (ignore-errors (read (substring rest 0 space)))))
          (and (integerp id) id))))))

(defun tuned-async-evaluate--handle-line (proc line)
  "Parse LINE from PROC and deliver any result it carries."
  (when (string-prefix-p tuned-async-evaluate-result-marker line)
    (cond
     ((> (string-bytes line) tuned-async-evaluate--max-result-bytes)
      (when-let* ((id (tuned-async-evaluate--line-id line)))
        (tuned-async-evaluate--deliver
         proc id
         (list :error
               (format "result line too long (%d bytes)"
                       (string-bytes line))))))
     (t
      (let* ((rest (substring line
                              (length tuned-async-evaluate-result-marker)))
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
                (tuned-async-evaluate--deliver proc id result))))))))))

(defun tuned-async-evaluate--output (proc chunk)
  "Split CHUNK into lines and hand each complete line to PROC's parser."
  (let ((text (concat (or (process-get proc 'tuned-async-evaluate-line) "")
                      chunk)))
    (while (string-match "\n" text)
      (let ((line (substring text 0 (match-beginning 0))))
        (setq text (substring text (match-end 0)))
        (tuned-async-evaluate--handle-line proc line)))
    ;; Do not grow without bound when a line never ends.
    (when (> (string-bytes text) tuned-async-evaluate--max-result-bytes)
      (when (string-prefix-p tuned-async-evaluate-result-marker text)
        (when-let* ((id (tuned-async-evaluate--line-id text)))
          (tuned-async-evaluate--deliver
           proc id
           (list :error
                 (format "result line too long (over %d bytes)"
                         tuned-async-evaluate--max-result-bytes)))))
      (setq text ""))
    (process-put proc 'tuned-async-evaluate-line text)))

;;; ============================================================
;;; Ending
;;; ============================================================

(defun tuned-async-evaluate--exit-reason (proc)
  "Describe why PROC ended, for an error result."
  (pcase (tuned-async-status proc)
    ('timeout "timed out")
    ('cancelled "cancelled")
    (_ (format "exit code %s" (tuned-async-exit-code proc)))))

(defun tuned-async-evaluate--finish (proc)
  "Fail requests that PROC left unanswered when it ended."
  (process-put proc 'tuned-async-evaluate-line "")
  (let ((kind (process-get proc 'tuned-async-evaluate-kind))
        (reason (tuned-async-evaluate--exit-reason proc))
        (pending (copy-sequence
                  (process-get proc 'tuned-async-evaluate-requests))))
    ;; Stopping a worker on purpose drops its pending callbacks.
    (unless (process-get proc 'tuned-async-evaluate-stop-requested)
      (dolist (entry pending)
        (tuned-async-evaluate--deliver
         proc (car entry)
         (list :error
               (if (eq kind 'worker)
                   (format "worker exited: %s" reason)
                 (format "no result; the child ended: %s" reason)))))))
  (process-put proc 'tuned-async-evaluate-requests nil))

(defun tuned-async-evaluate--init (proc kind directories)
  "Prepare PROC, a child of KIND, to serve requests.
DIRECTORIES is prepended to every request's load-path."
  (tuned-async-evaluate--mark-buffer proc)
  (process-put proc 'tuned-async-evaluate-kind kind)
  (process-put proc 'tuned-async-evaluate-load-path directories)
  (process-put proc 'tuned-async-evaluate-requests nil)
  (process-put proc 'tuned-async-evaluate-results nil)
  (process-put proc 'tuned-async-evaluate-last-result nil)
  (process-put proc 'tuned-async-evaluate-line "")
  (process-put proc 'tuned-async-evaluate-next-id 0)
  (tuned-async-on-output proc #'tuned-async-evaluate--output)
  (tuned-async-finally proc #'tuned-async-evaluate--finish))

;;; ============================================================
;;; Evaluate
;;; ============================================================

;;;###autoload
(defun tuned-async-evaluate (form &optional callback &rest args)
  "Evaluate FORM in a fresh child Emacs and return the process.
ARGS may give `:load-path', a list of directories prepended to the
child's load-path, and `:init-directory', a profile folder the child
starts from, its init running as in a normal start.  CALLBACK, when given, is called once with the
process; read the result with `tuned-async-evaluate-result'.  The output
buffer is *async-buffer <pid>* and stays after the child ends."
  (tuned-async-evaluate--prune-buffers)
  (let ((proc (tuned-async-start
               (tuned-async-evaluate--command (plist-get args :init-directory))
               :keep-input t
               :buffer-name "*async-buffer %d*"
               :name "tuned-async-evaluate")))
    (tuned-async-evaluate--init proc 'evaluate (plist-get args :load-path))
    (when callback
      (tuned-async-evaluate--register proc 0 callback))
    (tuned-async-evaluate--request proc 0 form)
    (tuned-async-close-input proc)
    proc))

;;; ============================================================
;;; Worker
;;; ============================================================

(defun tuned-async-worker-start (&rest args)
  "Start a child Emacs that keeps its state between requests.
ARGS may give `:load-path' (a list of directories), `:directory',
`:coding' and `:name'.  Return the process, so ownership and timeouts
apply to the worker like any other `tuned-async-start' task."
  (tuned-async-evaluate--prune-buffers)
  (let ((default-directory (or (plist-get args :directory)
                               default-directory)))
    (let ((proc (tuned-async-start
                 (tuned-async-evaluate--command)
                 :keep-input t
                 :buffer-name "*async-buffer %d*"
                 :coding (plist-get args :coding)
                 :name (or (plist-get args :name) "tuned-async-worker"))))
      (tuned-async-evaluate--init proc 'worker (plist-get args :load-path))
      proc)))

(defun tuned-async-worker-eval (worker form &optional callback)
  "Evaluate FORM in WORKER and return the request ID.
CALLBACK, when given, is called once with WORKER; read that request's
result with `tuned-async-evaluate-result'.  Requests are served in order."
  (let ((id (process-get worker 'tuned-async-evaluate-next-id)))
    (process-put worker 'tuned-async-evaluate-next-id (1+ id))
    (if (tuned-async-live-p worker)
        (progn
          (when callback
            (tuned-async-evaluate--register worker id callback))
          (tuned-async-evaluate--request worker id form))
      (when callback
        (tuned-async-evaluate--register worker id callback))
      (tuned-async-evaluate--deliver
       worker id
       (list :error
             (format "worker exited: %s"
                     (tuned-async-evaluate--exit-reason worker)))))
    id))

(defun tuned-async-worker-stop (worker)
  "Close WORKER's input and drop its pending callbacks.
The child exits after the request it is running.  Return WORKER."
  (process-put worker 'tuned-async-evaluate-stop-requested t)
  (process-put worker 'tuned-async-evaluate-requests nil)
  (tuned-async-close-input worker)
  worker)

(provide 'tuned-async-evaluate)
;;; tuned-async-evaluate.el ends here
