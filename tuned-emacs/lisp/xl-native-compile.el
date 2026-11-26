;;; xl-native-compile.el --- queue and monitor native compilations -*- lexical-binding: t; -*-

;;; Commentary:

;; `xl-async-native-compile' called without a callback comes here.  Requests
;; go into one queue, so only one child Emacs runs at a time, however fast the
;; requests come.  A file that is already queued or running is not queued again.
;; The monitor buffer `*xl-native-compile*' shows each job in turn, and its mode
;; line has a spinner for the job that is running.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xl-async)
(require 'xl-spinner)

(declare-function xl-async-evaluate-result "xl-async-evaluate" (proc &optional id))
(declare-function xl-async-native-compile--start "xl-async-evaluate" (file args callback))

(defvar xl-native-compile-buffer-name "*xl-native-compile*"
  "Name of the buffer that shows the progress of native compilations.")

(defvar xl-native-compile--queue-key 'xl-native-compile
  "The queue key of native compilations; one child runs at a time for it.")

(defvar xl-native-compile--pending nil
  "Files queued or running, oldest first.  A file is removed when its child ends.")

(define-derived-mode xl-native-compile-mode special-mode "Native-Compile"
  "Monitor of native compilations started by `xl-async-native-compile'."
  (xl-spinner-mode-line-install))

(defun xl-native-compile--buffer ()
  "The monitor buffer, shown if it is not already visible."
  (let ((buffer (get-buffer-create xl-native-compile-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'xl-native-compile-mode)
        (xl-native-compile-mode)))
    (display-buffer buffer)
    buffer))

(defun xl-native-compile--append (buffer text)
  "Append the child's output TEXT to BUFFER.
The lines the child uses for its result protocol are not shown."
  (when (buffer-live-p buffer)
    (let ((shown (replace-regexp-in-string "^XL-ASYNC-RESULT[^\n]*\n?" "" text)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (at-end (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert shown))
          (when at-end (goto-char (point-max))))))))

(defun xl-native-compile--finish (buffer name process)
  "Stop the spinner in BUFFER and report the result of compiling NAME."
  (let ((result (xl-async-evaluate-result process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (if (eq (car-safe result) :ok)
              (progn
                (xl-spinner-mode-line-stop (format "compiled %s" name))
                (insert (format "\nCompiled %s\n-> %s\n" name (cadr result)))
                (message "Native-compiled %s" name))
            (xl-spinner-mode-line-stop (format "native compile of %s failed" name))
            (insert (format "\nFailed: %s\n"
                            (or (cadr result) "no result from the child Emacs")))
            (message "Native compile of %s failed; see %s" name
                     xl-native-compile-buffer-name)))))))

(defun xl-native-compile--label (name)
  "The spinner label for NAME, with how many other files wait behind it."
  (let ((waiting (1- (length xl-native-compile--pending))))
    (if (> waiting 0)
        (format "native-compiling %s (%d waiting)" name waiting)
      (format "native-compiling %s" name))))

(defun xl-native-compile--run (file args)
  "Start the child that compiles FILE with ARGS; the queue runs one at a time.
Return the child process, which the queue waits for."
  (let* ((name (xl-native-compile--short-name file))
         (buffer (xl-native-compile--buffer))
         (process (xl-async-native-compile--start file args nil)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n--- Native-compiling %s\n%s\n" name file)))
      (xl-spinner-mode-line-start (xl-native-compile--label name)))
    (xl-async-finally process
      (lambda (_p) (setq xl-native-compile--pending
                         (delete file xl-native-compile--pending))))
    (thread-first
      process
      (xl-async-own buffer)
      (xl-async-on-output
       (lambda (_process chunk)
         (xl-native-compile--append buffer chunk)))
      (xl-async-on-exit
       (lambda (proc)
         (xl-native-compile--finish buffer name proc))))
    process))

(defun xl-native-compile--short-name (file)
  "The feature name of FILE, without its directory and .el or .el.gz suffix."
  (file-name-nondirectory (file-name-sans-extension
                           (file-name-sans-extension file))))

(defun xl-native-compile-request (file args)
  "Queue the native compilation of FILE with ARGS.
Return a status line: whether the request started at once, waits behind the
file being compiled, or was already queued or running.  It is returned, not
printed, so an evaluation shows it once.  The child process runs when the queue reaches this request."
  (let* ((short (xl-native-compile--short-name file))
         (ahead xl-native-compile--pending)
         (status
          (cond
           ((member file ahead)
            (format "busy: %s is already queued or running" short))
           (ahead
            (format "busy compiling %s; %s waits behind %d other request(s)"
                    (xl-native-compile--short-name (car ahead)) short
                    (length ahead)))
           (t (format "compiling %s" short)))))
    (unless (member file ahead)
      (setq xl-native-compile--pending (append ahead (list file)))
      (xl-async-queue-push
       xl-native-compile--queue-key
       (xl-async-job (lambda () (xl-native-compile--run file args)))))
    status))

(provide 'xl-native-compile)
;;; xl-native-compile.el ends here
