;;; xl-async.el --- Owned, cancellable subprocesses and job queues  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, processes

;;; Commentary:
;; Every async feature of this config does the same few things: start a
;; command, collect its output, hand it to a callback in the right buffer,
;; and throw the answer away when a newer request or a killed buffer made it
;; irrelevant.  This file does them once, on top of `make-process':
;;
;;   (xl-async-run
;;    :command '("git" "blame" "--line-porcelain" "--" "init.el")
;;    :directory root      ; default-directory of the process
;;    :owner buffer        ; default: the current buffer
;;    :slot 'blame         ; a newer run in the same slot cancels this one
;;    :stdin patch         ; optional text written to the process
;;    :on-done (lambda (output code) ...))
;;
;; ON-DONE runs with OWNER current, and never runs when OWNER was killed or
;; the run was cancelled, so callers need no request counters or "is this
;; still my process" checks.  Killing OWNER kills what it owns, and the
;; ownership survives a major-mode change in OWNER.
;;
;; `xl-async-enqueue' runs jobs of one slot strictly in order (a fast
;; `s s s' stages in order instead of racing), retries a job's next command
;; when one fails, and calls ON-DRAIN once the slot's queue is empty.
;;
;; Output is plain text: stdout and stderr together, as git writes them.
;; Parsing stays with the caller, who knows the format.

;;; Code:

(require 'seq) ; seq-some

(defvar-local xl-async--processes nil
  "Live processes owned by this buffer, newest first.")
(put 'xl-async--processes 'permanent-local t)

(defvar-local xl-async--queues nil
  "Job queues of this buffer, an alist of (SLOT JOBS ON-DRAIN).")
(put 'xl-async--queues 'permanent-local t)

(defun xl-async--kill-owned ()
  "Kill the processes this buffer owns; it is going away."
  (xl-async-cancel))
(put 'xl-async--kill-owned 'permanent-local-hook t)

(defconst xl-async-timeout-code 124
  "The exit code ON-DONE gets when a run was stopped by its :timeout.
It is the code of coreutils `timeout', so callers can treat both alike.")

(defun xl-async--exit-code (proc)
  "Return the exit code of the finished PROC, or the timeout code."
  (if (process-get proc 'xl-async-timed-out)
      xl-async-timeout-code
    (process-exit-status proc)))

(defun xl-async--stop-timer (proc)
  "Cancel PROC's timeout timer, if it has one."
  (let ((timer (process-get proc 'xl-async-timer)))
    (when (timerp timer) (cancel-timer timer))
    (process-put proc 'xl-async-timer nil)))

(defun xl-async--sentinel (proc _event)
  "Deliver PROC's output to its owner once it has exited."
  (unless (process-live-p proc)
    (xl-async--stop-timer proc)
    (let* ((out (process-buffer proc))
           (output (if (buffer-live-p out)
                       (with-current-buffer out (buffer-string))
                     ""))
           (owner (process-get proc 'xl-async-owner))
           (on-done (process-get proc 'xl-async-on-done)))
      (when (buffer-live-p out) (kill-buffer out))
      (cond
       ((process-get proc 'xl-async-unowned)
        (when on-done
          (funcall on-done output (xl-async--exit-code proc))))
       ((buffer-live-p owner)
        (with-current-buffer owner
          ;; A cancelled run was removed from the list before it was killed.
          (when (memq proc xl-async--processes)
            (setq xl-async--processes (delq proc xl-async--processes))
            (when on-done
              (funcall on-done output (xl-async--exit-code proc))))))))))

(defun xl-async--filter (proc chunk)
  "Append CHUNK, output of PROC, to its buffer and to its :on-output callback."
  (let ((out (process-buffer proc)))
    (when (buffer-live-p out)
      (with-current-buffer out
        (save-excursion
          (goto-char (point-max))
          (insert chunk))))
    (let ((on-output (process-get proc 'xl-async-on-output))
          (owner (process-get proc 'xl-async-owner)))
      (when on-output
        (if (buffer-live-p owner)
            (with-current-buffer owner (funcall on-output chunk))
          (funcall on-output chunk))))))

(defun xl-async-run (&rest args)
  "Start a process described by the plist ARGS and return it.

  :command    the program and its arguments (required)
  :directory  the process's working directory, default `default-directory'
  :owner      the buffer that owns it, default the current buffer;
              nil makes the run unowned: killing a buffer never stops
              it, ON-DONE always runs, and it cannot take a :slot
  :slot       a symbol; starting a run cancels OWNER's older run in it
  :stdin      text written to the process before its input is closed
  :name       the process name, default \"xl-async\"
  :on-done    called as (ON-DONE OUTPUT EXIT-CODE) with OWNER current
  :on-output  called as (ON-OUTPUT CHUNK) with OWNER current each time the
              process writes; OUTPUT in ON-DONE still has all of it
  :timeout    seconds; the process is then killed and ON-DONE gets the
              exit code `xl-async-timeout-code'

ON-DONE is skipped when OWNER was killed or the run was cancelled."
  (let* ((unowned (and (plist-member args :owner)
                       (null (plist-get args :owner))))
         (owner (unless unowned (or (plist-get args :owner) (current-buffer))))
         (slot (unless unowned (plist-get args :slot)))
         (stdin (plist-get args :stdin))
         (default-directory (or (plist-get args :directory) default-directory))
         (out (generate-new-buffer " *xl-async*"))
         proc)
    (when slot
      (with-current-buffer owner (xl-async-cancel slot)))
    (setq proc (make-process
                :name (or (plist-get args :name) "xl-async")
                :buffer out
                :command (plist-get args :command)
                :connection-type 'pipe
                :noquery t
                :sentinel #'xl-async--sentinel
                :filter (and (plist-get args :on-output) #'xl-async--filter)))
    (process-put proc 'xl-async-owner owner)
    (process-put proc 'xl-async-slot slot)
    (process-put proc 'xl-async-unowned unowned)
    (process-put proc 'xl-async-on-done (plist-get args :on-done))
    (process-put proc 'xl-async-on-output (plist-get args :on-output))
    (when-let* ((seconds (plist-get args :timeout)))
      (process-put proc 'xl-async-timer
                   (run-with-timer seconds nil
                                   (lambda ()
                                     (when (process-live-p proc)
                                       (process-put proc 'xl-async-timed-out t)
                                       (delete-process proc))))))
    (when owner
      (with-current-buffer owner
        (push proc xl-async--processes)
        (add-hook 'kill-buffer-hook #'xl-async--kill-owned nil t)))
    (when stdin (process-send-string proc stdin))
    ;; Close stdin, so a command such as `git apply' that reads it finishes.
    (ignore-errors (process-send-eof proc))
    proc))

(defun xl-async-cancel (&optional slot buffer)
  "Kill BUFFER's run in SLOT without calling its ON-DONE.
With SLOT nil, kill every run BUFFER owns.  BUFFER defaults to the
current buffer.  Queued jobs are not touched; see `xl-async-clear-queue'."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (proc xl-async--processes)
      (when (or (null slot) (eq slot (process-get proc 'xl-async-slot)))
        (setq xl-async--processes (delq proc xl-async--processes))
        (xl-async--stop-timer proc)
        (when (process-live-p proc)
          (delete-process proc))))))

(defun xl-async-busy-p (&optional slot buffer)
  "Return non-nil while BUFFER has a run in SLOT, or any run if SLOT is nil."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (seq-some (lambda (proc)
                     (or (null slot)
                         (eq slot (process-get proc 'xl-async-slot))))
                   (buffer-local-value 'xl-async--processes buffer)))))

;; ============================================================
;; Queues
;; ============================================================

(defun xl-async-enqueue (slot job &optional on-drain)
  "Append JOB to the queue of SLOT in this buffer and start it if idle.
JOB is a plist:

  :attempts   commands tried in order; the next runs when one fails
  :directory  working directory, default `default-directory'
  :stdin      text for every attempt's standard input
  :on-output  streaming callback, see `xl-async-run'
  :timeout    seconds for every attempt, see `xl-async-run'
  :on-start   called with no arguments when the job begins
  :on-done    called as (ON-DONE OUTPUT EXIT-CODE) for the last attempt

ON-DRAIN, when given, replaces the queue's drain function, which is
called once the queue runs out of jobs."
  (let ((entry (or (assq slot xl-async--queues)
                   (car (push (list slot nil nil) xl-async--queues)))))
    (setf (nth 1 entry) (append (nth 1 entry) (list job)))
    (when on-drain (setf (nth 2 entry) on-drain))
    (xl-async--queue-next slot)))

(defun xl-async--queue-next (slot)
  "Start the next job of SLOT's queue, or call its drain function."
  (let ((entry (assq slot xl-async--queues)))
    (when (and entry (not (xl-async-busy-p slot)))
      (let ((job (pop (nth 1 entry))))
        (if (null job)
            (progn
              (setq xl-async--queues (assq-delete-all slot xl-async--queues))
              (when (nth 2 entry) (funcall (nth 2 entry))))
          (when (plist-get job :on-start)
            (funcall (plist-get job :on-start)))
          (xl-async--queue-attempt slot job (plist-get job :attempts)))))))

(defun xl-async--queue-attempt (slot job attempts)
  "Run the first of ATTEMPTS for JOB in SLOT, falling back to the rest."
  (xl-async-run
   :command (car attempts)
   :slot slot
   :directory (plist-get job :directory)
   :stdin (plist-get job :stdin)
   :on-output (plist-get job :on-output)
   :timeout (plist-get job :timeout)
   :on-done (lambda (output code)
              (if (and (not (zerop code)) (cdr attempts))
                  (xl-async--queue-attempt slot job (cdr attempts))
                (when (plist-get job :on-done)
                  (funcall (plist-get job :on-done) output code))
                (xl-async--queue-next slot)))))

(defun xl-async-queue-length (slot &optional buffer)
  "Return the number of jobs waiting behind the running one in SLOT."
  (with-current-buffer (or buffer (current-buffer))
    (length (nth 1 (assq slot xl-async--queues)))))

(defun xl-async-queue-busy-p (slot &optional buffer)
  "Return non-nil while SLOT's queue in BUFFER runs or has jobs waiting."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (assq slot xl-async--queues) t)))))

(defun xl-async-clear-queue (slot &optional buffer)
  "Drop SLOT's waiting jobs in BUFFER and kill the running one."
  (with-current-buffer (or buffer (current-buffer))
    (setq xl-async--queues (assq-delete-all slot xl-async--queues))
    (xl-async-cancel slot)))

(provide 'xl-async)
;;; xl-async.el ends here
