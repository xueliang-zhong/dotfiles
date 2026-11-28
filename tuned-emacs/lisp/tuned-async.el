;;; tuned-async.el --- Owned, cancellable subprocesses and job queues -*- lexical-binding: t; -*-

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
;;   (tuned-async-run
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
;; `tuned-async-enqueue' runs jobs of one slot strictly in order (a fast
;; `s s s' stages in order instead of racing), retries a job's next command
;; when one fails, and calls ON-DRAIN once the slot's queue is empty.  A
;; cons SLOT is a key shared by every buffer that passes it, so jobs of one
;; repository run in order whichever buffer started them; a job may also
;; name its own owner, or nil to outlive every buffer.
;;
;; Output is plain text: stdout and stderr together, as git writes them.
;; Parsing stays with the caller, who knows the format.

;;; Code:

;; This file holds the process layers used by the Git views, the Package
;; Menu, the Babel async blocks and the perfetto recorder: the
;; core primitives, the behaviors composed from them, and the compatibility
;; functions the existing callers use.  The loader lives in tuned-spinner.el;
;; a caller that shows one requires both modules, so the process layer does
;; not depend on the UI layer.

;;; Requirements

(require 'seq)

(defconst tuned-async-reap-interval 0.25
  "Seconds between checks that a finished process's sentinel has run.")

(defconst tuned-async-timeout-code 124
  "The exit code ON-DONE gets when a run was stopped by its :timeout.
It is the code of coreutils `timeout', so callers can treat both alike.")

(defvar tuned-async--queue-table nil
  "Alist of (KEY JOBS ON-DRAIN RUNNING) for the composable queues.
RUNNING is nil when idle, a process when that process can be aborted,
or t while a job that did not return a process runs.")

;;; ---- Layer 0: core primitives ----
;;
;; A task is a process.  `tuned-async-start' is the only place that calls
;; `make-process' and installs the filter and sentinel; every other
;; primitive only registers a handler on the process.  The end flow runs
;; once, guarded by the `tuned-async-finished' property.

(require 'cl-lib)

(defvar tuned-async-debug-on-error nil
  "When non-nil, signal handler errors instead of reporting them.
Tests bind this to see failures; the default keeps one bad handler from
hiding the others.")

(defun tuned-async-status (proc)
  "Return PROC's status: `running', `done', `timeout' or `cancelled'."
  (and (processp proc) (process-get proc 'tuned-async-status)))

(defun tuned-async-live-p (proc)
  "Return non-nil while PROC's task is still running."
  (eq (tuned-async-status proc) 'running))

(defun tuned-async-buffer (proc)
  "Return PROC's output buffer while it is alive, else nil."
  (let ((buffer (and (processp proc) (process-buffer proc))))
    (and (buffer-live-p buffer) buffer)))

(defun tuned-async-output (proc)
  "Return PROC's output so far, or all of it after the task ended."
  (if (tuned-async-live-p proc)
      (let ((buffer (tuned-async-buffer proc)))
        (if buffer (with-current-buffer buffer (buffer-string)) ""))
    (or (process-get proc 'tuned-async-output) "")))

(defun tuned-async-exit-code (proc)
  "Return PROC's exit code, 124 after a timeout, or nil while running."
  (pcase (tuned-async-status proc)
    ('running nil)
    ('timeout tuned-async-timeout-code)
    ('done (process-exit-status proc))
    ('cancelled (process-exit-status proc))
    (_ nil)))

(defun tuned-async--stop-timeout-timer (proc)
  "Cancel PROC's timeout timer, if it has one."
  (let ((timer (process-get proc 'tuned-async-timeout-timer)))
    (when (timerp timer) (cancel-timer timer))
    (process-put proc 'tuned-async-timeout-timer nil)))

(defun tuned-async--call-handler (proc kind fn &rest args)
  "Call FN with PROC and ARGS in PROC's owner buffer, guarding errors.
KIND names the handler list in the error message."
  (condition-case err
      (let ((owner (process-get proc 'tuned-async-owner)))
        (if (buffer-live-p owner)
            (with-current-buffer owner (apply fn proc args))
          (apply fn proc args)))
    (error
     (if tuned-async-debug-on-error
         (signal (car err) (cdr err))
       (message "tuned-async: %s handler of %s failed: %s"
                kind (process-name proc) (error-message-string err))))))

(defun tuned-async--output-filter (proc chunk)
  "Append CHUNK to PROC's buffer, then run its output handlers."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert chunk)))))
  (dolist (fn (process-get proc 'tuned-async-on-output))
    (tuned-async--call-handler proc 'on-output fn chunk)))

(defun tuned-async--finish (proc)
  "Run PROC's end flow, exactly once.
Read the pipe dry, settle the status, save the output, run the
on-exit and finally handlers, then dispose of the buffer."
  (unless (process-get proc 'tuned-async-finished)
    (process-put proc 'tuned-async-finished t)
    (tuned-async--stop-timeout-timer proc)
    ;; The process has exited, but data may still be in the pipe.
    (when (tuned-async-buffer proc)
      (while (accept-process-output proc 0 nil t)))
    (let ((status (process-get proc 'tuned-async-status)))
      (unless (memq status '(cancelled timeout))
        (process-put proc 'tuned-async-status 'done)))
    (process-put proc 'tuned-async-output
                 (let ((buffer (process-buffer proc)))
                   (if (buffer-live-p buffer)
                       (with-current-buffer buffer (buffer-string))
                     "")))
    ;; An owner that is gone means the task was abandoned.
    (let ((owner (process-get proc 'tuned-async-owner)))
      (when (and owner (not (buffer-live-p owner)))
        (process-put proc 'tuned-async-status 'cancelled)))
    (when (memq (tuned-async-status proc) '(done timeout))
      (dolist (fn (process-get proc 'tuned-async-on-exit))
        (tuned-async--call-handler proc 'on-exit fn)))
    (dolist (fn (process-get proc 'tuned-async-finally))
      (tuned-async--call-handler proc 'finally fn))
    (unless (process-get proc 'tuned-async-keep-buffer)
      (let ((buffer (process-buffer proc)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun tuned-async--exit-sentinel (proc _event)
  "Finish PROC after it has exited."
  (unless (process-live-p proc)
    (tuned-async--finish proc)))

(defun tuned-async--reap-task (proc)
  "Finish PROC if its sentinel never ran, reading the pipe first."
  (cond
   ((process-get proc 'tuned-async-finished) nil)
   ((process-live-p proc)
    (run-with-timer tuned-async-reap-interval nil #'tuned-async--reap-task proc))
   (t
    (while (and (not (process-get proc 'tuned-async-finished))
                (accept-process-output proc 0 nil t)))
    (tuned-async--finish proc))))

(defun tuned-async-start (command &rest args)
  "Start COMMAND as a new task and return its process.
ARGS is a plist:

  :directory    working directory, default `default-directory'
  :stdin        text written to standard input
  :keep-input   non-nil leaves standard input open after :stdin
  :name         process name, default \"tuned-async\"
  :buffer-name  output buffer name; %d becomes the pid, and the buffer
                is kept after the task ends.  Default is a hidden
                buffer that is deleted at the end
  :coding       process coding, default `utf-8-unix'

If :keep-input is nil, standard input is closed after :stdin, so a
command that reads it such as `git apply' can finish."
  (let* ((name (or (plist-get args :name) "tuned-async"))
         (named (plist-get args :buffer-name))
         (default-directory (or (plist-get args :directory)
                                default-directory))
         proc)
    (setq proc (make-process
                :name name
                :buffer nil
                :command command
                :connection-type 'pipe
                :noquery t
                :coding (or (plist-get args :coding) 'utf-8-unix)
                :filter #'tuned-async--output-filter
                :sentinel #'tuned-async--exit-sentinel))
    (set-process-buffer
     proc
     (generate-new-buffer
      (if named (format named (process-id proc)) " *tuned-async*")))
    (process-put proc 'tuned-async-status 'running)
    (process-put proc 'tuned-async-keep-buffer (and named t))
    (process-put proc 'tuned-async-on-output nil)
    (process-put proc 'tuned-async-on-exit nil)
    (process-put proc 'tuned-async-finally nil)
    (run-with-timer tuned-async-reap-interval nil #'tuned-async--reap-task proc)
    (when-let* ((stdin (plist-get args :stdin)))
      (process-send-string proc stdin))
    (unless (plist-get args :keep-input)
      (ignore-errors (process-send-eof proc)))
    proc))

(defun tuned-async--register (proc property fn)
  "Append FN to PROC's handler list PROPERTY and return PROC."
  (process-put proc property (append (process-get proc property) (list fn)))
  proc)

(defun tuned-async--late-on-exit (proc fn)
  "Run FN for PROC in the next event loop, as a late on-exit handler."
  (run-at-time 0 nil
               (lambda ()
                 (when (and (memq (tuned-async-status proc) '(done timeout))
                            (let ((owner (process-get proc 'tuned-async-owner)))
                              (or (null owner) (buffer-live-p owner))))
                   (tuned-async--call-handler proc 'on-exit fn)))))

(defun tuned-async--late-finally (proc fn)
  "Run FN for PROC in the next event loop, as a late finally handler."
  (run-at-time 0 nil
               (lambda () (tuned-async--call-handler proc 'finally fn))))

(defun tuned-async-on-output (proc fn)
  "Call (FN PROC CHUNK) for each chunk PROC writes.  Return PROC.
The chunk is already in PROC's buffer when FN runs."
  (if (process-get proc 'tuned-async-finished)
      proc
    (tuned-async--register proc 'tuned-async-on-output fn)))

(defun tuned-async-on-exit (proc fn)
  "Call (FN PROC) once when PROC ends normally.  Return PROC.
A cancelled task never calls FN.  Registering after PROC has finished
runs FN in the next event loop instead."
  (if (process-get proc 'tuned-async-finished)
      (progn (tuned-async--late-on-exit proc fn) proc)
    (tuned-async--register proc 'tuned-async-on-exit fn)))

(defun tuned-async-finally (proc fn)
  "Call (FN PROC) once however PROC ends.  Return PROC.
Registering after PROC has finished runs FN in the next event loop."
  (if (process-get proc 'tuned-async-finished)
      (progn (tuned-async--late-finally proc fn) proc)
    (tuned-async--register proc 'tuned-async-finally fn)))

(defun tuned-async-abort (proc)
  "Cancel PROC: set its status to `cancelled' and kill it.  Return PROC.
Its on-exit handlers do not run; its finally handlers do.  A finished
task is left alone."
  (when (tuned-async-live-p proc)
    (process-put proc 'tuned-async-status 'cancelled)
    (tuned-async--stop-timeout-timer proc)
    (delete-process proc))
  proc)

(defun tuned-async-send (proc string)
  "Write STRING to PROC's standard input and return PROC."
  (when (process-live-p proc)
    (ignore-errors (process-send-string proc string)))
  proc)

(defun tuned-async-close-input (proc)
  "Close PROC's standard input and return PROC."
  (when (process-live-p proc)
    (ignore-errors (process-send-eof proc)))
  proc)

;;; ---- Layer 1: behaviors ----

(defvar-local tuned-async--owned nil
  "Tasks owned by this buffer, newest first.")
(put 'tuned-async--owned 'permanent-local t)

(defvar-local tuned-async--dying nil
  "Non-nil while this buffer's kill hook runs.
A drain that runs while the buffer's own task is aborted must not start
new work in it.")
(put 'tuned-async--dying 'permanent-local t)

(defun tuned-async-buffer-dying-p (&optional buffer)
  "Return non-nil while BUFFER is being killed.
BUFFER defaults to the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'tuned-async--dying buffer))))

(defun tuned-async--abort-owned ()
  "Abort every task this buffer owns; it is going away."
  (mapc #'tuned-async-abort (copy-sequence tuned-async--owned)))
(put 'tuned-async--abort-owned 'permanent-local-hook t)

(defvar tuned-async--slots nil
  "Alist of ((OWNER . SLOT) . PROCESS) for slotted tasks.")

(defun tuned-async-timeout (proc seconds)
  "Give PROC SECONDS to finish, then cancel it with status `timeout'.
The exit code is 124 and the on-exit handlers still run.  Return PROC."
  (when (and (tuned-async-live-p proc)
             (not (process-get proc 'tuned-async-timeout-timer)))
    (process-put
     proc 'tuned-async-timeout-timer
     (run-with-timer
      seconds nil
      (lambda ()
        ;; A process that exited just before the timer ran is done, not
        ;; late: leave it for the sentinel or the reaper.
        (when (and (tuned-async-live-p proc) (process-live-p proc))
          (process-put proc 'tuned-async-status 'timeout)
          (delete-process proc))))))
  (tuned-async-finally proc #'tuned-async--stop-timeout-timer)
  proc)

(defun tuned-async-own (proc buffer)
  "Make BUFFER the owner of PROC and return PROC.
Handlers run with BUFFER current, and killing BUFFER aborts PROC.  The
ownership survives a major-mode change in BUFFER."
  (process-put proc 'tuned-async-owner buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (memq proc tuned-async--owned)
        (push proc tuned-async--owned))
      (add-hook 'kill-buffer-hook #'tuned-async--buffer-dying nil t)))
  (tuned-async-finally
   proc
   (lambda (p)
     (let ((owner (process-get p 'tuned-async-owner)))
       (when (buffer-live-p owner)
         (with-current-buffer owner
           (setq tuned-async--owned (delq p tuned-async--owned)))))))
  proc)

(defun tuned-async-slot (proc slot)
  "Put PROC in SLOT of its owner, aborting the older task there.
The key is (OWNER . SLOT); with no owner the slot is global.  Return
PROC."
  (let* ((owner (process-get proc 'tuned-async-owner))
         (key (cons owner slot))
         (old (cdr (assoc key tuned-async--slots))))
    (when (and old (not (eq old proc)))
      (tuned-async-abort old))
    (let ((entry (assoc key tuned-async--slots)))
      (if entry
          (setcdr entry proc)
        (push (cons key proc) tuned-async--slots)))
    (tuned-async-finally
     proc
     (lambda (p)
       (let ((entry (assoc key tuned-async--slots)))
         (when (eq (cdr entry) p)
           (setq tuned-async--slots (assoc-delete-all key tuned-async--slots)))))))
  proc)

(defun tuned-async-slot-process (slot &optional buffer)
  "Return the task in SLOT of BUFFER, defaulting to this buffer.
Falls back to a global slot when no task is found in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (or (cdr (assoc (cons buffer slot) tuned-async--slots))
        (cdr (assoc (cons nil slot) tuned-async--slots)))))

(defun tuned-async--drop-buffer-queues ()
  "Drop this buffer's jobs from every queue, and clear its own queues.
It is going away.  Waiting jobs of a keyed queue name their owner, so
they are removed before the buffer's running tasks are aborted: the
abort may advance the queue synchronously, and a job of a dying buffer
must not start."
  (let ((buffer (current-buffer)))
    (dolist (entry (copy-sequence tuned-async--queue-table))
      (if (and (consp (car entry))
               (eq (car (car entry)) buffer))
          (tuned-async-queue-clear (car entry))
        (setf (nth 1 entry)
              (seq-remove (lambda (cell) (eq (cdr cell) buffer))
                          (nth 1 entry)))))))
(put 'tuned-async--drop-buffer-queues 'permanent-local-hook t)

(defun tuned-async--buffer-dying ()
  "This buffer is going away: drop its queued jobs, then abort its tasks.
The order matters: aborting a task advances its queue in the same call
stack, and the next job must not be one of the dying buffer's."
  (setq tuned-async--dying t)
  (unwind-protect
      (progn
        (tuned-async--drop-buffer-queues)
        (tuned-async--abort-owned))
    (setq tuned-async--dying nil)))
(put 'tuned-async--buffer-dying 'permanent-local-hook t)

(defun tuned-async-queue-push (key job &optional on-drain owner)
  "Append JOB to KEY's queue and return KEY.
JOB is called with one argument, DONE, and calls (funcall DONE) when it
finishes; jobs of one KEY run strictly in order.  When the queue empties
ON-DRAIN is called once; a new ON-DRAIN replaces the old one.  OWNER, a
buffer, ties JOB to that buffer: killing it drops the job before it
starts.  Jobs with no OWNER are never dropped this way."
  (let ((entry (assoc key tuned-async--queue-table))
        (buffer (and (consp key) (car key))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook #'tuned-async--buffer-dying nil t)))
    (unless entry
      (setq entry (list key nil nil nil))
      (push entry tuned-async--queue-table))
    (setf (nth 1 entry) (append (nth 1 entry) (list (cons job owner))))
    (when on-drain (setf (nth 2 entry) on-drain))
    (tuned-async--queue-start key)
    key))

(defun tuned-async--queue-start (key)
  "Start KEY's next job, or drain the queue when it is empty."
  (let ((entry (assoc key tuned-async--queue-table)))
    (when (and entry (not (nth 3 entry)))
      (let ((cell (pop (nth 1 entry))))
        (if (null cell)
            (progn
              (setq tuned-async--queue-table
                    (assoc-delete-all key tuned-async--queue-table))
              (when (nth 2 entry) (funcall (nth 2 entry))))
          (let ((done-called nil))
            (setf (nth 3 entry) t)
            (let ((proc (funcall
                         (car cell)
                         (lambda ()
                           (unless done-called
                             (setq done-called t)
                             (setf (nth 3 entry) nil)
                             (tuned-async--queue-start key))))))
              ;; A job that starts a process should return it, so that
              ;; clearing the queue can abort the running job.
              (when (and (processp proc) (not done-called))
                (setf (nth 3 entry) proc)))))))))

(defun tuned-async-job (thunk)
  "Return a job that runs THUNK and finishes when its process does.
THUNK returns a process; the job returns that process too."
  (lambda (done)
    (let ((proc (funcall thunk)))
      (if (processp proc)
          (progn
            (tuned-async-finally proc (lambda (_p) (funcall done)))
            proc)
        (funcall done)))))

(defun tuned-async-attempts (thunks on-done)
  "Return a job trying THUNKS in order until one exits zero.
Each thunk returns a process.  When the last attempt ends, call
(ON-DONE PROC), then finish the job.  A cancelled task calls neither
ON-DONE nor the next thunk."
  (lambda (done)
    (let ((settled nil))
      (cl-labels
          ((attempt (rest)
             (let* ((last (null (cdr rest)))
                    (proc (funcall (car rest))))
               (if (not (processp proc))
                   (if (not last)
                       (attempt (cdr rest))
                     (unless settled
                       (setq settled t)
                       (unwind-protect
                           (when on-done (funcall on-done proc))
                         (funcall done))))
                 (tuned-async-on-exit
                  proc
                  (lambda (p)
                    (if (and (not last)
                             (/= 0 (or (tuned-async-exit-code p) 1)))
                        (attempt (cdr rest))
                      (unless settled
                        (setq settled t)
                        (unwind-protect
                            (when on-done (funcall on-done p))
                          (funcall done))))))
                 (tuned-async-finally
                  proc
                  (lambda (p)
                    ;; A normal end always reaches on-exit, so only a
                    ;; cancelled task needs the queue advanced here.
                    (when (and (not settled)
                               (eq (tuned-async-status p) 'cancelled))
                      (setq settled t)
                      (funcall done))))
                 proc))))
        (attempt thunks)))))

(defun tuned-async-queue-size (key)
  "Return the number of jobs waiting in KEY's queue."
  (length (nth 1 (assoc key tuned-async--queue-table))))

(defun tuned-async-queue-busy-p (key &optional buffer)
  "Return non-nil while KEY's queue runs or has jobs waiting.
KEY may be an explicit queue key, as `tuned-async-enqueue' accepts, or a
SLOT in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (or (and (assoc key tuned-async--queue-table) t)
        (and (assoc (cons buffer key) tuned-async--queue-table) t))))

(defun tuned-async-queue-clear (key)
  "Drop KEY's waiting jobs and abort the running one when known."
  (let ((entry (assoc key tuned-async--queue-table)))
    (when entry
      (setq tuned-async--queue-table
            (assoc-delete-all key tuned-async--queue-table))
      (let ((running (nth 3 entry)))
        (when (processp running) (tuned-async-abort running))))))

;;; ---- Compatibility layer ----
;;
;; The callers of this file keep their old interfaces; each function here
;; is a composition of layers 0 and 1.

(defun tuned-async-run (&rest args)
  "Start a process described by the plist ARGS and return it.

  :command    the program and its arguments (required)
  :directory  the process's working directory, default `default-directory'
  :owner      the buffer that owns it, default the current buffer;
              nil makes the run unowned: killing a buffer never stops
              it, ON-DONE always runs, and it cannot take a :slot
  :slot       a symbol; starting a run cancels OWNER's older run in it
  :stdin      text written to the process before its input is closed
  :name       the process name, default \"tuned-async\"
  :on-done    called as (ON-DONE OUTPUT EXIT-CODE) with OWNER current
  :on-output  called as (ON-OUTPUT CHUNK) with OWNER current each time the
              process writes; OUTPUT in ON-DONE still has all of it
  :timeout    seconds; the process is then killed and ON-DONE gets the
              exit code `tuned-async-timeout-code'

ON-DONE is skipped when OWNER was killed or the run was cancelled."
  (let* ((unowned (and (plist-member args :owner)
                       (null (plist-get args :owner))))
         (owner (unless unowned (or (plist-get args :owner) (current-buffer))))
         (slot (unless unowned (plist-get args :slot)))
         (on-output (plist-get args :on-output))
         (on-done (plist-get args :on-done))
         proc)
    (setq proc (tuned-async-start
                (plist-get args :command)
                :directory (plist-get args :directory)
                :stdin (plist-get args :stdin)
                :name (plist-get args :name)))
    (when owner (tuned-async-own proc owner))
    (when slot (tuned-async-slot proc slot))
    (when-let* ((seconds (plist-get args :timeout)))
      (tuned-async-timeout proc seconds))
    (when on-output
      (tuned-async-on-output proc (lambda (_p chunk) (funcall on-output chunk))))
    (when on-done
      (tuned-async-on-exit
       proc
       (lambda (p)
         (funcall on-done (tuned-async-output p) (tuned-async-exit-code p)))))
    proc))

(defun tuned-async-cancel (&optional slot buffer)
  "Kill BUFFER's run in SLOT without calling its ON-DONE.
With SLOT nil, kill every run BUFFER owns.  BUFFER defaults to the
current buffer.  Queued jobs are not touched; see `tuned-async-clear-queue'."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (if slot
          (let ((entry (assoc (cons buffer slot) tuned-async--slots)))
            (when entry (tuned-async-abort (cdr entry))))
        (mapc #'tuned-async-abort
              (copy-sequence
               (buffer-local-value 'tuned-async--owned buffer)))))))

(defun tuned-async-busy-p (&optional slot buffer)
  "Return non-nil while BUFFER has a run in SLOT, or any run if SLOT is nil."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (if slot
             (let ((entry (assoc (cons buffer slot) tuned-async--slots)))
               (and entry (tuned-async-live-p (cdr entry))))
           (seq-some #'tuned-async-live-p
                     (buffer-local-value 'tuned-async--owned buffer))))))

(defun tuned-async--queue-key (slot buffer)
  "Return SLOT's queue key in BUFFER.
A cons SLOT is a key of its own, shared by every buffer that passes it;
a symbol names the queue of SLOT in BUFFER, as before."
  (if (consp slot) slot (cons buffer slot)))

(defun tuned-async-enqueue (slot job &optional on-drain)
  "Append JOB to SLOT's queue and start it if the queue is idle.
SLOT is a symbol naming this buffer's queue, or a cons KEY shared by
every caller that passes the same key.

JOB is a plist:

  :attempts   commands tried in order; the next runs when one fails
  :directory  working directory, default `default-directory'
  :owner      buffer owning every attempt, default the current one;
              nil makes them unowned, so no killed buffer stops them
  :stdin      text for every attempt's standard input
  :on-output  streaming callback, see `tuned-async-run'
  :timeout    seconds for every attempt, see `tuned-async-run'
  :on-start   called with no arguments when the job begins
  :on-done    called as (ON-DONE OUTPUT EXIT-CODE) for the last attempt

ON-DRAIN, when given, replaces the queue's drain function, which is
called once the queue runs out of jobs.  A job whose owner buffer was
killed before it started is skipped: it calls no handler and no
command, and the queue moves on."
  (let* ((buffer (current-buffer))
         (explicit (consp slot))
         (slot-name (unless explicit slot))
         (key (tuned-async--queue-key slot buffer))
         (owner (if (plist-member job :owner)
                    (plist-get job :owner)
                  buffer))
         (directory (or (plist-get job :directory) default-directory))
         (attempt-args (list :directory directory
                             :stdin (plist-get job :stdin)
                             :on-output (plist-get job :on-output)
                             :timeout (plist-get job :timeout)))
         (thunks
          (mapcar
           (lambda (command)
             (lambda ()
               (apply #'tuned-async-run
                      :command command
                      :owner owner
                      :slot slot-name
                      attempt-args)))
           (plist-get job :attempts)))
         (on-start (plist-get job :on-start))
         (on-done (plist-get job :on-done))
         (attempts-job
          (tuned-async-attempts
           thunks
           (lambda (proc)
             (when on-done
               (funcall on-done (tuned-async-output proc)
                        (tuned-async-exit-code proc)))))))
    (tuned-async-queue-push
     key
     (lambda (done)
       ;; A keyed queue outlives its buffer, so a job may reach the
       ;; front after its owner died: it must not start at all.
       (if (or (null owner) (buffer-live-p owner))
           (progn
             (when on-start (funcall on-start))
             (funcall attempts-job done))
         (funcall done)))
     on-drain
     owner)))

(defun tuned-async-queue-length (slot &optional buffer)
  "Return the number of jobs waiting behind the running one in SLOT."
  (let* ((buffer (or buffer (current-buffer)))
         (entry (or (assoc (cons buffer slot) tuned-async--queue-table)
                    (assoc slot tuned-async--queue-table))))
    (length (nth 1 entry))))

(defun tuned-async-clear-queue (slot &optional buffer)
  "Drop SLOT's waiting jobs in BUFFER and kill the running one.
SLOT may also be a cons queue KEY, as `tuned-async-enqueue' accepts."
  (let ((buffer (or buffer (current-buffer))))
    (tuned-async-queue-clear (tuned-async--queue-key slot buffer))
    ;; A symbol slot may also have a run that the queue did not start.
    (unless (consp slot)
      (tuned-async-cancel slot buffer))))


(provide 'tuned-async)

;;; tuned-async.el ends here
