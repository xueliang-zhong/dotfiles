;;; xl-perfetto-trace.el --- Chrome JSON timelines of Emacs Lisp work -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))
;; Keywords: tools, profiling

;;; Commentary:
;; Record what Emacs does at the Lisp level for a while, then look at it
;; on a timeline in ui.perfetto.dev:
;;
;;   M-x xl-perfetto-trace-start
;;   ... use Emacs: type, switch buffers, run a command or two ...
;;   M-x xl-perfetto-trace-stop
;;
;; The result is a Chrome JSON trace written to `xl-perfetto-trace-directory'.
;; Each category has its own track: commands, timers, process filters and
;; sentinels, GC, redisplay and font-lock, functions you asked to trace, and
;; your own marks.  `xl-async' jobs and memory counters get their own tracks.
;;
;; Nothing is recorded outside a recording: hooks, advice, process function
;; wrappers and timers exist only between `xl-perfetto-trace-start' and
;; `xl-perfetto-trace-stop'.  No C primitive is advised, because native-comp
;; would compile a trampoline synchronously and freeze the UI; process
;; callbacks are wrapped with `add-function' instead, and processes created
;; during a recording are found by a periodic scan.
;;
;; Nothing about the content of your work is recorded: no key sequences,
;; buffer text, minibuffer input or process output.  Only names, durations
;; and sizes, plus buffer and file names unless
;; `xl-perfetto-trace-record-names' is nil.
;;
;; Trace your own code with:
;;
;;   (xl-perfetto-with-span "eln-opt rank" (eln-opt-rank--compute dir))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'timer)

(declare-function jit-lock-function "jit-lock" (start))
(declare-function xl-async-run "xl-async" (&rest args))
(declare-function xl-async-cancel "xl-async" (&optional slot buffer))

;;; ============================================================
;;; Options
;;; ============================================================

(defgroup xl-perfetto nil
  "Record Emacs Lisp activity as a Chrome JSON trace."
  :group 'tools
  :prefix "xl-perfetto-")

(defcustom xl-perfetto-trace-categories
  '(commands timers processes xl-async gc redisplay functions counters)
  "Categories of events a recording collects.
`marks' is not here: `xl-perfetto-trace-mark' always records, because you
asked for that mark explicitly."
  :type '(set (const commands) (const timers) (const processes)
              (const xl-async) (const gc) (const redisplay)
              (const functions) (const counters))
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-ring-size 100000
  "Number of events the ring buffer holds.
When it is full, the oldest event is overwritten and counted in the
trace metadata as a dropped event."
  :type 'natnum
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-max-seconds 60
  "Longest recording, in seconds.
When it is reached, the recording stops and its file is written as if
`xl-perfetto-trace-stop' had been called."
  :type 'number
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-directory
  (expand-file-name "traces/" user-emacs-directory)
  "Directory the trace files are written to.
It is created when a trace is written."
  :type 'directory
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-record-names t
  "Whether events may name buffers and files.
When nil, an event says only the major mode, never a buffer or file name,
so a trace can be shared without revealing what you were editing."
  :type 'boolean
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-process-scan-interval 0.5
  "Seconds between scans for new processes during a recording.
A process created while recording is wrapped at the next scan, so output
it writes before then is not recorded.  `xl-async' jobs are recorded
exactly from the start; see `xl-perfetto--async-around'."
  :type 'number
  :group 'xl-perfetto)

;;; ============================================================
;;; Recording state
;;; ============================================================

(defconst xl-perfetto--tracks
  '((1 . "commands")
    (2 . "timers")
    (3 . "processes")
    (4 . "gc")
    (5 . "redisplay")
    (6 . "functions")
    (7 . "marks"))
  "Track number and name, as written into the trace metadata.")

(defvar xl-perfetto--recording nil
  "Non-nil while a recording is running.")

(defvar xl-perfetto--t0 0
  "Start of the recording, in integer microseconds.")

(defvar xl-perfetto--started nil
  "Wall-clock time when the recording started, for the metadata.")

(defvar xl-perfetto--ring nil
  "Vector of event vectors, or nil when nothing is recorded.")

(defvar xl-perfetto--write-index 0
  "Slot in `xl-perfetto--ring' where the next event goes.")

(defvar xl-perfetto--capacity 0
  "Length of `xl-perfetto--ring', kept here to avoid a call per event.")

(defvar xl-perfetto--count 0
  "Total number of events written to the ring, including overwritten ones.")

(defvar xl-perfetto--meta-events nil
  "The track-naming metadata events, kept out of the ring.")

(defvar xl-perfetto--installed nil
  "Cleanup functions of the instrumentation, most recent first.")

(defvar xl-perfetto--inside nil
  "Reentrancy guard: non-nil while the recorder itself is writing an event.")

(defvar xl-perfetto--command-start nil
  "Start of the command being measured, in microseconds, or nil.")

(defvar xl-perfetto--command-name nil
  "Name of the command being measured, or nil.")

(defvar xl-perfetto--command-buffer nil
  "Buffer name of the command being measured, or nil.")

(defvar xl-perfetto--command-file nil
  "File name of the command being measured, or nil.")

(defvar xl-perfetto--command-mode nil
  "Major mode of the command being measured, or nil.")

(defvar xl-perfetto--gc-elapsed 0
  "Value of `gc-elapsed' when the recording started.")

(defvar xl-perfetto--in-gc nil
  "Guards the GC hook against a collection caused by its own work.")

(defvar xl-perfetto--in-counter nil
  "Guards counter sampling against a collection caused by its own work.")

(defvar xl-perfetto--last-redisplay-ms nil
  "Millisecond of the last redisplay mark, so one millisecond has one mark.")

(defvar xl-perfetto--wrapped-processes nil
  "Processes whose filter and sentinel this recording wrapped.")

(defvar xl-perfetto--process-scan-timer nil
  "Timer that finds new processes and samples counters, or nil.")

(defvar xl-perfetto--auto-stop-timer nil
  "Timer that stops the recording at its time limit, or nil.")

(defvar xl-perfetto--last-counter nil
  "Time of the last counter sample, or nil.")

(defvar xl-perfetto--async-open nil
  "Alist of open `xl-async' jobs: id to a plist with :name.")

(defvar xl-perfetto--async-counter 0
  "Number used to give each `xl-async' job a trace id.")

(defvar xl-perfetto--traced-functions nil
  "Functions the user asked to trace.
The list survives a recording, so a function traced once is traced by
every following recording until `xl-perfetto-untrace-function'.")

(defvar xl-perfetto--function-origins (make-hash-table :test #'eq)
  "Map from an advised function object to the symbol that owns it.")

(defvar xl-perfetto--mode-line nil
  "The mode-line recording indicator, or nil.")

(defvar xl-perfetto--old-global-mode-string nil
  "Value of `global-mode-string' before the indicator was added.")

(defvar xl-perfetto--last-file nil
  "File written by the most recent recording, or nil.")

(defvar xl-perfetto--last-count 0
  "Number of events in the most recent trace file.")

(defvar xl-perfetto--last-dropped 0
  "Number of dropped events in the most recent trace file.")

;;; ============================================================
;;; Time, events and the ring
;;; ============================================================

(defun xl-perfetto--now ()
  "Return the current time in integer microseconds since the epoch."
  (car (time-convert nil 1000000)))

(defun xl-perfetto--elapsed ()
  "Return microseconds since the recording started."
  (- (xl-perfetto--now) xl-perfetto--t0))

(defun xl-perfetto--record (ph name cat ts dur tid &optional args id)
  "Write one event into the ring.
PH is the trace-event phase, NAME and CAT its name and category, TS and
DUR its start and duration in microseconds, TID its track, ARGS a plist
of extra fields and ID the asynchronous id of a `b' or `e' event.
The recorder ignores events triggered by its own work, so recording one
event cannot recursively record another."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (let ((xl-perfetto--inside t)
          (index xl-perfetto--write-index))
      (aset xl-perfetto--ring index
            (vector ph name cat ts dur tid args id))
      (setq xl-perfetto--write-index
            (if (= (1+ index) xl-perfetto--capacity) 0 (1+ index)))
      (setq xl-perfetto--count (1+ xl-perfetto--count)))))

(defun xl-perfetto--events ()
  "Return the events in the ring, oldest first."
  (let* ((size (length xl-perfetto--ring))
         (n (min xl-perfetto--count size))
         (start (mod (- xl-perfetto--write-index n) size))
         events)
    (dotimes (index n)
      (push (aref xl-perfetto--ring (mod (+ start index) size)) events))
    (nreverse events)))

(defun xl-perfetto--dropped ()
  "Return the number of events the ring has overwritten."
  (max 0 (- xl-perfetto--count (length xl-perfetto--ring))))

;;; ============================================================
;;; JSON output
;;; ============================================================

(defun xl-perfetto--event-json (event)
  "Return EVENT, a vector from the ring, as a JSON-ready plist."
  (let ((ph (aref event 0))
        (name (aref event 1))
        (cat (aref event 2))
        (ts (aref event 3))
        (dur (aref event 4))
        (tid (aref event 5))
        (args (aref event 6))
        (id (aref event 7)))
    (append
     (list :name name :ph ph :ts ts :pid (emacs-pid))
     (when cat (list :cat cat))
     (when (equal ph "X") (list :dur (or dur 0)))
     (when tid (list :tid tid))
     (when id (list :id id))
     ;; Instant events default to thread scope in Perfetto when `s' is
     ;; omitted; writing it keeps the event format explicit.
     (when (equal ph "i") (list :s "t"))
     (when args (list :args args)))))

(defun xl-perfetto--json ()
  "Return the whole recording as a Chrome JSON trace string."
  (let ((events (append xl-perfetto--meta-events (xl-perfetto--events))))
    (json-serialize
     (list :displayTimeUnit "ms"
           :traceEvents (vconcat (mapcar #'xl-perfetto--event-json events))
           :metadata (list :emacs-version emacs-version
                           :system-type (symbol-name system-type)
                           :started xl-perfetto--started
                           :dropped-events (xl-perfetto--dropped))))))

(defun xl-perfetto--trace-directory ()
  "Return the trace directory, as a directory name."
  (file-name-as-directory (expand-file-name xl-perfetto-trace-directory)))

(defun xl-perfetto--unique-file (directory base)
  "Return an unused file name in DIRECTORY made from BASE."
  (let ((file (expand-file-name (concat base ".json") directory))
        (number 0))
    (while (file-exists-p file)
      (setq number (1+ number)
            file (expand-file-name (format "%s-%d.json" base number) directory)))
    file))

(defun xl-perfetto--write-file ()
  "Write the recording to a trace file and return its path.
The file is written under a temporary name and renamed into place, so a
recording never leaves a half-written trace behind."
  (let* ((directory (xl-perfetto--trace-directory))
         (_ (make-directory directory t))
         (base (format "emacs-trace-%s" (format-time-string "%Y%m%d-%H%M%S")))
         (file (xl-perfetto--unique-file directory base))
         (temp (make-temp-file (expand-file-name "xl-perfetto-" directory)
                               nil ".json"))
         (coding-system-for-write 'utf-8-unix)
         (text (xl-perfetto--json)))
    (unwind-protect
        (progn
          (with-temp-file temp (insert text))
          (rename-file temp file t)
          (setq xl-perfetto--last-file file
                xl-perfetto--last-count (+ (length xl-perfetto--meta-events)
                                           (min xl-perfetto--count
                                                xl-perfetto--capacity))
                xl-perfetto--last-dropped (xl-perfetto--dropped))
          file)
      (when (file-exists-p temp)
        (ignore-errors (delete-file temp))))))

;;; ============================================================
;;; Mode-line indicator
;;; ============================================================

(defun xl-perfetto--mode-line-on ()
  "Show the recording indicator in `global-mode-string'."
  (setq xl-perfetto--old-global-mode-string
        (default-value 'global-mode-string))
  (setq xl-perfetto--mode-line " ● REC 0s")
  (set-default 'global-mode-string
               (cons '(:eval xl-perfetto--mode-line)
                     (if (listp xl-perfetto--old-global-mode-string)
                         xl-perfetto--old-global-mode-string
                       (list xl-perfetto--old-global-mode-string))))
  (force-mode-line-update t))

(defun xl-perfetto--mode-line-off ()
  "Remove the recording indicator."
  (when (boundp 'global-mode-string)
    (set-default 'global-mode-string xl-perfetto--old-global-mode-string))
  (setq xl-perfetto--mode-line nil
        xl-perfetto--old-global-mode-string nil)
  (force-mode-line-update t))

(defun xl-perfetto--update-mode-line ()
  "Refresh the elapsed seconds of the recording indicator."
  (when xl-perfetto--recording
    (let ((text (format " ● REC %ds" (/ (xl-perfetto--elapsed) 1000000))))
      (unless (equal text xl-perfetto--mode-line)
        (setq xl-perfetto--mode-line text)
        (force-mode-line-update t)))))

;;; ============================================================
;;; Commands
;;; ============================================================

(defun xl-perfetto--pre-command ()
  "Remember when the current command started."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    ;; A second pre without a post means a recursive edit or an earlier
    ;; span that never ended; the old start is dropped rather than used.
    (setq xl-perfetto--command-start (xl-perfetto--elapsed)
          xl-perfetto--command-name
          (if (symbolp this-command)
              (symbol-name this-command)
            "(anonymous command)")
          xl-perfetto--command-buffer
          (and xl-perfetto-trace-record-names (buffer-name))
          xl-perfetto--command-file
          (and xl-perfetto-trace-record-names (buffer-file-name))
          xl-perfetto--command-mode (symbol-name major-mode))))

(defun xl-perfetto--post-command ()
  "Record the duration of the command that just ran."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             xl-perfetto--command-start)
    (let ((start xl-perfetto--command-start)
          (name xl-perfetto--command-name)
          (buffer xl-perfetto--command-buffer)
          (file xl-perfetto--command-file)
          (mode xl-perfetto--command-mode))
      (setq xl-perfetto--command-start nil)
      (xl-perfetto--record
       "X" name "commands" start (- (xl-perfetto--elapsed) start) 1
       (append (list :mode mode)
               (when buffer (list :buffer buffer))
               (when file (list :file file)))))))

;;; ============================================================
;;; Timers
;;; ============================================================

(defun xl-perfetto--own-function-p (function)
  "Return non-nil when FUNCTION belongs to this recorder."
  (and (symbolp function)
       (string-prefix-p "xl-perfetto-" (symbol-name function))))

(defun xl-perfetto--timer-advice (original timer &rest args)
  "Time ORIGINAL, called with TIMER and ARGS."
  (let ((function (timer--function timer)))
    (if (or (not xl-perfetto--recording)
            xl-perfetto--inside
            (xl-perfetto--own-function-p function))
        (apply original timer args)
      (let ((start (xl-perfetto--elapsed)))
        (unwind-protect
            (apply original timer args)
          (xl-perfetto--record
           "X"
           (if (symbolp function)
               (symbol-name function)
             "(anonymous timer)")
           "timers" start (- (xl-perfetto--elapsed) start) 2
           (list :idle (if (timer--idle-delay timer) t :false))))))))

;;; ============================================================
;;; Processes
;;; ============================================================

(defun xl-perfetto--process-filter-around (original process chunk)
  "Time ORIGINAL, a filter of PROCESS receiving CHUNK."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (funcall original process chunk)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (funcall original process chunk)
        (xl-perfetto--record
         "X" (format "filter %s" (process-name process)) "processes"
         start (- (xl-perfetto--elapsed) start) 3
         (list :bytes (string-bytes chunk)))))))

(defun xl-perfetto--process-sentinel-around (original process event)
  "Time ORIGINAL, a sentinel of PROCESS receiving EVENT."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (funcall original process event)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (funcall original process event)
        (xl-perfetto--record
         "X" (format "sentinel %s" (process-name process)) "processes"
         start (- (xl-perfetto--elapsed) start) 3
         (list :event event))))))

(defun xl-perfetto--wrap-process (process)
  "Wrap PROCESS's filter and sentinel, once per recording."
  (unless (memq process xl-perfetto--wrapped-processes)
    (push process xl-perfetto--wrapped-processes)
    (add-function :around (process-filter process)
                  #'xl-perfetto--process-filter-around)
    (add-function :around (process-sentinel process)
                  #'xl-perfetto--process-sentinel-around)))

(defun xl-perfetto--wrap-new-processes ()
  "Wrap every live process that this recording has not wrapped yet."
  (dolist (process (process-list))
    (xl-perfetto--wrap-process process)))

(defun xl-perfetto--unwrap-processes ()
  "Remove this recording's wrappers from every process it touched."
  (dolist (process xl-perfetto--wrapped-processes)
    (ignore-errors
      (remove-function (process-filter process)
                       #'xl-perfetto--process-filter-around))
    (ignore-errors
      (remove-function (process-sentinel process)
                       #'xl-perfetto--process-sentinel-around)))
  (setq xl-perfetto--wrapped-processes nil))

;;; ============================================================
;;; xl-async jobs
;;; ============================================================

(defun xl-perfetto--async-name (args)
  "Return a display name for the `xl-async-run' arguments ARGS."
  (let ((name (plist-get args :name))
        (command (plist-get args :command)))
    (or (and name (format "%s" name))
        (and (consp command) (car command)
             (file-name-nondirectory (car command)))
        "xl-async")))

(defun xl-perfetto--async-around (original &rest args)
  "Record the `xl-async-run' described by ARGS around ORIGINAL."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let* ((args (copy-sequence args))
           (id (format "xl-async-%d" (cl-incf xl-perfetto--async-counter)))
           (name (xl-perfetto--async-name args))
           (old-on-done (plist-get args :on-done)))
      (xl-perfetto--record "b" name "xl-async" (xl-perfetto--elapsed)
                           nil 3 nil id)
      (push (cons id (list :name name)) xl-perfetto--async-open)
      (plist-put args :on-done
                 (lambda (output code)
                   (xl-perfetto--async-finish id code (string-bytes output))
                   (when old-on-done
                     (funcall old-on-done output code))))
      (apply original args))))

(defun xl-perfetto--async-finish (id code bytes)
  "Close the asynchronous event ID with exit CODE and BYTES of output."
  (let ((entry (assoc id xl-perfetto--async-open)))
    (when entry
      (setq xl-perfetto--async-open
            (assq-delete-all id xl-perfetto--async-open))
      (when xl-perfetto--recording
        (xl-perfetto--record
         "e" (plist-get (cdr entry) :name) "xl-async"
         (xl-perfetto--elapsed) nil 3
         (list :exit code :bytes bytes) id)))))

(defun xl-perfetto--close-async-events ()
  "Close every `xl-async' job still open when the recording stops."
  (dolist (entry xl-perfetto--async-open)
    (xl-perfetto--record
     "e" (plist-get (cdr entry) :name) "xl-async"
     (xl-perfetto--elapsed) nil 3
     (list :cancelled-or-running t) (car entry)))
  (setq xl-perfetto--async-open nil))

;;; ============================================================
;;; GC, redisplay, counters
;;; ============================================================

(defun xl-perfetto--post-gc ()
  "Record the garbage collection that just finished."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             (not xl-perfetto--in-gc))
    (let ((xl-perfetto--in-gc t))
      (let* ((now (xl-perfetto--elapsed))
             (elapsed gc-elapsed)
             (duration (max 0 (round (* 1000000 (- elapsed
                                                    xl-perfetto--gc-elapsed))))))
        (setq xl-perfetto--gc-elapsed elapsed)
        (xl-perfetto--record "X" "gc" "gc" (max 0 (- now duration))
                             duration 4
                             (list :gcs-done gcs-done))
        (when (memq 'counters xl-perfetto-trace-categories)
          (xl-perfetto--record-counter))))))

(defun xl-perfetto--pre-redisplay (&rest _)
  "Mark the start of a redisplay, at most once per millisecond."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (let* ((now (xl-perfetto--elapsed))
           (millisecond (/ now 1000)))
      (unless (eq millisecond xl-perfetto--last-redisplay-ms)
        (setq xl-perfetto--last-redisplay-ms millisecond)
        (xl-perfetto--record "i" "redisplay" "redisplay"
                             now nil 5 nil)))))

(defun xl-perfetto--jit-lock-around (original start)
  "Time ORIGINAL, a `jit-lock-function' call starting at START."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (funcall original start)
    (let ((begin (xl-perfetto--elapsed))
          (buffer (and xl-perfetto-trace-record-names (buffer-name)))
          (file (and xl-perfetto-trace-record-names (buffer-file-name)))
          (mode (symbol-name major-mode))
          (chars (if (numberp start)
                     (max 0 (- (point-max) start))
                   0)))
      (unwind-protect
          (funcall original start)
        (xl-perfetto--record
         "X" "jit-lock" "redisplay" begin
         (- (xl-perfetto--elapsed) begin) 5
         (append (list :mode mode :chars chars)
                 (when buffer (list :buffer buffer))
                 (when file (list :file file))))))))

(defun xl-perfetto--record-counter ()
  "Record one memory and object counter sample."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             (not xl-perfetto--in-counter))
    (let ((xl-perfetto--in-counter t)
          (counts (memory-use-counts)))
      (xl-perfetto--record
       "C" "memory" "counters" (xl-perfetto--elapsed) nil nil
       (list :conses (or (nth 0 counts) 0)
             :floats (or (nth 1 counts) 0)
             :strings (or (nth 2 counts) 0)
             :vector-cells (or (nth 3 counts) 0)
             :symbols (or (nth 4 counts) 0)
             :gcs-done gcs-done
             :processes (length (process-list))
             :buffers (length (buffer-list)))))))

(defun xl-perfetto--scan ()
  "Periodic work of a recording: new processes, counters, mode line."
  (when xl-perfetto--recording
    (when (memq 'processes xl-perfetto-trace-categories)
      (xl-perfetto--wrap-new-processes))
    (when (memq 'counters xl-perfetto-trace-categories)
      (let ((now (float-time)))
        (when (or (null xl-perfetto--last-counter)
                  (>= (- now xl-perfetto--last-counter) 1))
          (setq xl-perfetto--last-counter now)
          (xl-perfetto--record-counter))))
    (xl-perfetto--update-mode-line)))

;;; ============================================================
;;; User-named functions
;;; ============================================================

(defun xl-perfetto--primitive-safe-p (function)
  "Return non-nil when FUNCTION may be advised without a trampoline.
A C primitive on a native-comp Emacs would have its trampoline compiled
synchronously by `advice-add', freezing the UI for about a second, so it
is refused unless native compilation is unavailable or eln-opt says its
trampoline is already cached."
  (let ((definition (and (fboundp function) (indirect-function function))))
    (or (not (subr-primitive-p definition))
        (not (and (fboundp 'native-comp-available-p)
                  (native-comp-available-p)))
        (and (fboundp 'eln-opt--trampoline-cached-p)
             (eln-opt--trampoline-cached-p function)))))

(defun xl-perfetto--check-traceable (function)
  "Signal a `user-error' when FUNCTION cannot be traced."
  (cond
   ((not (symbolp function))
    (user-error "Trace a function symbol, not %S" function))
   ((not (fboundp function))
    (user-error "No such function: %s" function))
   ((xl-perfetto--own-function-p function)
    (user-error "xl-perfetto does not trace itself: %s" function))
   ((not (xl-perfetto--primitive-safe-p function))
    (user-error "Refusing to advise the C primitive %s: native-comp would compile a trampoline synchronously"
                function))))

(defun xl-perfetto--install-function-advice (function)
  "Install the tracing advice on FUNCTION."
  (unless (advice-member-p #'xl-perfetto--function-advice function)
    (puthash (symbol-function function) (symbol-name function)
             xl-perfetto--function-origins)
    (advice-add function :around #'xl-perfetto--function-advice)))

(defun xl-perfetto--remove-function-advice (function)
  "Remove the tracing advice from FUNCTION."
  (when (advice-member-p #'xl-perfetto--function-advice function)
    (advice-remove function #'xl-perfetto--function-advice)))

(defun xl-perfetto--function-advice (original &rest args)
  "Record a span around ORIGINAL, called with ARGS.
The symbol whose function is ORIGINAL is looked up so the event can name
it; a non-local exit still ends the span and then propagates."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let* ((function (gethash original xl-perfetto--function-origins))
           (name (or function "function"))
           (start (xl-perfetto--elapsed))
           (buffer (and xl-perfetto-trace-record-names (buffer-name)))
           (file (and xl-perfetto-trace-record-names (buffer-file-name)))
           (mode (symbol-name major-mode)))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" name "functions" start (- (xl-perfetto--elapsed) start) 6
         (append (list :mode mode)
                 (when buffer (list :buffer buffer))
                 (when file (list :file file))))))))

(defmacro xl-perfetto-with-span (name &rest body)
  "Run BODY and, while recording, put it on the timeline as NAME.
Outside a recording this is just BODY plus one variable check.  A
non-local exit still ends the span."
  (declare (indent 1) (debug (form body)))
  `(if (not (and xl-perfetto--recording
                 (memq 'functions xl-perfetto-trace-categories)))
       (progn ,@body)
     (let ((span-name (format "%s" ,name))
           (span-start (xl-perfetto--elapsed))
           (span-buffer (and xl-perfetto-trace-record-names (buffer-name)))
           (span-file (and xl-perfetto-trace-record-names (buffer-file-name)))
           (span-mode (symbol-name major-mode)))
       (unwind-protect
           (progn ,@body)
         (xl-perfetto--record
          "X" span-name "functions" span-start
          (- (xl-perfetto--elapsed) span-start) 6
          (append (list :mode span-mode)
                  (when span-buffer (list :buffer span-buffer))
                  (when span-file (list :file span-file))))))))

;;; ============================================================
;;; Installation and teardown
;;; ============================================================

(defun xl-perfetto--install-commands ()
  "Install the command hooks, first and last."
  (add-hook 'pre-command-hook #'xl-perfetto--pre-command -100)
  (add-hook 'post-command-hook #'xl-perfetto--post-command 100)
  (push (lambda ()
          (remove-hook 'pre-command-hook #'xl-perfetto--pre-command)
          (remove-hook 'post-command-hook #'xl-perfetto--post-command))
        xl-perfetto--installed))

(defun xl-perfetto--install-timers ()
  "Install the advice on `timer-event-handler'."
  (advice-add 'timer-event-handler :around #'xl-perfetto--timer-advice)
  (push (lambda ()
          (advice-remove 'timer-event-handler #'xl-perfetto--timer-advice))
        xl-perfetto--installed))

(defun xl-perfetto--install-processes ()
  "Wrap every live process and remember to unwrap them."
  (xl-perfetto--wrap-new-processes)
  (push #'xl-perfetto--unwrap-processes xl-perfetto--installed))

(defun xl-perfetto--install-async ()
  "Advise `xl-async-run' when xl-async is loaded."
  (when (fboundp 'xl-async-run)
    (advice-add 'xl-async-run :around #'xl-perfetto--async-around)
    (push (lambda ()
            (advice-remove 'xl-async-run #'xl-perfetto--async-around))
          xl-perfetto--installed)))

(defun xl-perfetto--install-gc ()
  "Install the hook that records garbage collections."
  (setq xl-perfetto--gc-elapsed gc-elapsed)
  (add-hook 'post-gc-hook #'xl-perfetto--post-gc)
  (push (lambda () (remove-hook 'post-gc-hook #'xl-perfetto--post-gc))
        xl-perfetto--installed))

(defun xl-perfetto--install-redisplay ()
  "Install the redisplay mark and the font-lock advice."
  (add-hook 'pre-redisplay-functions #'xl-perfetto--pre-redisplay)
  (push (lambda ()
          (remove-hook 'pre-redisplay-functions #'xl-perfetto--pre-redisplay))
        xl-perfetto--installed)
  (require 'jit-lock)
  (when (fboundp 'jit-lock-function)
    (advice-add 'jit-lock-function :around #'xl-perfetto--jit-lock-around)
    (push (lambda ()
            (advice-remove 'jit-lock-function #'xl-perfetto--jit-lock-around))
          xl-perfetto--installed)))

(defun xl-perfetto--install-functions ()
  "Install tracing advice on every registered function."
  (dolist (function xl-perfetto--traced-functions)
    (when (and (fboundp function)
               (xl-perfetto--primitive-safe-p function))
      (xl-perfetto--install-function-advice function)
      (push (lambda ()
              (xl-perfetto--remove-function-advice function))
            xl-perfetto--installed))))

(defun xl-perfetto--install-scan ()
  "Start the timer that scans processes and samples counters."
  (let ((interval (max 0.01 (or xl-perfetto-trace-process-scan-interval 0.5))))
    (setq xl-perfetto--process-scan-timer
          (run-with-timer interval interval #'xl-perfetto--scan))
    (push (lambda ()
            (when (timerp xl-perfetto--process-scan-timer)
              (cancel-timer xl-perfetto--process-scan-timer))
            (setq xl-perfetto--process-scan-timer nil))
          xl-perfetto--installed)))

(defun xl-perfetto--install-auto-stop (seconds)
  "Stop the recording automatically after SECONDS."
  (when (and (numberp seconds) (> seconds 0))
    (setq xl-perfetto--auto-stop-timer
          (run-at-time seconds nil #'xl-perfetto--auto-stop))
    (push (lambda ()
            (when (timerp xl-perfetto--auto-stop-timer)
              (cancel-timer xl-perfetto--auto-stop-timer))
            (setq xl-perfetto--auto-stop-timer nil))
          xl-perfetto--installed)))

(defun xl-perfetto--install ()
  "Install the instrumentation the selected categories ask for."
  (let ((categories xl-perfetto-trace-categories))
    (when (memq 'commands categories) (xl-perfetto--install-commands))
    (when (memq 'timers categories) (xl-perfetto--install-timers))
    (when (memq 'processes categories) (xl-perfetto--install-processes))
    (when (memq 'xl-async categories) (xl-perfetto--install-async))
    (when (memq 'gc categories) (xl-perfetto--install-gc))
    (when (memq 'redisplay categories) (xl-perfetto--install-redisplay))
    (when (memq 'functions categories) (xl-perfetto--install-functions))
    (when (or (memq 'processes categories)
              (memq 'counters categories))
      (xl-perfetto--install-scan))))

(defun xl-perfetto--uninstall-all ()
  "Run every cleanup function, most recently installed first."
  (let ((installed xl-perfetto--installed))
    (setq xl-perfetto--installed nil)
    (dolist (cleanup installed)
      (ignore-errors (funcall cleanup)))))

(defun xl-perfetto--meta-events ()
  "Return the metadata events that name each track."
  (mapcar (lambda (track)
            (vector "M" "thread_name" nil 0 nil (car track)
                    (list :name (cdr track)) nil))
          xl-perfetto--tracks))

(defun xl-perfetto--reset-state ()
  "Forget a finished or failed recording, keeping the user settings."
  (setq xl-perfetto--ring nil
        xl-perfetto--capacity 0
        xl-perfetto--write-index 0
        xl-perfetto--count 0
        xl-perfetto--meta-events nil
        xl-perfetto--inside nil
        xl-perfetto--command-start nil
        xl-perfetto--command-name nil
        xl-perfetto--command-buffer nil
        xl-perfetto--command-file nil
        xl-perfetto--command-mode nil
        xl-perfetto--gc-elapsed 0
        xl-perfetto--last-redisplay-ms nil
        xl-perfetto--last-counter nil
        xl-perfetto--process-scan-timer nil
        xl-perfetto--auto-stop-timer nil))

(defun xl-perfetto--start (&optional max-seconds)
  "Start a recording, automatically stopping after MAX-SECONDS.
Any failure rolls back the instrumentation already installed and then
signals the error."
  (let ((size (max 1 (or xl-perfetto-trace-ring-size 100000)))
        (seconds (or max-seconds xl-perfetto-trace-max-seconds)))
    (setq xl-perfetto--ring (make-vector size nil)
          xl-perfetto--capacity size
          xl-perfetto--write-index 0
          xl-perfetto--count 0
          xl-perfetto--t0 (xl-perfetto--now)
          xl-perfetto--started (format-time-string "%Y-%m-%dT%H:%M:%S")
          xl-perfetto--installed nil
          xl-perfetto--inside nil
          xl-perfetto--wrapped-processes nil
          xl-perfetto--async-open nil
          xl-perfetto--last-counter nil
          xl-perfetto--last-redisplay-ms nil
          xl-perfetto--command-start nil
          xl-perfetto--meta-events (xl-perfetto--meta-events)
          xl-perfetto--recording t)
    (condition-case error
        (progn
          (xl-perfetto--install)
          (xl-perfetto--install-auto-stop seconds)
          (xl-perfetto--mode-line-on)
          t)
      ((error quit)
       (xl-perfetto--uninstall-all)
       (xl-perfetto--mode-line-off)
       (setq xl-perfetto--recording nil
             xl-perfetto--async-open nil)
       (xl-perfetto--reset-state)
       (signal (car error) (cdr error))))))

(defun xl-perfetto--stop ()
  "Stop the recording, write it out and return the file, or nil.
The instrumentation is removed even when writing fails."
  (when xl-perfetto--recording
    (let ((file nil))
      (unwind-protect
          (progn
            (xl-perfetto--uninstall-all)
            (xl-perfetto--close-async-events)
            (xl-perfetto--mode-line-off)
            (setq file (xl-perfetto--write-file))
            file)
        (setq xl-perfetto--recording nil)
        (xl-perfetto--reset-state)))))

(defun xl-perfetto--auto-stop ()
  "Stop the recording because its time limit was reached."
  (when xl-perfetto--recording
    (let ((file (xl-perfetto--stop)))
      (when file
        (message "xl-perfetto: recording time is up, wrote %s (%d events)"
                 file xl-perfetto--last-count)))))

;;; ============================================================
;;; Commands
;;; ============================================================

(defun xl-perfetto-trace-start (&optional max-seconds)
  "Start recording Emacs Lisp activity.
With a prefix argument, ask for the longest recording time in seconds
instead of using `xl-perfetto-trace-max-seconds'.  Starting while a
recording runs only says so; it does not restart."
  (interactive
   (list (when current-prefix-arg
           (read-number "Longest recording (seconds): "
                        xl-perfetto-trace-max-seconds))))
  (if xl-perfetto--recording
      (message "xl-perfetto: already recording")
    (let ((seconds (or max-seconds xl-perfetto-trace-max-seconds)))
      (xl-perfetto--start seconds)
      (message "xl-perfetto: recording, stop with %s"
               (substitute-command-keys
                "\\[xl-perfetto-trace-stop]")))))

(defun xl-perfetto-trace-stop ()
  "Stop recording, remove all instrumentation and write the trace file.
Return the file name.  Does nothing but report when no recording runs."
  (interactive)
  (if (not xl-perfetto--recording)
      (progn
        (message "xl-perfetto: not recording")
        nil)
    (let ((file (xl-perfetto--stop)))
      (when file
        (message "xl-perfetto: wrote %s (%d events, %d dropped)"
                 file xl-perfetto--last-count xl-perfetto--last-dropped))
      file)))

(defun xl-perfetto-trace-toggle ()
  "Start a recording, or stop the one that runs."
  (interactive)
  (if xl-perfetto--recording
      (xl-perfetto-trace-stop)
    (xl-perfetto-trace-start)))

(defun xl-perfetto-trace-function (function)
  "Trace FUNCTION during this and every later recording.
The function gets an `:around' advice while recording.  C primitives are
refused unless native compilation is unavailable or eln-opt has already
cached their trampoline."
  (interactive
   (list (intern
          (completing-read
           "Trace function: " obarray
           (lambda (symbol)
             (and (fboundp symbol)
                  (not (xl-perfetto--own-function-p symbol))))
           t nil nil (thing-at-point 'symbol)))))
  (xl-perfetto--check-traceable function)
  (add-to-list 'xl-perfetto--traced-functions function)
  (if (not xl-perfetto--recording)
      (message "xl-perfetto: will trace %s in the next recording" function)
    (if (not (memq 'functions xl-perfetto-trace-categories))
        (message "xl-perfetto: %s registered, but the functions category is off"
                 function)
      (xl-perfetto--install-function-advice function)
      (push (lambda () (xl-perfetto--remove-function-advice function))
            xl-perfetto--installed)
      (message "xl-perfetto: tracing %s" function))))

(defun xl-perfetto-untrace-function (function)
  "Stop tracing FUNCTION, now and in later recordings."
  (interactive
   (list (intern
          (completing-read "Untrace function: "
                           (or xl-perfetto--traced-functions obarray)
                           nil t nil nil (thing-at-point 'symbol)))))
  (setq xl-perfetto--traced-functions
        (delq function xl-perfetto--traced-functions))
  (xl-perfetto--remove-function-advice function)
  (message "xl-perfetto: no longer tracing %s" function))

(defun xl-perfetto-trace-mark (text)
  "Put a mark named TEXT on the timeline.
This is always recorded, whatever `xl-perfetto-trace-categories' says."
  (interactive (list (read-string "Mark: ")))
  (if (not xl-perfetto--recording)
      (message "xl-perfetto: not recording; the mark was not recorded")
    (xl-perfetto--record "i" text "marks" (xl-perfetto--elapsed) nil 7 nil)
    (message "xl-perfetto: mark %s" text)))

(defun xl-perfetto--reveal-file (file)
  "Show FILE in a Dired buffer, in Emacs, without an external program."
  (let ((directory (file-name-directory file)))
    (dired directory)
    (when (fboundp 'dired-goto-file)
      (ignore-errors (dired-goto-file file)))))

(defun xl-perfetto--latest-file ()
  "Return the most recent trace file, or nil."
  (or (and xl-perfetto--last-file (file-exists-p xl-perfetto--last-file)
           xl-perfetto--last-file)
      (let ((files (file-expand-wildcards
                    (expand-file-name "emacs-trace-*.json"
                                      (xl-perfetto--trace-directory)))))
        (car (sort files (lambda (first second)
                           (file-newer-than-file-p first second)))))))

(defun xl-perfetto-trace-open ()
  "Open ui.perfetto.dev and show the latest trace file in Emacs.
Drag the file from the Dired buffer into the page."
  (interactive)
  (browse-url "https://ui.perfetto.dev")
  (if-let* ((file (xl-perfetto--latest-file)))
      (progn
        (xl-perfetto--reveal-file file)
        (message "xl-perfetto: drag %s into ui.perfetto.dev" file))
    (message "xl-perfetto: no trace file yet; record one first")))

;;; ============================================================
;;; Benchmark
;;; ============================================================

(defun xl-perfetto--benchmark-target ()
  "Empty function used by `xl-perfetto-trace-benchmark'.")

(defun xl-perfetto--benchmark-run (iterations)
  "Call `xl-perfetto--benchmark-target' ITERATIONS times, return seconds."
  (let ((start (float-time)))
    (dotimes (_ iterations)
      (xl-perfetto--benchmark-target))
    (- (float-time) start)))

(defun xl-perfetto-trace-benchmark (&optional iterations)
  "Measure the per-call cost of tracing a function.
Call an empty function ITERATIONS times (default 100000) with and without
the tracing advice and report the difference in microseconds per call.
It only reports a number; no test asserts a speed, because slow machines
would make such a test flaky."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           100000)))
  (let* ((iterations (or iterations 100000))
         (baseline (xl-perfetto--benchmark-run iterations))
         (started nil)
         (was-traced (advice-member-p #'xl-perfetto--function-advice
                                      'xl-perfetto--benchmark-target))
         traced)
    (unless xl-perfetto--recording
      (let ((xl-perfetto-trace-categories '(functions))
            (xl-perfetto-trace-ring-size 1000)
            (xl-perfetto-trace-max-seconds 3600))
        (xl-perfetto--start 3600)
        (setq started t)))
    (unless was-traced
      (xl-perfetto--install-function-advice 'xl-perfetto--benchmark-target))
    (unwind-protect
        (setq traced (xl-perfetto--benchmark-run iterations))
      (unless was-traced
        (xl-perfetto--remove-function-advice 'xl-perfetto--benchmark-target))
      (when started
        (let ((file (xl-perfetto--stop)))
          (when (and file (file-exists-p file))
            (ignore-errors (delete-file file))))))
    (let ((microseconds (* 1000000.0 (/ (- traced baseline) iterations))))
      (message "xl-perfetto: %.2f µs extra per traced call (%d calls)"
               microseconds iterations)
      microseconds)))

(provide 'xl-perfetto-trace)
;;; xl-perfetto-trace.el ends here
