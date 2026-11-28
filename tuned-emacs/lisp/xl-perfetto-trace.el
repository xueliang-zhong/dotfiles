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
;; sentinels, GC, redisplay frames and font-lock, functions you asked to trace,
;; and your own marks.  `xl-async' jobs get their own async tracks, and
;; redisplay frames get a counter track.  A redisplay frame has its duration,
;; the windows it repainted, the Lisp phases inside it and whether it missed
;; its budget (jank, shown as a `^' event), in the style of a SurfaceFlinger
;; trace.  Frames closer than `xl-perfetto-trace-frame-min-interval' are
;; coalesced, so a program that calls `redisplay' in a loop cannot flood the
;; ring buffer.
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
;;   (xl-perfetto-with-span "scan" (scan-dir dir))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'timer)

(declare-function jit-lock-function "jit-lock" (start))
(declare-function xl-async-start "xl-async" (command &rest args))
(declare-function xl-async-status "xl-async" (proc))
(declare-function xl-async-output "xl-async" (proc))
(declare-function xl-async-exit-code "xl-async" (proc))
(declare-function xl-async-finally "xl-async" (proc fn))
(declare-function xl-async-cancel "xl-async" (&optional slot buffer))

;;; ============================================================
;;; Options
;;; ============================================================

(defgroup xl-perfetto nil
  "Record Emacs Lisp activity as a Chrome JSON trace."
  :group 'tools
  :prefix "xl-perfetto-")

(defcustom xl-perfetto-trace-categories
  '(commands keys timers processes xl-async gc redisplay functions modes
    completion input files counters)
  "Categories of events a recording collects.
`marks' is not here: `xl-perfetto-trace-mark' always records, because you
asked for that mark explicitly."
  :type '(set (const commands) (const timers) (const processes)
              (const xl-async) (const gc) (const redisplay)
              (const functions) (const modes) (const completion)
              (const input) (const files)
              (const counters))
  :group 'xl-perfetto)

(defcustom xl-perfetto-marker-file
  (expand-file-name "traces/recording.eld" user-emacs-directory)
  "File that says a recording runs, for other Emacs processes to follow.
Written when a recording starts and deleted when it stops.  A follower process
reads it between units of work and records alongside (see `xl-perfetto-follow-marker')."
  :type 'file
  :group 'xl-perfetto)

(defvar xl-perfetto--file-prefix "emacs-trace"
  "Start of the trace file names this process writes.")

(defvar xl-perfetto--follower nil
  "Non-nil in a process that records only because another Emacs records.")

(defcustom xl-perfetto-trace-ring-size 100000
  "Number of events the ring buffer holds.
When it is full, the oldest event is overwritten and counted in the
trace metadata as a dropped event."
  :type 'natnum
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-max-seconds 120
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

(defcustom xl-perfetto-trace-frame-budget 0.0167
  "Budget of one redisplay frame, in seconds.
A redisplay that takes longer is marked with `jank' in the trace and
counted as a janky frame, the way a missed vsync is marked in a
SurfaceFlinger trace.  The default is one frame at 60 Hz."
  :type 'number
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-redisplay-detail t
  "Whether a recording details each recorded redisplay frame.
When non-nil, every redisplayed window gets a marker and the Lisp phases
inside redisplay (window state, size and buffer changes, scrolling) get
their own spans.  When nil, a recording keeps only one span per redisplay
frame, which is much cheaper but hides which window and phase ran."
  :type 'boolean
  :group 'xl-perfetto)

(defcustom xl-perfetto-trace-frame-min-interval 0.008
  "Shortest gap between two recorded redisplay frames, in seconds.
Frames closer together than this are coalesced into the next recorded
frame, because a program that calls `redisplay' in a loop can make
thousands of frames per second and fill the ring buffer.  A janky frame
is always recorded.  Set this to 0 to record every redisplay.  The
default 0.008 is 125 frames per second, enough for a 120 Hz display."
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
    (7 . "marks")
    (8 . "modes")
    (9 . "completion")
    (10 . "files")
    (11 . "input"))
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

(defvar xl-perfetto--wrapped-processes nil
  "Processes whose filter and sentinel this recording wrapped.")

(defvar xl-perfetto--process-scan-timer nil
  "Timer that finds new processes and updates the mode line, or nil.")

(defvar xl-perfetto--auto-stop-timer nil
  "Timer that stops the recording at its time limit, or nil.")

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

(cl-defstruct (xl-perfetto--frame (:constructor xl-perfetto--make-frame)
                                  (:copier nil))
  "The redisplay frame currently being measured.
START, INTERVAL, KEY-LATENCY, COMMAND-LATENCY and AFTER-COMMAND are in
microseconds.  WINDOWS counts distinct windows; SEEN and PRE-SEEN are the
windows already counted and waiting for a marker.  SPANS holds the
deferred font-lock and phase spans, so a coalesced frame costs no trace
events.  The phase flags say which Lisp redisplay hooks ran in this frame."
  start interval key-latency command-latency after-command
  windows seen pre-seen spans fontified
  scroll resize state buffer-change
  selected-buffer selected-mode)

(defvar xl-perfetto--frame nil
  "The open redisplay frame, or nil when none is open.")

(defvar xl-perfetto--frame-timer nil
  "Timer that closes the open redisplay frame, or nil.")

(defvar xl-perfetto--last-frame-start nil
  "Start of the previous redisplay frame, in microseconds, or nil.")

(defvar xl-perfetto--last-key-time nil
  "Time of the last recorded special key, in microseconds, or nil.")

(defvar xl-perfetto--last-command-start nil
  "Start of the most recent command, in microseconds, or nil.")

(defvar xl-perfetto--last-command-end nil
  "End of the most recent command, in microseconds, or nil.")

(defvar xl-perfetto--redisplay-phase nil
  "The open Lisp redisplay phase as (NAME START WINDOW), or nil.")

(defvar xl-perfetto--frame-count 0
  "Number of redisplay frames in the current recording.")

(defvar xl-perfetto--jank-count 0
  "Number of redisplay frames over `xl-perfetto-trace-frame-budget'.")

(defvar xl-perfetto--frame-max 0
  "Duration of the longest redisplay frame, in microseconds.")

(defvar xl-perfetto--last-recorded-frame-start nil
  "Start of the last redisplay frame written to the trace, or nil.")

(defvar xl-perfetto--recorded-frame-count 0
  "Number of redisplay frames written to the current trace.")

(defvar xl-perfetto--coalesced-pending 0
  "Fast frames skipped since the last recorded frame.")

(defvar xl-perfetto--coalesced-count 0
  "Number of redisplay frames coalesced away in the current recording.")

;;; ============================================================
;;; Time, events and the ring
;;; ============================================================

(defun xl-perfetto--now ()
  "Return the current time in integer microseconds since the epoch."
  (car (time-convert nil 1000000)))

(defun xl-perfetto--elapsed ()
  "Return microseconds since the recording started."
  (- (xl-perfetto--now) xl-perfetto--t0))

(defun xl-perfetto--ms (microseconds)
  "MICROSECONDS as milliseconds, rounded to two decimals."
  (if microseconds
      (/ (round (* microseconds 100.0)) 100000.0)
    0))

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
                           :dropped-events (xl-perfetto--dropped)
                           :redisplay-frames xl-perfetto--frame-count
                           :redisplay-recorded-frames
                           xl-perfetto--recorded-frame-count
                           :redisplay-coalesced-frames
                           xl-perfetto--coalesced-count
                           :redisplay-jank-frames xl-perfetto--jank-count
                           :redisplay-max-frame-ms
                           (xl-perfetto--ms xl-perfetto--frame-max))))))

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
         (base (format "%s-%s" xl-perfetto--file-prefix
                       (format-time-string "%Y%m%d-%H%M%S")))
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

(defcustom xl-perfetto-trace-keys '(return)
  "Special keys recorded as instants when they start a command.
Any of `return', `tab', `escape'.  Printable keys are never recorded, so
the trace holds no typed text."
  :type '(set (const return) (const tab) (const escape))
  :group 'xl-perfetto)

(defun xl-perfetto--key-label (event)
  "The label of EVENT when it is one of `xl-perfetto-trace-keys', else nil."
  (let ((key (cond ((memq event '(return 13)) 'return)
                   ((memq event '(tab 9)) 'tab)
                   ((memq event '(escape 27)) 'escape))))
    (and key (memq key xl-perfetto-trace-keys)
         (pcase key ('return "RET") ('tab "TAB") ('escape "ESC")))))

(defun xl-perfetto--pre-command ()
  "Remember when the current command started, and when a special key started it."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (let ((label (and (memq 'keys xl-perfetto-trace-categories)
                      (xl-perfetto--key-label last-command-event))))
      (when label
        (let ((now (xl-perfetto--elapsed)))
          (setq xl-perfetto--last-key-time now)
          (xl-perfetto--record "i" (format "key %s" label) "keys"
                               now nil 1 nil))))
    ;; A second pre without a post means a recursive edit or an earlier
    ;; span that never ended; the old start is dropped rather than used.
    (setq xl-perfetto--command-start (xl-perfetto--elapsed)
          xl-perfetto--last-command-start xl-perfetto--command-start
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
      (setq xl-perfetto--last-command-end (xl-perfetto--elapsed)
            xl-perfetto--command-start nil)
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

;; Advised on `xl-async-start', the one place every job starts, so jobs made
;; directly (package installs, native compiles) are recorded too.  The end is
;; recorded by a finally handler on the process, which runs however the job ends.

(defun xl-perfetto--async-name (command args)
  "Return a display name for a job started with COMMAND and ARGS."
  (let ((name (plist-get args :name)))
    (or (and name (format "%s" name))
        (and (consp command) (car command)
             (file-name-nondirectory (car command)))
        "xl-async")))

(defun xl-perfetto--async-around (original command &rest args)
  "Record the job that `xl-async-start' starts for COMMAND and ARGS.
ORIGINAL is `xl-async-start'.  Returns the process it returns."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original command args)
    (let* ((id (format "xl-async-%d" (cl-incf xl-perfetto--async-counter)))
           (name (xl-perfetto--async-name command args))
           (process (apply original command args)))
      (when (processp process)
        (xl-perfetto--record "b" name "xl-async" (xl-perfetto--elapsed)
                             nil 3 nil id)
        (push (cons id (list :name name)) xl-perfetto--async-open)
        (xl-async-finally process
                          (lambda (proc)
                            (xl-perfetto--async-finish id proc))))
      process)))

(defun xl-perfetto--async-finish (id process)
  "Close the asynchronous event ID for PROCESS, which has ended.
Records its status, exit code and output size."
  (let ((entry (assoc id xl-perfetto--async-open)))
    (when entry
      (setq xl-perfetto--async-open
            (assq-delete-all id xl-perfetto--async-open))
      (when xl-perfetto--recording
        (xl-perfetto--record
         "e" (plist-get (cdr entry) :name) "xl-async"
         (xl-perfetto--elapsed) nil 3
         (list :status (format "%s" (xl-async-status process))
               :exit (xl-async-exit-code process)
               :bytes (string-bytes (or (xl-async-output process) "")))
         id)))))

(defun xl-perfetto--close-async-events ()
  "Close every `xl-async' job still open when the recording stops."
  (dolist (entry xl-perfetto--async-open)
    (xl-perfetto--record
     "e" (plist-get (cdr entry) :name) "xl-async"
     (xl-perfetto--elapsed) nil 3
     (list :cancelled-or-running t) (car entry)))
  (setq xl-perfetto--async-open nil))

;;; ============================================================
;;; Major modes
;;; ============================================================

;; The hooks run for every major-mode change, whether a file was opened, the
;; mode was called directly (`org-mode'), or a mode was switched in a buffer.
;; Advising `set-auto-mode' or `normal-mode' would miss the direct calls.

(defvar xl-perfetto--mode-change nil
  "(START BUFFER OLD-MODE) of the major-mode change in progress, or nil.")

(defun xl-perfetto--mode-change-start ()
  "Remember when a major-mode change begins, in microseconds."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (setq xl-perfetto--mode-change
          (list (xl-perfetto--elapsed)
                (and xl-perfetto-trace-record-names (buffer-name))
                (symbol-name major-mode)))))

(defun xl-perfetto--minibuffer-mode-p ()
  "Non-nil in a minibuffer, whose mode switches are not worth a span."
  (or (minibufferp)
      (memq major-mode '(minibuffer-mode minibuffer-inactive-mode))))

(defun xl-perfetto--mode-change-end ()
  "Record the major-mode change that has just finished, as a span.
Changes into or out of the minibuffer are skipped: every minibuffer use
switches modes, and that would bury the mode changes that matter."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             xl-perfetto--mode-change
             (not (xl-perfetto--minibuffer-mode-p)))
    (let ((change xl-perfetto--mode-change))
      (setq xl-perfetto--mode-change nil)
      (xl-perfetto--record
       "X" (format "mode %s" major-mode) "modes"
       (car change) (- (xl-perfetto--elapsed) (car change)) 8
       (append (list :from (nth 2 change) :to (symbol-name major-mode))
               (when (nth 1 change) (list :buffer (nth 1 change))))))))

(defun xl-perfetto--install-modes ()
  "Install the hooks that time major-mode changes."
  (add-hook 'change-major-mode-hook #'xl-perfetto--mode-change-start)
  (add-hook 'after-change-major-mode-hook #'xl-perfetto--mode-change-end)
  (push (lambda ()
          (remove-hook 'change-major-mode-hook #'xl-perfetto--mode-change-start)
          (remove-hook 'after-change-major-mode-hook #'xl-perfetto--mode-change-end)
          (setq xl-perfetto--mode-change nil))
        xl-perfetto--installed))

;;; ============================================================
;;; Completion
;;; ============================================================

;; `completion-at-point' runs every function in `completion-at-point-functions':
;; corfu, cape, ispell and the rest.  Timing that one call covers all of them.
;; Only the duration and the number of candidates are recorded, never the text.

(defun xl-perfetto--completion-around (original &rest args)
  "Record the duration of `completion-at-point' around ORIGINAL, called with ARGS."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let* ((start (xl-perfetto--elapsed))
           (buffer (and xl-perfetto-trace-record-names (buffer-name)))
           (mode (symbol-name major-mode))
           (result nil))
      (unwind-protect
          (setq result (apply original args))
        (xl-perfetto--record
         "X" "completion-at-point" "completion" start
         (- (xl-perfetto--elapsed) start) 9
         (append (list :mode mode
                       :candidates (if (and (consp result) (integerp (nth 2 result)))
                                       (- (nth 2 result) (nth 1 result))
                                     0))
                 (when buffer (list :buffer buffer))))))))

(defun xl-perfetto--install-completion ()
  "Advise `completion-at-point' with the timing."
  (advice-add 'completion-at-point :around #'xl-perfetto--completion-around)
  (push (lambda ()
          (advice-remove 'completion-at-point #'xl-perfetto--completion-around))
        xl-perfetto--installed))

;; Minibuffer completion (vertico, and consult lists shown through it) does not go
;; through `completion-at-point', so it is timed on `vertico--update', the function
;; that recomputes and shows the candidates on each change of input.

(defun xl-perfetto--vertico-around (original &rest args)
  "Record `vertico--update' around ORIGINAL, called with ARGS, with its candidates."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed))
          (candidates 0))
      (unwind-protect
          (apply original args)
        (setq candidates (if (boundp 'vertico--total) vertico--total 0))
        (xl-perfetto--record
         "X" "vertico--update" "completion" start
         (- (xl-perfetto--elapsed) start) 9
         (list :candidates candidates))))))

(defun xl-perfetto--corfu-around (original &rest args)
  "Record `corfu--update' around ORIGINAL, called with ARGS: the candidates
of the in-buffer popup are computed here on every keystroke."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" "corfu--update" "completion" start
         (- (xl-perfetto--elapsed) start) 9
         (append (list :candidates (if (boundp 'corfu--total) corfu--total 0))
                 (when xl-perfetto-trace-record-names
                   (list :buffer (buffer-name)))))))))

(defun xl-perfetto--install-corfu ()
  "Advise `corfu--update' when corfu is loaded."
  (when (fboundp 'corfu--update)
    (advice-add 'corfu--update :around #'xl-perfetto--corfu-around)
    (push (lambda ()
            (advice-remove 'corfu--update #'xl-perfetto--corfu-around))
          xl-perfetto--installed)))

(defun xl-perfetto--install-vertico ()
  "Advise `vertico--update' when vertico is loaded."
  (when (fboundp 'vertico--update)
    (advice-add 'vertico--update :around #'xl-perfetto--vertico-around)
    (push (lambda ()
            (advice-remove 'vertico--update #'xl-perfetto--vertico-around))
          xl-perfetto--installed)))

;;; ============================================================
;;; Chinese input (pyim)
;;; ============================================================

;; pyim runs in Lisp.  A composition enters `pyim-input-method', which reads
;; further keys until the text is committed; each key rebuilds the candidate
;; list (`pyim-process-run') and redraws the popup (`pyim-page--refresh'), and
;; the commit inserts the text (`pyim-select-word').  These spans fill the gap
;; the command track cannot see: the keys are read inside the input method, so
;; no command runs until the text is inserted.
;; Only durations and candidate counts are recorded, never the pinyin or text.

(defun xl-perfetto--pyim-input-around (original &rest args)
  "Record a whole pyim composition around ORIGINAL, called with ARGS."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" "pyim-input-method" "input" start
         (- (xl-perfetto--elapsed) start) 11
         (append (list :mode (symbol-name major-mode))
                 (when xl-perfetto-trace-record-names
                   (list :buffer (buffer-name)))))))))

(defun xl-perfetto--pyim-run-around (original &rest args)
  "Record `pyim-process-run' around ORIGINAL, called with ARGS.
The candidate count is the size of the list pyim just built."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed))
          (candidates 0))
      (unwind-protect
          (apply original args)
        (when (boundp 'pyim-process--candidates)
          (setq candidates (length pyim-process--candidates)))
        (xl-perfetto--record
         "X" "pyim-process-run" "input" start
         (- (xl-perfetto--elapsed) start) 11
         (list :candidates candidates))))))

(defun xl-perfetto--pyim-refresh-around (original &rest args)
  "Record `pyim-page--refresh' around ORIGINAL, called with ARGS: the popup."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" "pyim-page--refresh" "input" start
         (- (xl-perfetto--elapsed) start) 11)))))

(defun xl-perfetto--pyim-select-around (original &rest args)
  "Record `pyim-select-word' around ORIGINAL, called with ARGS: the commit.
Its duration covers inserting the text and the `after-change' hooks that
follow, which is what makes a commit feel slow."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" "pyim-select-word" "input" start
         (- (xl-perfetto--elapsed) start) 11)))))

(defun xl-perfetto--install-input ()
  "Advise pyim's entry points.  Loads pyim when it is installed."
  (when (require 'pyim nil t)
    (dolist (pair '((pyim-input-method . xl-perfetto--pyim-input-around)
                    (pyim-process-run . xl-perfetto--pyim-run-around)
                    (pyim-page--refresh . xl-perfetto--pyim-refresh-around)
                    (pyim-select-word . xl-perfetto--pyim-select-around)))
      (let ((function (car pair))
            (advice (cdr pair)))
        (when (fboundp function)
          (advice-add function :around advice)
          (push (lambda () (advice-remove function advice))
                xl-perfetto--installed))))))

;;; ============================================================
;;; Files: opening and saving
;;; ============================================================

;; `find-file-noselect' and `save-buffer' are Lisp functions, so they are advised.
;; The save hooks are timed separately, because format-on-save and similar
;; hooks are often the slow part of a save.  File names are recorded only when
;; `xl-perfetto-trace-record-names' allows it.

(defvar xl-perfetto--save-hook-start nil
  "Microsecond time when the current save hook started, or nil.")

(defun xl-perfetto--timed-around (name original &rest args)
  "Record a span named NAME around ORIGINAL, called with ARGS, on the files track."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let* ((start (xl-perfetto--elapsed))
           (buffer (and xl-perfetto-trace-record-names (buffer-name)))
           (mode (symbol-name major-mode))
           (file (and xl-perfetto-trace-record-names
                      (or (and (stringp (car args)) (car args))
                          (buffer-file-name)))))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" name "files" start (- (xl-perfetto--elapsed) start) 10
         (append (list :mode mode)
                 (when buffer (list :buffer buffer))
                 (when file (list :file file))))))))

(defun xl-perfetto--find-file-around (original &rest args)
  "Record `find-file-noselect' around ORIGINAL, called with ARGS."
  (apply #'xl-perfetto--timed-around "find-file" original args))

(defun xl-perfetto--save-buffer-around (original &rest args)
  "Record `save-buffer' around ORIGINAL, called with ARGS."
  (apply #'xl-perfetto--timed-around "save-buffer" original args))

(defun xl-perfetto--save-hook-begin ()
  "Start the timing of a save hook."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (setq xl-perfetto--save-hook-start (xl-perfetto--elapsed))))

(defun xl-perfetto--save-hook-end (name)
  "End the timing of the save hook NAME, started by `xl-perfetto--save-hook-begin'."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             xl-perfetto--save-hook-start)
    (let ((start xl-perfetto--save-hook-start))
      (setq xl-perfetto--save-hook-start nil)
      (xl-perfetto--record
       "X" name "files" start (- (xl-perfetto--elapsed) start) 10
       (list :buffer (and xl-perfetto-trace-record-names (buffer-name)))))))

(defun xl-perfetto--before-save-end ()
  "End the timing of `before-save-hook'."
  (xl-perfetto--save-hook-end "before-save-hook"))

(defun xl-perfetto--after-save-end ()
  "End the timing of `after-save-hook'."
  (xl-perfetto--save-hook-end "after-save-hook"))

(defun xl-perfetto--install-files ()
  "Advise the file functions and time the save hooks."
  (advice-add 'find-file-noselect :around #'xl-perfetto--find-file-around)
  (advice-add 'save-buffer :around #'xl-perfetto--save-buffer-around)
  ;; Start at the front of each hook (depth -100), end at the back (depth 100).
  (add-hook 'before-save-hook #'xl-perfetto--save-hook-begin -100)
  (add-hook 'before-save-hook #'xl-perfetto--before-save-end 100)
  (add-hook 'after-save-hook #'xl-perfetto--save-hook-begin -100)
  (add-hook 'after-save-hook #'xl-perfetto--after-save-end 100)
  (push (lambda ()
          (advice-remove 'find-file-noselect #'xl-perfetto--find-file-around)
          (advice-remove 'save-buffer #'xl-perfetto--save-buffer-around)
          (remove-hook 'before-save-hook #'xl-perfetto--save-hook-begin)
          (remove-hook 'before-save-hook #'xl-perfetto--before-save-end)
          (remove-hook 'after-save-hook #'xl-perfetto--save-hook-begin)
          (remove-hook 'after-save-hook #'xl-perfetto--after-save-end)
          (setq xl-perfetto--save-hook-start nil))
        xl-perfetto--installed))

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
                             (list :gcs-done gcs-done))))))

;;; ------------------------------------------------------------
;;; Redisplay frames
;;; ------------------------------------------------------------

;; Emacs has no hook that says when redisplay ends.  A zero-delay timer
;; scheduled from its first Lisp hook runs as soon as redisplay returns to
;; the command loop, which gives a frame span: the Lisp hooks plus the C
;; drawing in between.  Long frames are marked as jank against
;; `xl-perfetto-trace-frame-budget', the way a SurfaceFlinger trace marks a
;; missed vsync.

(defun xl-perfetto--frame-open ()
  "Open a redisplay frame when none is open, and return it.
The frame is closed by `xl-perfetto--redisplay-frame-end'."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             (memq 'redisplay xl-perfetto-trace-categories))
    (unless xl-perfetto--frame
      (let* ((now (xl-perfetto--elapsed))
             (selected (selected-window))
             (buffer (and (window-live-p selected)
                          (window-buffer selected))))
        (setq xl-perfetto--frame
              (xl-perfetto--make-frame
               :start now
               :interval (and xl-perfetto--last-frame-start
                              (- now xl-perfetto--last-frame-start))
               :key-latency (and xl-perfetto--last-key-time
                                 (- now xl-perfetto--last-key-time))
               :command-latency (and xl-perfetto--last-command-start
                                     (- now xl-perfetto--last-command-start))
               :after-command (and xl-perfetto--last-command-end
                                   (- now xl-perfetto--last-command-end))
               :windows 0
               :spans nil
               :fontified 0
               :selected-buffer (and buffer
                                     xl-perfetto-trace-record-names
                                     (buffer-name buffer))
               :selected-mode (and buffer
                                   (with-current-buffer buffer
                                     (symbol-name major-mode)))))
        ;; Latencies belong to the first frame after the input or command;
        ;; a later idle-timer redisplay must not report them again.
        (setq xl-perfetto--last-frame-start now
              xl-perfetto--last-key-time nil
              xl-perfetto--last-command-start nil
              xl-perfetto--last-command-end nil)
        (setq xl-perfetto--frame-timer
              (run-at-time 0 nil #'xl-perfetto--redisplay-frame-end))))
    xl-perfetto--frame))

(defun xl-perfetto--frame-note-window (window)
  "Count WINDOW in the open frame and mark its redisplay once per frame."
  (when (and xl-perfetto--frame (window-live-p window))
    (let ((frame xl-perfetto--frame))
      (unless (memq window (xl-perfetto--frame-seen frame))
        (push window (xl-perfetto--frame-seen frame))
        (cl-incf (xl-perfetto--frame-windows frame)))
      (when (and xl-perfetto-trace-redisplay-detail
                 (not (memq window (xl-perfetto--frame-pre-seen frame))))
        ;; The marker is written only if the frame itself is recorded.
        (push window (xl-perfetto--frame-pre-seen frame))))))

(defun xl-perfetto--mark-window-redisplay (window &optional timestamp)
  "Record an instant saying that WINDOW is being redisplayed.
TIMESTAMP defaults to now."
  (when (window-live-p window)
    (let* ((buffer (window-buffer window))
           (name (if (and xl-perfetto-trace-record-names
                          (buffer-name buffer))
                     (format "redisplay %s" (buffer-name buffer))
                   "redisplay window"))
           (args (append (list :mode (with-current-buffer buffer
                                       (symbol-name major-mode))
                               :selected (if (eq window (selected-window))
                                             t :false)
                               :width (window-body-width window)
                               :height (window-body-height window))
                         (when xl-perfetto-trace-record-names
                           (list :buffer (buffer-name buffer))))))
      (xl-perfetto--record "i" name "redisplay"
                           (or timestamp (xl-perfetto--elapsed))
                           nil 5 args))))

(defun xl-perfetto--frame-note-phase (phase)
  "Remember that PHASE ran in the open redisplay frame."
  (when xl-perfetto--frame
    (let ((frame xl-perfetto--frame))
      (pcase phase
        ('window-scroll
         (setf (xl-perfetto--frame-scroll frame) t))
        ('window-size-change
         (setf (xl-perfetto--frame-resize frame) t))
        ('window-state-change
         (setf (xl-perfetto--frame-state frame) t))
        ('window-buffer-change
         (setf (xl-perfetto--frame-buffer-change frame) t))))))

(defun xl-perfetto--defer-span (name start duration args)
  "Keep a redisplay span for the open frame, or record it when none is open.
Deferring lets a coalesced frame cost no trace events."
  (if xl-perfetto--frame
      (push (list name start duration args)
            (xl-perfetto--frame-spans xl-perfetto--frame))
    (xl-perfetto--record "X" name "redisplay" start duration 5 args)))

(defun xl-perfetto--redisplay-phase-begin (phase &optional window)
  "Start the Lisp redisplay phase PHASE, for WINDOW when one is known."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (xl-perfetto--frame-open)
    (when (windowp window)
      (xl-perfetto--frame-note-window window))
    (xl-perfetto--frame-note-phase phase)
    (when xl-perfetto-trace-redisplay-detail
      (when xl-perfetto--redisplay-phase
        (xl-perfetto--redisplay-phase-end t))
      (setq xl-perfetto--redisplay-phase
            (list phase (xl-perfetto--elapsed)
                  (and (windowp window) window))))))

(defun xl-perfetto--redisplay-phase-end (&optional interrupted)
  "End the open Lisp redisplay phase, marking it INTERRUPTED if asked."
  (when (and xl-perfetto--recording
             (not xl-perfetto--inside)
             xl-perfetto--redisplay-phase)
    (let* ((phase xl-perfetto--redisplay-phase)
           (name (symbol-name (car phase)))
           (start (nth 1 phase))
           (window (nth 2 phase))
           (args (list :phase name)))
      (setq xl-perfetto--redisplay-phase nil)
      (when interrupted
        (setq args (append args (list :interrupted t))))
      (when (window-live-p window)
        (let ((buffer (window-buffer window)))
          (setq args (append args
                             (list :mode
                                   (with-current-buffer buffer
                                     (symbol-name major-mode))))
                args (if xl-perfetto-trace-record-names
                         (append args (list :buffer (buffer-name buffer)))
                       args))))
      (xl-perfetto--defer-span name start
                               (- (xl-perfetto--elapsed) start) args))))

(defun xl-perfetto--pre-redisplay (&optional window)
  "Note the redisplay of WINDOW, opening a frame when needed."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (xl-perfetto--frame-open)
    (when (windowp window)
      (xl-perfetto--frame-note-window window))))

(defun xl-perfetto--window-state-change-begin (&optional thing)
  "Start the span of `window-state-change-functions' for THING."
  (xl-perfetto--redisplay-phase-begin 'window-state-change
                                      (and (windowp thing) thing)))

(defun xl-perfetto--window-state-change-end (&optional _thing)
  "End the span of `window-state-change-functions'."
  (xl-perfetto--redisplay-phase-end))

(defun xl-perfetto--window-size-change-begin (&optional thing)
  "Start the span of `window-size-change-functions' for THING."
  (xl-perfetto--redisplay-phase-begin 'window-size-change
                                      (and (windowp thing) thing)))

(defun xl-perfetto--window-size-change-end (&optional _thing)
  "End the span of `window-size-change-functions'."
  (xl-perfetto--redisplay-phase-end))

(defun xl-perfetto--window-buffer-change-begin (&optional thing)
  "Start the span of `window-buffer-change-functions' for THING."
  (xl-perfetto--redisplay-phase-begin 'window-buffer-change
                                      (and (windowp thing) thing)))

(defun xl-perfetto--window-buffer-change-end (&optional _thing)
  "End the span of `window-buffer-change-functions'."
  (xl-perfetto--redisplay-phase-end))

(defun xl-perfetto--window-scroll-begin (window _start)
  "Start the span of `window-scroll-functions' for WINDOW."
  (xl-perfetto--redisplay-phase-begin 'window-scroll window))

(defun xl-perfetto--window-scroll-end (_window _start)
  "End the span of `window-scroll-functions'."
  (xl-perfetto--redisplay-phase-end))

(defun xl-perfetto--state-change-hook-begin ()
  "Start the span of `window-state-change-hook'."
  (xl-perfetto--redisplay-phase-begin 'window-state-change-hook))

(defun xl-perfetto--state-change-hook-end ()
  "End the span of `window-state-change-hook'."
  (xl-perfetto--redisplay-phase-end))

(defun xl-perfetto--redisplay-frame-end (&optional cut)
  "Close the open redisplay frame.  CUT says the recording is stopping.
A frame faster than `xl-perfetto-trace-frame-min-interval' is coalesced into
the next recorded frame; a janky frame is always recorded."
  (when (and xl-perfetto--recording (not xl-perfetto--inside))
    (xl-perfetto--redisplay-phase-end)
    (let ((frame xl-perfetto--frame)
          (timer xl-perfetto--frame-timer))
      (when frame
        (setq xl-perfetto--frame nil
              xl-perfetto--frame-timer nil)
        (when (timerp timer)
          (ignore-errors (cancel-timer timer)))
        (let* ((now (xl-perfetto--elapsed))
               (start (xl-perfetto--frame-start frame))
               (duration (max 0 (- now start)))
               (budget (* 1000000.0 (or xl-perfetto-trace-frame-budget 0)))
               (jank (and (> duration budget) t))
               (min-interval
                (* 1000000.0 (or xl-perfetto-trace-frame-min-interval 0)))
               (since-recorded
                (and xl-perfetto--last-recorded-frame-start
                     (- start xl-perfetto--last-recorded-frame-start)))
               (record (or jank
                           (null since-recorded)
                           (>= since-recorded min-interval)))
               (interval (or since-recorded
                             (xl-perfetto--frame-interval frame))))
          (setq xl-perfetto--frame-count (1+ xl-perfetto--frame-count))
          (when jank
            (setq xl-perfetto--jank-count (1+ xl-perfetto--jank-count)))
          (when (> duration xl-perfetto--frame-max)
            (setq xl-perfetto--frame-max duration))
          (if (not record)
              (setq xl-perfetto--coalesced-pending
                    (1+ xl-perfetto--coalesced-pending)
                    xl-perfetto--coalesced-count
                    (1+ xl-perfetto--coalesced-count))
            (let ((pending xl-perfetto--coalesced-pending))
              (setq xl-perfetto--last-recorded-frame-start start
                    xl-perfetto--coalesced-pending 0
                    xl-perfetto--recorded-frame-count
                    (1+ xl-perfetto--recorded-frame-count))
              ;; Details first, so the frame span stays the newest event.
              (dolist (window (reverse (xl-perfetto--frame-pre-seen frame)))
                (xl-perfetto--mark-window-redisplay window start))
              (dolist (span (reverse (xl-perfetto--frame-spans frame)))
                (xl-perfetto--record "X" (nth 0 span) "redisplay"
                                     (nth 1 span) (nth 2 span) 5
                                     (nth 3 span)))
              (let ((args (append
                           (list :windows (xl-perfetto--frame-windows frame)
                                 :fontified (xl-perfetto--frame-fontified frame)
                                 :jank (if jank t :false))
                           (when interval
                             (list :interval-ms (xl-perfetto--ms interval)))
                           (when (> pending 0)
                             (list :coalesced pending))
                           (when (xl-perfetto--frame-key-latency frame)
                             (list :key-latency-ms
                                   (xl-perfetto--ms
                                    (xl-perfetto--frame-key-latency frame))))
                           (when (xl-perfetto--frame-command-latency frame)
                             (list :command-latency-ms
                                   (xl-perfetto--ms
                                    (xl-perfetto--frame-command-latency frame))))
                           (when (xl-perfetto--frame-after-command frame)
                             (list :after-command-ms
                                   (xl-perfetto--ms
                                    (xl-perfetto--frame-after-command frame))))
                           (when (xl-perfetto--frame-scroll frame)
                             (list :scrolled t))
                           (when (xl-perfetto--frame-resize frame)
                             (list :resized t))
                           (when (xl-perfetto--frame-state frame)
                             (list :state-changed t))
                           (when (xl-perfetto--frame-buffer-change frame)
                             (list :buffer-changed t))
                           (when (xl-perfetto--frame-selected-buffer frame)
                             (list :buffer
                                   (xl-perfetto--frame-selected-buffer frame)))
                           (when (xl-perfetto--frame-selected-mode frame)
                             (list :mode
                                   (xl-perfetto--frame-selected-mode frame)))
                           (when cut (list :cut t)))))
                (xl-perfetto--record "X" "redisplay frame" "redisplay"
                                     start duration 5 args)
                ;; Jank is a discrete event (a caret at the start of the bad
                ;; frame), not a 0/1 counter curve.
                (when jank
                  (xl-perfetto--record
                   "i" "^" "redisplay" start nil 5
                   (append (list :frame-ms (xl-perfetto--ms duration)
                                 :budget-ms (xl-perfetto--ms budget)
                                 :windows (xl-perfetto--frame-windows frame))
                           (when (> pending 0)
                             (list :coalesced pending))
                           (when (xl-perfetto--frame-selected-buffer frame)
                             (list :buffer
                                   (xl-perfetto--frame-selected-buffer frame)))
                           (when (xl-perfetto--frame-selected-mode frame)
                             (list :mode
                                   (xl-perfetto--frame-selected-mode
                                    frame))))))
                (when (memq 'counters xl-perfetto-trace-categories)
                  (xl-perfetto--record
                   "C" "redisplay frame" "counters" now nil nil
                   (append (list :ms (xl-perfetto--ms duration)
                                 :interval-ms
                                 (xl-perfetto--ms (or interval 0))
                                 :windows
                                 (xl-perfetto--frame-windows frame))
                           (when (> pending 0)
                             (list :coalesced pending)))))))))))))

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
      (when xl-perfetto--frame
        (cl-incf (xl-perfetto--frame-fontified xl-perfetto--frame) chars))
      (unwind-protect
          (funcall original start)
        (xl-perfetto--defer-span
         "jit-lock" begin (- (xl-perfetto--elapsed) begin)
         (append (list :mode mode :chars chars)
                 (when buffer (list :buffer buffer))
                 (when file (list :file file))))))))

(defun xl-perfetto--scan ()
  "Periodic work of a recording: new processes and the mode line."
  (when xl-perfetto--recording
    (when (memq 'processes xl-perfetto-trace-categories)
      (xl-perfetto--wrap-new-processes))
    (xl-perfetto--update-mode-line)))

;;; ============================================================
;;; User-named functions
;;; ============================================================

(defun xl-perfetto--primitive-safe-p (function)
  "Return non-nil when FUNCTION may be advised without a trampoline.
A C primitive on a native-comp Emacs would have its trampoline compiled
synchronously by `advice-add', freezing the UI for about a second, so it
is refused unless native compilation is unavailable."
  (let ((definition (and (fboundp function) (indirect-function function))))
    (or (not (subr-primitive-p definition))
        (not (and (fboundp 'native-comp-available-p)
                  (native-comp-available-p))))))

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

;;;###autoload
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

(defun xl-perfetto--mx-around (original &rest args)
  "Record the command that M-x runs as its own span, named \"M-x NAME\".
The command loop sees only `execute-extended-command'; the command it runs
is `this-command' once that returns."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original args)
    (let ((start (xl-perfetto--elapsed)))
      (unwind-protect
          (apply original args)
        (xl-perfetto--record
         "X" (format "M-x %s" (or this-command "?")) "commands" start
         (- (xl-perfetto--elapsed) start) 1
         (when xl-perfetto-trace-record-names
           (list :buffer (buffer-name))))))))

(defun xl-perfetto--install-commands ()
  "Install the command hooks, first and last, and the M-x span."
  (add-hook 'pre-command-hook #'xl-perfetto--pre-command -100)
  (add-hook 'post-command-hook #'xl-perfetto--post-command 100)
  (advice-add 'execute-extended-command :around #'xl-perfetto--mx-around)
  (push (lambda ()
          (remove-hook 'pre-command-hook #'xl-perfetto--pre-command)
          (remove-hook 'post-command-hook #'xl-perfetto--post-command)
          (advice-remove 'execute-extended-command #'xl-perfetto--mx-around))
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
  "Advise `xl-async-start' when xl-async is loaded."
  (when (fboundp 'xl-async-start)
    (advice-add 'xl-async-start :around #'xl-perfetto--async-around)
    (push (lambda ()
            (advice-remove 'xl-async-start #'xl-perfetto--async-around))
          xl-perfetto--installed)))

(defun xl-perfetto--install-gc ()
  "Install the hook that records garbage collections."
  (setq xl-perfetto--gc-elapsed gc-elapsed)
  (add-hook 'post-gc-hook #'xl-perfetto--post-gc)
  (push (lambda () (remove-hook 'post-gc-hook #'xl-perfetto--post-gc))
        xl-perfetto--installed))

(defconst xl-perfetto--redisplay-hooks
  '((window-state-change-functions
     xl-perfetto--window-state-change-begin
     xl-perfetto--window-state-change-end)
    (window-size-change-functions
     xl-perfetto--window-size-change-begin
     xl-perfetto--window-size-change-end)
    (window-buffer-change-functions
     xl-perfetto--window-buffer-change-begin
     xl-perfetto--window-buffer-change-end)
    (window-scroll-functions
     xl-perfetto--window-scroll-begin
     xl-perfetto--window-scroll-end)
    (window-state-change-hook
     xl-perfetto--state-change-hook-begin
     xl-perfetto--state-change-hook-end))
  "Hooks that bracket the Lisp phases of a redisplay.
Each entry is (HOOK BEGIN END); BEGIN goes at the front of the hook and
END at the back, so the span covers every other function in it.")

(defun xl-perfetto--install-redisplay-hooks ()
  "Add the phase sentinels to every redisplay hook that exists."
  (dolist (entry xl-perfetto--redisplay-hooks)
    (when (boundp (nth 0 entry))
      (add-hook (nth 0 entry) (nth 1 entry) -100)
      (add-hook (nth 0 entry) (nth 2 entry) 100))))

(defun xl-perfetto--remove-redisplay-hooks ()
  "Remove the phase sentinels from every redisplay hook."
  (dolist (entry xl-perfetto--redisplay-hooks)
    (remove-hook (nth 0 entry) (nth 1 entry))
    (remove-hook (nth 0 entry) (nth 2 entry))))

(defun xl-perfetto--install-redisplay ()
  "Install the redisplay frame, its phase hooks and the font-lock advice."
  (add-hook 'pre-redisplay-functions #'xl-perfetto--pre-redisplay)
  (xl-perfetto--install-redisplay-hooks)
  (push (lambda ()
          (remove-hook 'pre-redisplay-functions
                       #'xl-perfetto--pre-redisplay)
          (xl-perfetto--remove-redisplay-hooks)
          (when (timerp xl-perfetto--frame-timer)
            (cancel-timer xl-perfetto--frame-timer))
          (setq xl-perfetto--frame-timer nil
                xl-perfetto--frame nil
                xl-perfetto--redisplay-phase nil))
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
  "Start the timer that finds new processes and refreshes the mode line."
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
    (when (memq 'modes categories) (xl-perfetto--install-modes))
    (when (memq 'completion categories) (xl-perfetto--install-completion))
    (when (memq 'completion categories) (xl-perfetto--install-vertico))
    (when (memq 'completion categories) (xl-perfetto--install-corfu))
    (when (memq 'input categories) (xl-perfetto--install-input))
    (when (memq 'files categories) (xl-perfetto--install-files))
    ;; The scan timer also refreshes the mode-line indicator, so it runs
    ;; whatever categories are selected.
    (xl-perfetto--install-scan)))

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
        xl-perfetto--frame nil
        xl-perfetto--frame-timer nil
        xl-perfetto--last-frame-start nil
        xl-perfetto--last-key-time nil
        xl-perfetto--last-command-start nil
        xl-perfetto--last-command-end nil
        xl-perfetto--redisplay-phase nil
        xl-perfetto--frame-count 0
        xl-perfetto--jank-count 0
        xl-perfetto--frame-max 0
        xl-perfetto--last-recorded-frame-start nil
        xl-perfetto--recorded-frame-count 0
        xl-perfetto--coalesced-pending 0
        xl-perfetto--coalesced-count 0
        xl-perfetto--process-scan-timer nil
        xl-perfetto--auto-stop-timer nil))

(defun xl-perfetto--write-marker (seconds)
  "Tell other Emacs processes that a recording of SECONDS runs.  Never fails."
  (unless xl-perfetto--follower
    (ignore-errors
      (let* ((marker xl-perfetto-marker-file)
             (temp (concat marker ".tmp")))
        (make-directory (file-name-directory marker) t)
        (with-temp-file temp
          (prin1 (list :dir (xl-perfetto--trace-directory)
                       :started (float-time)
                       :max seconds)
                 (current-buffer)))
        (rename-file temp marker t)))))

(defun xl-perfetto--remove-marker ()
  "Delete the recording marker.  Never fails."
  (unless xl-perfetto--follower
    (ignore-errors (delete-file xl-perfetto-marker-file))))

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
          xl-perfetto--command-start nil
          xl-perfetto--frame nil
          xl-perfetto--frame-timer nil
          xl-perfetto--last-frame-start nil
          xl-perfetto--last-key-time nil
          xl-perfetto--last-command-start nil
          xl-perfetto--last-command-end nil
          xl-perfetto--redisplay-phase nil
          xl-perfetto--frame-count 0
          xl-perfetto--jank-count 0
          xl-perfetto--frame-max 0
          xl-perfetto--last-recorded-frame-start nil
          xl-perfetto--recorded-frame-count 0
          xl-perfetto--coalesced-pending 0
          xl-perfetto--coalesced-count 0
          xl-perfetto--meta-events (xl-perfetto--meta-events)
          xl-perfetto--recording t)
    (condition-case error
        (progn
          (xl-perfetto--install)
          (xl-perfetto--install-auto-stop seconds)
          (xl-perfetto--write-marker seconds)
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
            (xl-perfetto--redisplay-frame-end t)
            (xl-perfetto--uninstall-all)
            (xl-perfetto--close-async-events)
            (xl-perfetto--mode-line-off)
            (setq file (xl-perfetto--write-file))
            file)
        (xl-perfetto--remove-marker)
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

;;; ============================================================
;;; Following: other batch processes record while this one does
;;; ============================================================

(defun xl-perfetto--read-marker ()
  "The recording marker as a plist, or nil when there is none or it is bad."
  (let ((file xl-perfetto-marker-file))
    (when (and (stringp file) (file-regular-p file))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let ((info (read (current-buffer))))
            (and (stringp (plist-get info :dir))
                 (numberp (plist-get info :started))
                 (numberp (plist-get info :max))
                 info)))))))

(defun xl-perfetto-follow-marker (prefix)
  "Record in this batch process while the main Emacs records.
Call it between units of work.  When the marker appears and is not past its
time limit, start a recording into the marker's folder, with file names that
start with PREFIX.  When the marker is gone or stale, stop and write the file.
Returns the file written on a stop, or nil.  Never signals: tracing must not
break the work it watches."
  (ignore-errors
    (setq xl-perfetto--follower t
          xl-perfetto--file-prefix prefix)
    (let* ((info (xl-perfetto--read-marker))
           (left (and info (- (+ (plist-get info :started) (plist-get info :max))
                              (float-time)))))
      (cond
       ((and info (> left 0) (not xl-perfetto--recording))
        (setq xl-perfetto-trace-directory (plist-get info :dir))
        (xl-perfetto--start (max 1 (ceiling left)))
        nil)
       ((and xl-perfetto--recording (or (null info) (<= left 0)))
        (xl-perfetto--stop))))))

(defun xl-perfetto-follow-finish ()
  "Stop this process's following recording, write it and return the file."
  (when (and xl-perfetto--follower xl-perfetto--recording)
    (ignore-errors (xl-perfetto--stop))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun xl-perfetto-trace-function (function)
  "Trace FUNCTION during this and every later recording.
A Lisp API, not a command: call it from your own code or config.  The
function gets an `:around' advice while recording.  C primitives are refused
unless native compilation is unavailable."
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

;;;###autoload
(defun xl-perfetto-untrace-function (function)
  "Stop tracing FUNCTION, now and in later recordings.  A Lisp API."
  (setq xl-perfetto--traced-functions
        (delq function xl-perfetto--traced-functions))
  (xl-perfetto--remove-function-advice function)
  (message "xl-perfetto: no longer tracing %s" function))

;;;###autoload
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

;;;###autoload
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

(provide 'xl-perfetto-trace)
;;; xl-perfetto-trace.el ends here
