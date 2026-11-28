;;; tuned-perfetto-trace.el --- Chrome JSON timelines of Emacs Lisp work -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tuned-async "0.1.0"))
;; Keywords: tools, profiling

;;; Commentary:
;; Record what Emacs does at the Lisp level for a while, then look at it
;; on a timeline in ui.perfetto.dev:
;;
;;   M-x tuned-perfetto-trace-start
;;   ... use Emacs: type, switch buffers, run a command or two ...
;;   M-x tuned-perfetto-trace-stop
;;
;; The result is a Chrome JSON trace written to
;; `tuned-perfetto-trace-directory'.
;; Each category has its own track: commands, timers, process filters and
;; sentinels, GC, redisplay frames and font-lock, functions you asked to trace,
;; and your own marks.  `tuned-async' jobs get their own async tracks, and
;; redisplay frames get a counter track.  A redisplay frame has its duration,
;; the windows it repainted, the Lisp phases inside it and whether it missed
;; its budget (jank, shown as a `^' event), in the style of a SurfaceFlinger
;; trace.  Frames closer than `tuned-perfetto-trace-frame-min-interval' are
;; coalesced, so a program that calls `redisplay' in a loop cannot flood the
;; ring buffer.
;;
;; Nothing is recorded outside a recording: hooks, advice, process function
;; wrappers and timers exist only between `tuned-perfetto-trace-start' and
;; `tuned-perfetto-trace-stop'.  No C primitive is advised, because native-comp
;; would compile a trampoline synchronously and freeze the UI; process
;; callbacks are wrapped with `add-function' instead, and processes created
;; during a recording are found by a periodic scan.
;;
;; Nothing about the content of your work is recorded: no key sequences,
;; buffer text, minibuffer input or process output.  Only names, durations
;; and sizes, plus buffer and file names unless
;; `tuned-perfetto-trace-record-names' is nil.
;;
;; Trace your own code with:
;;
;;   (tuned-perfetto-with-span "scan" (scan-dir dir))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'timer)

(declare-function jit-lock-function "jit-lock" (start))
(declare-function tuned-async-start "tuned-async" (command &rest args))
(declare-function tuned-async-status "tuned-async" (proc))
(declare-function tuned-async-output "tuned-async" (proc))
(declare-function tuned-async-exit-code "tuned-async" (proc))
(declare-function tuned-async-finally "tuned-async" (proc fn))
(declare-function tuned-async-cancel "tuned-async" (&optional slot buffer))

;;; ============================================================
;;; Options
;;; ============================================================

(defgroup tuned-perfetto nil
  "Record Emacs Lisp activity as a Chrome JSON trace."
  :group 'tools
  :prefix "tuned-perfetto-")

(defcustom tuned-perfetto-trace-categories
  '(commands keys timers processes tuned-async gc redisplay functions modes
    completion input files counters)
  "Categories of events a recording collects.
`marks' is not here: `tuned-perfetto-trace-mark' always records, because you
asked for that mark explicitly."
  :type '(set (const commands) (const timers) (const processes)
              (const tuned-async) (const gc) (const redisplay)
              (const functions) (const modes) (const completion)
              (const input) (const files)
              (const counters))
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-marker-file
  (expand-file-name "traces/recording.eld" user-emacs-directory)
  "File that says a recording runs, for other Emacs processes to follow.
Written when a recording starts and deleted when it stops.  A follower
process reads it between units of work and records alongside (see
`tuned-perfetto-follow-marker')."
  :type 'file
  :group 'tuned-perfetto)

(defvar tuned-perfetto--file-prefix "emacs-trace"
  "Start of the trace file names this process writes.")

(defvar tuned-perfetto--follower nil
  "Non-nil in a process that records only because another Emacs records.")

(defcustom tuned-perfetto-trace-ring-size 100000
  "Number of events the ring buffer holds.
When it is full, the oldest event is overwritten and counted in the
trace metadata as a dropped event."
  :type 'natnum
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-max-seconds 120
  "Longest recording, in seconds.
When it is reached, the recording stops and its file is written as if
`tuned-perfetto-trace-stop' had been called."
  :type 'number
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-directory
  (expand-file-name "traces/" user-emacs-directory)
  "Directory the trace files are written to.
It is created when a trace is written."
  :type 'directory
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-record-names t
  "Whether events may name buffers and files.
When nil, an event says only the major mode, never a buffer or file name,
so a trace can be shared without revealing what you were editing."
  :type 'boolean
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-process-scan-interval 0.5
  "Seconds between scans for new processes during a recording.
A process created while recording is wrapped at the next scan, so output
it writes before then is not recorded.  `tuned-async' jobs are recorded
exactly from the start; see the tuned-async probe in tuned-perfetto-probes.el."
  :type 'number
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-frame-budget 0.0167
  "Budget of one redisplay frame, in seconds.
A redisplay that takes longer is marked with `jank' in the trace and
counted as a janky frame, the way a missed vsync is marked in a
SurfaceFlinger trace.  The default is one frame at 60 Hz."
  :type 'number
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-redisplay-detail t
  "Whether a recording details each recorded redisplay frame.
When non-nil, every redisplayed window gets a marker and the Lisp phases
inside redisplay (window state, size and buffer changes, scrolling) get
their own spans.  When nil, a recording keeps only one span per redisplay
frame, which is much cheaper but hides which window and phase ran."
  :type 'boolean
  :group 'tuned-perfetto)

(defcustom tuned-perfetto-trace-frame-min-interval 0.008
  "Shortest gap between two recorded redisplay frames, in seconds.
Frames closer together than this are coalesced into the next recorded
frame, because a program that calls `redisplay' in a loop can make
thousands of frames per second and fill the ring buffer.  A janky frame
is always recorded.  Set this to 0 to record every redisplay.  The
default 0.008 is 125 frames per second, enough for a 120 Hz display."
  :type 'number
  :group 'tuned-perfetto)

;;; ============================================================
;;; Recording state
;;; ============================================================

(defconst tuned-perfetto--tracks
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

(defvar tuned-perfetto--recording nil
  "Non-nil while a recording is running.")

(defvar tuned-perfetto--t0 0
  "Start of the recording, in integer microseconds.")

(defvar tuned-perfetto--started nil
  "Wall-clock time when the recording started, for the metadata.")

(defvar tuned-perfetto--ring nil
  "Vector of event vectors, or nil when nothing is recorded.")

(defvar tuned-perfetto--write-index 0
  "Slot in `tuned-perfetto--ring' where the next event goes.")

(defvar tuned-perfetto--capacity 0
  "Length of `tuned-perfetto--ring', kept here to avoid a call per event.")

(defvar tuned-perfetto--count 0
  "Total number of events written to the ring, including overwritten ones.")

(defvar tuned-perfetto--meta-events nil
  "The track-naming metadata events, kept out of the ring.")

(defvar tuned-perfetto--installed nil
  "Cleanup functions of the instrumentation, most recent first.")

(defvar tuned-perfetto--inside nil
  "Reentrancy guard: non-nil while the recorder itself is writing an event.")

(defvar tuned-perfetto--command-start nil
  "Start of the command being measured, in microseconds, or nil.")

(defvar tuned-perfetto--command-name nil
  "Name of the command being measured, or nil.")

(defvar tuned-perfetto--command-buffer nil
  "Buffer name of the command being measured, or nil.")

(defvar tuned-perfetto--command-file nil
  "File name of the command being measured, or nil.")

(defvar tuned-perfetto--command-mode nil
  "Major mode of the command being measured, or nil.")

(defvar tuned-perfetto--gc-elapsed 0
  "Value of `gc-elapsed' when the recording started.")

(defvar tuned-perfetto--in-gc nil
  "Guards the GC hook against a collection caused by its own work.")

(defvar tuned-perfetto--wrapped-processes nil
  "Processes whose filter and sentinel this recording wrapped.")

(defvar tuned-perfetto--process-scan-timer nil
  "Timer that finds new processes and updates the mode line, or nil.")

(defvar tuned-perfetto--auto-stop-timer nil
  "Timer that stops the recording at its time limit, or nil.")

(defvar tuned-perfetto--async-open nil
  "Alist of open `tuned-async' jobs: id to a plist with :name.")

(defvar tuned-perfetto--async-counter 0
  "Number used to give each `tuned-async' job a trace id.")

(defvar tuned-perfetto--traced-functions nil
  "Functions the user asked to trace.
The list survives a recording, so a function traced once is traced by
every following recording until `tuned-perfetto-untrace-function'.")

(defvar tuned-perfetto--probes nil
  "Registered probes, in installation order.
Each entry is (FEATURE CATEGORY INSTALL).  CATEGORY nil means the probe
runs for every recording.  A probe whose FEATURE is not loaded yet installs
itself when the feature arrives during a recording.")

(defmacro tuned-perfetto-define-probe (feature category install)
  "Register INSTALL as a probe that needs FEATURE and CATEGORY.
CATEGORY nil means the probe is not tied to a trace category.  INSTALL is
called when a recording starts, and again if FEATURE is loaded while one
is running.  Probes live beside the recorder but only attach when their
feature is present, so an unloaded package costs nothing."
  `(progn
     (setq tuned-perfetto--probes
           (append tuned-perfetto--probes (list (list ',feature ,category ,install))))
     ;; A feature already loaded is installed by
     ;; `tuned-perfetto--install-probes' once.  This hook is only for one
     ;; that arrives during a recording.
     (unless (featurep ',feature)
       (with-eval-after-load ',feature
         (when (and tuned-perfetto--recording
                    (or (null ,category)
                        (memq ,category tuned-perfetto-trace-categories)))
           (funcall ,install))))))

(defmacro tuned-perfetto-define-span-advice
    (name function arglist call category track name-form
          &optional args-form skip-form doc)
  "Define NAME as an :around span advice for FUNCTION.
ARGLIST is the advice's arguments after the original; CALL invokes the
original.  NAME-FORM and ARGS-FORM are evaluated after it, and SKIP-FORM,
when given, runs the original without recording.  DOC is an optional
sentence added to the generated docstring."
  (declare (indent 6) (debug t))
  (let* ((target (if (symbolp function)
                     (format "`%s'" (symbol-name function))
                   function))
         (docstring (format "Record a span on the %s track around %s.%s"
                            category target
                            (if doc (concat "\n" doc) ""))))
    `(defun ,name (original ,@arglist)
       ,docstring
       (if (or (not tuned-perfetto--recording) tuned-perfetto--inside ,skip-form)
           ,call
         (let ((start (tuned-perfetto--elapsed)))
           (unwind-protect
               ,call
             (tuned-perfetto--record
              "X" ,name-form ,category start
              (- (tuned-perfetto--elapsed) start) ,track ,args-form)))))))

(defvar tuned-perfetto--function-origins (make-hash-table :test #'eq)
  "Map from an advised function object to the symbol that owns it.")

(defvar tuned-perfetto--mode-line nil
  "The mode-line recording indicator, or nil.")

(defvar tuned-perfetto--old-global-mode-string nil
  "Value of `global-mode-string' before the indicator was added.")

(defvar tuned-perfetto--last-file nil
  "File written by the most recent recording, or nil.")

(defvar tuned-perfetto--last-count 0
  "Number of events in the most recent trace file.")

(defvar tuned-perfetto--last-dropped 0
  "Number of dropped events in the most recent trace file.")

(cl-defstruct (tuned-perfetto--frame (:constructor tuned-perfetto--make-frame)
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

(defvar tuned-perfetto--frame nil
  "The open redisplay frame, or nil when none is open.")

(defvar tuned-perfetto--frame-timer nil
  "Timer that closes the open redisplay frame, or nil.")

(defvar tuned-perfetto--last-frame-start nil
  "Start of the previous redisplay frame, in microseconds, or nil.")

(defvar tuned-perfetto--last-key-time nil
  "Time of the last recorded special key, in microseconds, or nil.")

(defvar tuned-perfetto--last-command-start nil
  "Start of the most recent command, in microseconds, or nil.")

(defvar tuned-perfetto--last-command-end nil
  "End of the most recent command, in microseconds, or nil.")

(defvar tuned-perfetto--redisplay-phase nil
  "The open Lisp redisplay phase as (NAME START WINDOW), or nil.")

(defvar tuned-perfetto--frame-count 0
  "Number of redisplay frames in the current recording.")

(defvar tuned-perfetto--jank-count 0
  "Number of redisplay frames over `tuned-perfetto-trace-frame-budget'.")

(defvar tuned-perfetto--frame-max 0
  "Duration of the longest redisplay frame, in microseconds.")

(defvar tuned-perfetto--last-recorded-frame-start nil
  "Start of the last redisplay frame written to the trace, or nil.")

(defvar tuned-perfetto--recorded-frame-count 0
  "Number of redisplay frames written to the current trace.")

(defvar tuned-perfetto--coalesced-pending 0
  "Fast frames skipped since the last recorded frame.")

(defvar tuned-perfetto--coalesced-count 0
  "Number of redisplay frames coalesced away in the current recording.")

;;; ============================================================
;;; Time, events and the ring
;;; ============================================================

(defun tuned-perfetto--now ()
  "Return the current time in integer microseconds since the epoch."
  (car (time-convert nil 1000000)))

(defun tuned-perfetto--elapsed ()
  "Return microseconds since the recording started."
  (- (tuned-perfetto--now) tuned-perfetto--t0))

(defun tuned-perfetto--ms (microseconds)
  "MICROSECONDS as milliseconds, rounded to two decimals."
  (if microseconds
      (/ (round (* microseconds 100.0)) 100000.0)
    0))

(defun tuned-perfetto--record (ph name cat ts dur tid &optional args id)
  "Write one event into the ring.
PH is the trace-event phase, NAME and CAT its name and category, TS and
DUR its start and duration in microseconds, TID its track, ARGS a plist
of extra fields and ID the asynchronous id of a `b' or `e' event.
The recorder ignores events triggered by its own work, so recording one
event cannot recursively record another."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (let ((tuned-perfetto--inside t)
          (index tuned-perfetto--write-index))
      (aset tuned-perfetto--ring index
            (vector ph name cat ts dur tid args id))
      (setq tuned-perfetto--write-index
            (if (= (1+ index) tuned-perfetto--capacity) 0 (1+ index)))
      (setq tuned-perfetto--count (1+ tuned-perfetto--count)))))

(defun tuned-perfetto--events ()
  "Return the events in the ring, oldest first."
  (let* ((size (length tuned-perfetto--ring))
         (n (min tuned-perfetto--count size))
         (start (mod (- tuned-perfetto--write-index n) size))
         events)
    (dotimes (index n)
      (push (aref tuned-perfetto--ring (mod (+ start index) size)) events))
    (nreverse events)))

(defun tuned-perfetto--dropped ()
  "Return the number of events the ring has overwritten."
  (max 0 (- tuned-perfetto--count (length tuned-perfetto--ring))))

;;; ============================================================
;;; JSON output
;;; ============================================================

(defun tuned-perfetto--event-json (event)
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

(defun tuned-perfetto--json ()
  "Return the whole recording as a Chrome JSON trace string."
  (let ((events (append tuned-perfetto--meta-events (tuned-perfetto--events))))
    (json-serialize
     (list :displayTimeUnit "ms"
           :traceEvents (vconcat (mapcar #'tuned-perfetto--event-json events))
           :metadata (list :emacs-version emacs-version
                           :system-type (symbol-name system-type)
                           :started tuned-perfetto--started
                           :dropped-events (tuned-perfetto--dropped)
                           :redisplay-frames tuned-perfetto--frame-count
                           :redisplay-recorded-frames
                           tuned-perfetto--recorded-frame-count
                           :redisplay-coalesced-frames
                           tuned-perfetto--coalesced-count
                           :redisplay-jank-frames tuned-perfetto--jank-count
                           :redisplay-max-frame-ms
                           (tuned-perfetto--ms tuned-perfetto--frame-max))))))

(defun tuned-perfetto--trace-directory ()
  "Return the trace directory, as a directory name."
  (file-name-as-directory (expand-file-name tuned-perfetto-trace-directory)))

(defun tuned-perfetto--unique-file (directory base)
  "Return an unused file name in DIRECTORY made from BASE."
  (let ((file (expand-file-name (concat base ".json") directory))
        (number 0))
    (while (file-exists-p file)
      (setq number (1+ number)
            file (expand-file-name (format "%s-%d.json" base number) directory)))
    file))

(defun tuned-perfetto--write-file ()
  "Write the recording to a trace file and return its path.
The file is written under a temporary name and renamed into place, so a
recording never leaves a half-written trace behind."
  (let* ((directory (tuned-perfetto--trace-directory))
         (_ (make-directory directory t))
         (base (format "%s-%s" tuned-perfetto--file-prefix
                       (format-time-string "%Y%m%d-%H%M%S")))
         (file (tuned-perfetto--unique-file directory base))
         (temp (make-temp-file (expand-file-name "tuned-perfetto-" directory)
                               nil ".json"))
         (coding-system-for-write 'utf-8-unix)
         (text (tuned-perfetto--json)))
    (unwind-protect
        (progn
          (with-temp-file temp (insert text))
          (rename-file temp file t)
          (setq tuned-perfetto--last-file file
                tuned-perfetto--last-count (+ (length tuned-perfetto--meta-events)
                                           (min tuned-perfetto--count
                                                tuned-perfetto--capacity))
                tuned-perfetto--last-dropped (tuned-perfetto--dropped))
          file)
      (when (file-exists-p temp)
        (ignore-errors (delete-file temp))))))

;;; ============================================================
;;; Mode-line indicator
;;; ============================================================

(defun tuned-perfetto--mode-line-on ()
  "Show the recording indicator in `global-mode-string'."
  (setq tuned-perfetto--old-global-mode-string
        (default-value 'global-mode-string))
  (setq tuned-perfetto--mode-line " ● REC 0s")
  (set-default 'global-mode-string
               (cons '(:eval tuned-perfetto--mode-line)
                     (if (listp tuned-perfetto--old-global-mode-string)
                         tuned-perfetto--old-global-mode-string
                       (list tuned-perfetto--old-global-mode-string))))
  (force-mode-line-update t))

(defun tuned-perfetto--mode-line-off ()
  "Remove the recording indicator."
  (when (boundp 'global-mode-string)
    (set-default 'global-mode-string tuned-perfetto--old-global-mode-string))
  (setq tuned-perfetto--mode-line nil
        tuned-perfetto--old-global-mode-string nil)
  (force-mode-line-update t))

(defun tuned-perfetto--update-mode-line ()
  "Refresh the elapsed seconds of the recording indicator."
  (when tuned-perfetto--recording
    (let ((text (format " ● REC %ds" (/ (tuned-perfetto--elapsed) 1000000))))
      (unless (equal text tuned-perfetto--mode-line)
        (setq tuned-perfetto--mode-line text)
        (force-mode-line-update t)))))

;;; ============================================================
;;; Commands
;;; ============================================================

(defcustom tuned-perfetto-trace-keys '(return)
  "Special keys recorded as instants when they start a command.
Any of `return', `tab', `escape'.  Printable keys are never recorded, so
the trace holds no typed text."
  :type '(set (const return) (const tab) (const escape))
  :group 'tuned-perfetto)

(defun tuned-perfetto--key-label (event)
  "The label of EVENT when it is one of `tuned-perfetto-trace-keys', else nil."
  (let ((key (cond ((memq event '(return 13)) 'return)
                   ((memq event '(tab 9)) 'tab)
                   ((memq event '(escape 27)) 'escape))))
    (and key (memq key tuned-perfetto-trace-keys)
         (pcase key ('return "RET") ('tab "TAB") ('escape "ESC")))))

(defun tuned-perfetto--pre-command ()
  "Remember when the current command started, and when a special key started it."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (let ((label (and (memq 'keys tuned-perfetto-trace-categories)
                      (tuned-perfetto--key-label last-command-event))))
      (when label
        (let ((now (tuned-perfetto--elapsed)))
          (setq tuned-perfetto--last-key-time now)
          (tuned-perfetto--record "i" (format "key %s" label) "keys"
                               now nil 1 nil))))
    ;; A second pre without a post means a recursive edit or an earlier
    ;; span that never ended; the old start is dropped rather than used.
    (setq tuned-perfetto--command-start (tuned-perfetto--elapsed)
          tuned-perfetto--last-command-start tuned-perfetto--command-start
          tuned-perfetto--command-name
          (if (symbolp this-command)
              (symbol-name this-command)
            "(anonymous command)")
          tuned-perfetto--command-buffer
          (and tuned-perfetto-trace-record-names (buffer-name))
          tuned-perfetto--command-file
          (and tuned-perfetto-trace-record-names (buffer-file-name))
          tuned-perfetto--command-mode (symbol-name major-mode))))

(defun tuned-perfetto--post-command ()
  "Record the duration of the command that just ran."
  (when (and tuned-perfetto--recording
             (not tuned-perfetto--inside)
             tuned-perfetto--command-start)
    (let ((start tuned-perfetto--command-start)
          (name tuned-perfetto--command-name)
          (buffer tuned-perfetto--command-buffer)
          (file tuned-perfetto--command-file)
          (mode tuned-perfetto--command-mode))
      (setq tuned-perfetto--last-command-end (tuned-perfetto--elapsed)
            tuned-perfetto--command-start nil)
      (tuned-perfetto--record
       "X" name "commands" start (- (tuned-perfetto--elapsed) start) 1
       (append (list :mode mode)
               (when buffer (list :buffer buffer))
               (when file (list :file file)))))))

;;; ============================================================
;;; Timers
;;; ============================================================

(defun tuned-perfetto--own-function-p (function)
  "Return non-nil when FUNCTION belongs to this recorder."
  (and (symbolp function)
       (string-prefix-p "tuned-perfetto-" (symbol-name function))))

(defun tuned-perfetto--timer-advice (original timer &rest args)
  "Time ORIGINAL, called with TIMER and ARGS."
  (let ((function (timer--function timer)))
    (if (or (not tuned-perfetto--recording)
            tuned-perfetto--inside
            (tuned-perfetto--own-function-p function))
        (apply original timer args)
      (let ((start (tuned-perfetto--elapsed)))
        (unwind-protect
            (apply original timer args)
          (tuned-perfetto--record
           "X"
           (if (symbolp function)
               (symbol-name function)
             "(anonymous timer)")
           "timers" start (- (tuned-perfetto--elapsed) start) 2
           (list :idle (if (timer--idle-delay timer) t :false))))))))

;;; ============================================================
;;; Processes
;;; ============================================================

(tuned-perfetto-define-span-advice tuned-perfetto--process-filter-around
  process-filter (process chunk) (funcall original process chunk)
  "processes" 3
  (format "filter %s" (process-name process))
  (list :bytes (string-bytes chunk))
  nil "It records the bytes the filter received.")

(tuned-perfetto-define-span-advice tuned-perfetto--process-sentinel-around
  process-sentinel (process event) (funcall original process event)
  "processes" 3
  (format "sentinel %s" (process-name process))
  (list :event event)
  nil "It records the event the sentinel received.")

(defun tuned-perfetto--wrap-process (process)
  "Wrap PROCESS's filter and sentinel, once per recording."
  (unless (memq process tuned-perfetto--wrapped-processes)
    (push process tuned-perfetto--wrapped-processes)
    (add-function :around (process-filter process)
                  #'tuned-perfetto--process-filter-around)
    (add-function :around (process-sentinel process)
                  #'tuned-perfetto--process-sentinel-around)))

(defun tuned-perfetto--wrap-new-processes ()
  "Wrap every live process that this recording has not wrapped yet."
  (dolist (process (process-list))
    (tuned-perfetto--wrap-process process)))

(defun tuned-perfetto--unwrap-processes ()
  "Remove this recording's wrappers from every process it touched."
  (dolist (process tuned-perfetto--wrapped-processes)
    (ignore-errors
      (remove-function (process-filter process)
                       #'tuned-perfetto--process-filter-around))
    (ignore-errors
      (remove-function (process-sentinel process)
                       #'tuned-perfetto--process-sentinel-around)))
  (setq tuned-perfetto--wrapped-processes nil))

;;; ============================================================
;;; tuned-async jobs
;;; ============================================================

;; The probe that opens these events lives in tuned-perfetto-probes.el.
;; Stopping must still close whatever it left open when that file was not
;; loaded.
(defun tuned-perfetto--close-async-events ()
  "Close every `tuned-async' job still open when the recording stops."
  (dolist (entry tuned-perfetto--async-open)
    (tuned-perfetto--record
     "e" (plist-get (cdr entry) :name) "tuned-async"
     (tuned-perfetto--elapsed) nil 3
     (list :cancelled-or-running t) (car entry)))
  (setq tuned-perfetto--async-open nil))

;;; ============================================================
;;; Major modes
;;; ============================================================

;; The hooks run for every major-mode change, whether a file was opened, the
;; mode was called directly (`org-mode'), or a mode was switched in a buffer.
;; Advising `set-auto-mode' or `normal-mode' would miss the direct calls.

(defvar tuned-perfetto--mode-change nil
  "(START BUFFER OLD-MODE) of the major-mode change in progress, or nil.")

(defun tuned-perfetto--mode-change-start ()
  "Remember when a major-mode change begins, in microseconds."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (setq tuned-perfetto--mode-change
          (list (tuned-perfetto--elapsed)
                (and tuned-perfetto-trace-record-names (buffer-name))
                (symbol-name major-mode)))))

(defun tuned-perfetto--minibuffer-mode-p ()
  "Non-nil in a minibuffer, whose mode switches are not worth a span."
  (or (minibufferp)
      (memq major-mode '(minibuffer-mode minibuffer-inactive-mode))))

(defun tuned-perfetto--mode-change-end ()
  "Record the major-mode change that has just finished, as a span.
Changes into or out of the minibuffer are skipped: every minibuffer use
switches modes, and that would bury the mode changes that matter."
  (when (and tuned-perfetto--recording
             (not tuned-perfetto--inside)
             tuned-perfetto--mode-change
             (not (tuned-perfetto--minibuffer-mode-p)))
    (let ((change tuned-perfetto--mode-change))
      (setq tuned-perfetto--mode-change nil)
      (tuned-perfetto--record
       "X" (format "mode %s" major-mode) "modes"
       (car change) (- (tuned-perfetto--elapsed) (car change)) 8
       (append (list :from (nth 2 change) :to (symbol-name major-mode))
               (when (nth 1 change) (list :buffer (nth 1 change))))))))

(defun tuned-perfetto--install-modes ()
  "Install the hooks that time major-mode changes."
  (add-hook 'change-major-mode-hook #'tuned-perfetto--mode-change-start)
  (add-hook 'after-change-major-mode-hook #'tuned-perfetto--mode-change-end)
  (push (lambda ()
          (remove-hook 'change-major-mode-hook #'tuned-perfetto--mode-change-start)
          (remove-hook 'after-change-major-mode-hook #'tuned-perfetto--mode-change-end)
          (setq tuned-perfetto--mode-change nil))
        tuned-perfetto--installed))

;;; ============================================================
;;; Completion
;;; ============================================================

;; `completion-at-point' runs every function in `completion-at-point-functions':
;; corfu, cape, ispell and the rest.  Timing that one call covers all of them.
;; Only the duration and the number of candidates are recorded, never the text.

(defun tuned-perfetto--completion-around (original &rest args)
  "Record the duration of `completion-at-point' around ORIGINAL, called with ARGS."
  (if (or (not tuned-perfetto--recording) tuned-perfetto--inside)
      (apply original args)
    (let* ((start (tuned-perfetto--elapsed))
           (buffer (and tuned-perfetto-trace-record-names (buffer-name)))
           (mode (symbol-name major-mode))
           (result nil))
      (unwind-protect
          (setq result (apply original args))
        (tuned-perfetto--record
         "X" "completion-at-point" "completion" start
         (- (tuned-perfetto--elapsed) start) 9
         (append (list :mode mode
                       :candidates (if (and (consp result) (integerp (nth 2 result)))
                                       (- (nth 2 result) (nth 1 result))
                                     0))
                 (when buffer (list :buffer buffer))))))))

(defun tuned-perfetto--install-completion ()
  "Advise `completion-at-point' with the timing."
  (advice-add 'completion-at-point :around #'tuned-perfetto--completion-around)
  (push (lambda ()
          (advice-remove 'completion-at-point #'tuned-perfetto--completion-around))
        tuned-perfetto--installed))

;;; ============================================================
;;; Files: opening and saving
;;; ============================================================

;; `find-file-noselect' and `save-buffer' are Lisp functions, so they are advised.
;; The save hooks are timed separately, because format-on-save and similar
;; hooks are often the slow part of a save.  File names are recorded only when
;; `tuned-perfetto-trace-record-names' allows it.

(defvar tuned-perfetto--save-hook-start nil
  "Microsecond time when the current save hook started, or nil.")

(defun tuned-perfetto--timed-around (name original &rest args)
  "Record a span named NAME around ORIGINAL, called with ARGS, on the files track."
  (if (or (not tuned-perfetto--recording) tuned-perfetto--inside)
      (apply original args)
    (let* ((start (tuned-perfetto--elapsed))
           (buffer (and tuned-perfetto-trace-record-names (buffer-name)))
           (mode (symbol-name major-mode))
           (file (and tuned-perfetto-trace-record-names
                      (or (and (stringp (car args)) (car args))
                          (buffer-file-name)))))
      (unwind-protect
          (apply original args)
        (tuned-perfetto--record
         "X" name "files" start (- (tuned-perfetto--elapsed) start) 10
         (append (list :mode mode)
                 (when buffer (list :buffer buffer))
                 (when file (list :file file))))))))

(defun tuned-perfetto--find-file-around (original &rest args)
  "Record `find-file-noselect' around ORIGINAL, called with ARGS."
  (apply #'tuned-perfetto--timed-around "find-file" original args))

(defun tuned-perfetto--save-buffer-around (original &rest args)
  "Record `save-buffer' around ORIGINAL, called with ARGS."
  (apply #'tuned-perfetto--timed-around "save-buffer" original args))

(defun tuned-perfetto--save-hook-begin ()
  "Start the timing of a save hook."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (setq tuned-perfetto--save-hook-start (tuned-perfetto--elapsed))))

(defun tuned-perfetto--save-hook-end (name)
  "End the timing of the save hook NAME, started by
`tuned-perfetto--save-hook-begin'."
  (when (and tuned-perfetto--recording
             (not tuned-perfetto--inside)
             tuned-perfetto--save-hook-start)
    (let ((start tuned-perfetto--save-hook-start))
      (setq tuned-perfetto--save-hook-start nil)
      (tuned-perfetto--record
       "X" name "files" start (- (tuned-perfetto--elapsed) start) 10
       (list :buffer (and tuned-perfetto-trace-record-names (buffer-name)))))))

(defun tuned-perfetto--before-save-end ()
  "End the timing of `before-save-hook'."
  (tuned-perfetto--save-hook-end "before-save-hook"))

(defun tuned-perfetto--after-save-end ()
  "End the timing of `after-save-hook'."
  (tuned-perfetto--save-hook-end "after-save-hook"))

(defun tuned-perfetto--install-files ()
  "Advise the file functions and time the save hooks."
  (advice-add 'find-file-noselect :around #'tuned-perfetto--find-file-around)
  (advice-add 'save-buffer :around #'tuned-perfetto--save-buffer-around)
  ;; Start at the front of each hook (depth -100), end at the back (depth 100).
  (add-hook 'before-save-hook #'tuned-perfetto--save-hook-begin -100)
  (add-hook 'before-save-hook #'tuned-perfetto--before-save-end 100)
  (add-hook 'after-save-hook #'tuned-perfetto--save-hook-begin -100)
  (add-hook 'after-save-hook #'tuned-perfetto--after-save-end 100)
  (push (lambda ()
          (advice-remove 'find-file-noselect #'tuned-perfetto--find-file-around)
          (advice-remove 'save-buffer #'tuned-perfetto--save-buffer-around)
          (remove-hook 'before-save-hook #'tuned-perfetto--save-hook-begin)
          (remove-hook 'before-save-hook #'tuned-perfetto--before-save-end)
          (remove-hook 'after-save-hook #'tuned-perfetto--save-hook-begin)
          (remove-hook 'after-save-hook #'tuned-perfetto--after-save-end)
          (setq tuned-perfetto--save-hook-start nil))
        tuned-perfetto--installed))

;;; ============================================================
;;; GC, redisplay, counters
;;; ============================================================

(defun tuned-perfetto--post-gc ()
  "Record the garbage collection that just finished."
  (when (and tuned-perfetto--recording
             (not tuned-perfetto--inside)
             (not tuned-perfetto--in-gc))
    (let ((tuned-perfetto--in-gc t))
      (let* ((now (tuned-perfetto--elapsed))
             (elapsed gc-elapsed)
             (duration (max 0 (round (* 1000000 (- elapsed
                                                    tuned-perfetto--gc-elapsed))))))
        (setq tuned-perfetto--gc-elapsed elapsed)
        (tuned-perfetto--record "X" "gc" "gc" (max 0 (- now duration))
                             duration 4
                             (list :gcs-done gcs-done))))))

;;; ------------------------------------------------------------
;;; Redisplay frames
;;; ------------------------------------------------------------

;; Emacs has no hook that says when redisplay ends.  A zero-delay timer
;; scheduled from its first Lisp hook runs as soon as redisplay returns to
;; the command loop, which gives a frame span: the Lisp hooks plus the C
;; drawing in between.  Long frames are marked as jank against
;; `tuned-perfetto-trace-frame-budget', the way a SurfaceFlinger trace marks a
;; missed vsync.

(defun tuned-perfetto--frame-open ()
  "Open a redisplay frame when none is open, and return it.
The frame is closed by `tuned-perfetto--redisplay-frame-end'."
  (when (and tuned-perfetto--recording
             (not tuned-perfetto--inside)
             (memq 'redisplay tuned-perfetto-trace-categories))
    (unless tuned-perfetto--frame
      (let* ((now (tuned-perfetto--elapsed))
             (selected (selected-window))
             (buffer (and (window-live-p selected)
                          (window-buffer selected))))
        (setq tuned-perfetto--frame
              (tuned-perfetto--make-frame
               :start now
               :interval (and tuned-perfetto--last-frame-start
                              (- now tuned-perfetto--last-frame-start))
               :key-latency (and tuned-perfetto--last-key-time
                                 (- now tuned-perfetto--last-key-time))
               :command-latency (and tuned-perfetto--last-command-start
                                     (- now tuned-perfetto--last-command-start))
               :after-command (and tuned-perfetto--last-command-end
                                   (- now tuned-perfetto--last-command-end))
               :windows 0
               :spans nil
               :fontified 0
               :selected-buffer (and buffer
                                     tuned-perfetto-trace-record-names
                                     (buffer-name buffer))
               :selected-mode (and buffer
                                   (with-current-buffer buffer
                                     (symbol-name major-mode)))))
        ;; Latencies belong to the first frame after the input or command;
        ;; a later idle-timer redisplay must not report them again.
        (setq tuned-perfetto--last-frame-start now
              tuned-perfetto--last-key-time nil
              tuned-perfetto--last-command-start nil
              tuned-perfetto--last-command-end nil)
        (setq tuned-perfetto--frame-timer
              (run-at-time 0 nil #'tuned-perfetto--redisplay-frame-end))))
    tuned-perfetto--frame))

(defun tuned-perfetto--frame-note-window (window)
  "Count WINDOW in the open frame and mark its redisplay once per frame."
  (when (and tuned-perfetto--frame (window-live-p window))
    (let ((frame tuned-perfetto--frame))
      (unless (memq window (tuned-perfetto--frame-seen frame))
        (push window (tuned-perfetto--frame-seen frame))
        (cl-incf (tuned-perfetto--frame-windows frame)))
      (when (and tuned-perfetto-trace-redisplay-detail
                 (not (memq window (tuned-perfetto--frame-pre-seen frame))))
        ;; The marker is written only if the frame itself is recorded.
        (push window (tuned-perfetto--frame-pre-seen frame))))))

(defun tuned-perfetto--mark-window-redisplay (window &optional timestamp)
  "Record an instant saying that WINDOW is being redisplayed.
TIMESTAMP defaults to now."
  (when (window-live-p window)
    (let* ((buffer (window-buffer window))
           (name (if (and tuned-perfetto-trace-record-names
                          (buffer-name buffer))
                     (format "redisplay %s" (buffer-name buffer))
                   "redisplay window"))
           (args (append (list :mode (with-current-buffer buffer
                                       (symbol-name major-mode))
                               :selected (if (eq window (selected-window))
                                             t :false)
                               :width (window-body-width window)
                               :height (window-body-height window))
                         (when tuned-perfetto-trace-record-names
                           (list :buffer (buffer-name buffer))))))
      (tuned-perfetto--record "i" name "redisplay"
                           (or timestamp (tuned-perfetto--elapsed))
                           nil 5 args))))

(defun tuned-perfetto--frame-note-phase (phase)
  "Remember that PHASE ran in the open redisplay frame."
  (when tuned-perfetto--frame
    (let ((frame tuned-perfetto--frame))
      (pcase phase
        ('window-scroll
         (setf (tuned-perfetto--frame-scroll frame) t))
        ('window-size-change
         (setf (tuned-perfetto--frame-resize frame) t))
        ('window-state-change
         (setf (tuned-perfetto--frame-state frame) t))
        ('window-buffer-change
         (setf (tuned-perfetto--frame-buffer-change frame) t))))))

(defun tuned-perfetto--defer-span (name start duration args)
  "Keep a redisplay span for the open frame, or record it when none is open.
Deferring lets a coalesced frame cost no trace events."
  (if tuned-perfetto--frame
      (push (list name start duration args)
            (tuned-perfetto--frame-spans tuned-perfetto--frame))
    (tuned-perfetto--record "X" name "redisplay" start duration 5 args)))

(defun tuned-perfetto--redisplay-phase-begin (phase &optional window)
  "Start the Lisp redisplay phase PHASE, for WINDOW when one is known."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (tuned-perfetto--frame-open)
    (when (windowp window)
      (tuned-perfetto--frame-note-window window))
    (tuned-perfetto--frame-note-phase phase)
    (when tuned-perfetto-trace-redisplay-detail
      (when tuned-perfetto--redisplay-phase
        (tuned-perfetto--redisplay-phase-end t))
      (setq tuned-perfetto--redisplay-phase
            (list phase (tuned-perfetto--elapsed)
                  (and (windowp window) window))))))

(defun tuned-perfetto--redisplay-phase-end (&optional interrupted)
  "End the open Lisp redisplay phase, marking it INTERRUPTED if asked."
  (when (and tuned-perfetto--recording
             (not tuned-perfetto--inside)
             tuned-perfetto--redisplay-phase)
    (let* ((phase tuned-perfetto--redisplay-phase)
           (name (symbol-name (car phase)))
           (start (nth 1 phase))
           (window (nth 2 phase))
           (args (list :phase name)))
      (setq tuned-perfetto--redisplay-phase nil)
      (when interrupted
        (setq args (append args (list :interrupted t))))
      (when (window-live-p window)
        (let ((buffer (window-buffer window)))
          (setq args (append args
                             (list :mode
                                   (with-current-buffer buffer
                                     (symbol-name major-mode))))
                args (if tuned-perfetto-trace-record-names
                         (append args (list :buffer (buffer-name buffer)))
                       args))))
      (tuned-perfetto--defer-span name start
                               (- (tuned-perfetto--elapsed) start) args))))

(defun tuned-perfetto--pre-redisplay (&optional window)
  "Note the redisplay of WINDOW, opening a frame when needed."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (tuned-perfetto--frame-open)
    (when (windowp window)
      (tuned-perfetto--frame-note-window window))))

(defun tuned-perfetto--window-state-change-begin (&optional thing)
  "Start the span of `window-state-change-functions' for THING."
  (tuned-perfetto--redisplay-phase-begin 'window-state-change
                                      (and (windowp thing) thing)))

(defun tuned-perfetto--window-state-change-end (&optional _thing)
  "End the span of `window-state-change-functions'."
  (tuned-perfetto--redisplay-phase-end))

(defun tuned-perfetto--window-size-change-begin (&optional thing)
  "Start the span of `window-size-change-functions' for THING."
  (tuned-perfetto--redisplay-phase-begin 'window-size-change
                                      (and (windowp thing) thing)))

(defun tuned-perfetto--window-size-change-end (&optional _thing)
  "End the span of `window-size-change-functions'."
  (tuned-perfetto--redisplay-phase-end))

(defun tuned-perfetto--window-buffer-change-begin (&optional thing)
  "Start the span of `window-buffer-change-functions' for THING."
  (tuned-perfetto--redisplay-phase-begin 'window-buffer-change
                                      (and (windowp thing) thing)))

(defun tuned-perfetto--window-buffer-change-end (&optional _thing)
  "End the span of `window-buffer-change-functions'."
  (tuned-perfetto--redisplay-phase-end))

(defun tuned-perfetto--window-scroll-begin (window _start)
  "Start the span of `window-scroll-functions' for WINDOW."
  (tuned-perfetto--redisplay-phase-begin 'window-scroll window))

(defun tuned-perfetto--window-scroll-end (_window _start)
  "End the span of `window-scroll-functions'."
  (tuned-perfetto--redisplay-phase-end))

(defun tuned-perfetto--state-change-hook-begin ()
  "Start the span of `window-state-change-hook'."
  (tuned-perfetto--redisplay-phase-begin 'window-state-change-hook))

(defun tuned-perfetto--state-change-hook-end ()
  "End the span of `window-state-change-hook'."
  (tuned-perfetto--redisplay-phase-end))

(defun tuned-perfetto--redisplay-frame-end (&optional cut)
  "Close the open redisplay frame.  CUT says the recording is stopping.
A frame faster than `tuned-perfetto-trace-frame-min-interval' is coalesced into
the next recorded frame; a janky frame is always recorded."
  (when (and tuned-perfetto--recording (not tuned-perfetto--inside))
    (tuned-perfetto--redisplay-phase-end)
    (let ((frame tuned-perfetto--frame)
          (timer tuned-perfetto--frame-timer))
      (when frame
        (setq tuned-perfetto--frame nil
              tuned-perfetto--frame-timer nil)
        (when (timerp timer)
          (ignore-errors (cancel-timer timer)))
        (let* ((now (tuned-perfetto--elapsed))
               (start (tuned-perfetto--frame-start frame))
               (duration (max 0 (- now start)))
               (budget (* 1000000.0 (or tuned-perfetto-trace-frame-budget 0)))
               (jank (and (> duration budget) t))
               (min-interval
                (* 1000000.0 (or tuned-perfetto-trace-frame-min-interval 0)))
               (since-recorded
                (and tuned-perfetto--last-recorded-frame-start
                     (- start tuned-perfetto--last-recorded-frame-start)))
               (record (or jank
                           (null since-recorded)
                           (>= since-recorded min-interval)))
               (interval (or since-recorded
                             (tuned-perfetto--frame-interval frame))))
          (setq tuned-perfetto--frame-count (1+ tuned-perfetto--frame-count))
          (when jank
            (setq tuned-perfetto--jank-count (1+ tuned-perfetto--jank-count)))
          (when (> duration tuned-perfetto--frame-max)
            (setq tuned-perfetto--frame-max duration))
          (if (not record)
              (setq tuned-perfetto--coalesced-pending
                    (1+ tuned-perfetto--coalesced-pending)
                    tuned-perfetto--coalesced-count
                    (1+ tuned-perfetto--coalesced-count))
            (let ((pending tuned-perfetto--coalesced-pending))
              (setq tuned-perfetto--last-recorded-frame-start start
                    tuned-perfetto--coalesced-pending 0
                    tuned-perfetto--recorded-frame-count
                    (1+ tuned-perfetto--recorded-frame-count))
              ;; Details first, so the frame span stays the newest event.
              (dolist (window (reverse (tuned-perfetto--frame-pre-seen frame)))
                (tuned-perfetto--mark-window-redisplay window start))
              (dolist (span (reverse (tuned-perfetto--frame-spans frame)))
                (tuned-perfetto--record "X" (nth 0 span) "redisplay"
                                     (nth 1 span) (nth 2 span) 5
                                     (nth 3 span)))
              (let ((args (append
                           (list :windows (tuned-perfetto--frame-windows frame)
                                 :fontified (tuned-perfetto--frame-fontified frame)
                                 :jank (if jank t :false))
                           (when interval
                             (list :interval-ms (tuned-perfetto--ms interval)))
                           (when (> pending 0)
                             (list :coalesced pending))
                           (when (tuned-perfetto--frame-key-latency frame)
                             (list :key-latency-ms
                                   (tuned-perfetto--ms
                                    (tuned-perfetto--frame-key-latency frame))))
                           (when (tuned-perfetto--frame-command-latency frame)
                             (list :command-latency-ms
                                   (tuned-perfetto--ms
                                    (tuned-perfetto--frame-command-latency frame))))
                           (when (tuned-perfetto--frame-after-command frame)
                             (list :after-command-ms
                                   (tuned-perfetto--ms
                                    (tuned-perfetto--frame-after-command frame))))
                           (when (tuned-perfetto--frame-scroll frame)
                             (list :scrolled t))
                           (when (tuned-perfetto--frame-resize frame)
                             (list :resized t))
                           (when (tuned-perfetto--frame-state frame)
                             (list :state-changed t))
                           (when (tuned-perfetto--frame-buffer-change frame)
                             (list :buffer-changed t))
                           (when (tuned-perfetto--frame-selected-buffer frame)
                             (list :buffer
                                   (tuned-perfetto--frame-selected-buffer frame)))
                           (when (tuned-perfetto--frame-selected-mode frame)
                             (list :mode
                                   (tuned-perfetto--frame-selected-mode frame)))
                           (when cut (list :cut t)))))
                (tuned-perfetto--record "X" "redisplay frame" "redisplay"
                                     start duration 5 args)
                ;; Jank is a discrete event (a caret at the start of the bad
                ;; frame), not a 0/1 counter curve.
                (when jank
                  (tuned-perfetto--record
                   "i" "^" "redisplay" start nil 5
                   (append (list :frame-ms (tuned-perfetto--ms duration)
                                 :budget-ms (tuned-perfetto--ms budget)
                                 :windows (tuned-perfetto--frame-windows frame))
                           (when (> pending 0)
                             (list :coalesced pending))
                           (when (tuned-perfetto--frame-selected-buffer frame)
                             (list :buffer
                                   (tuned-perfetto--frame-selected-buffer frame)))
                           (when (tuned-perfetto--frame-selected-mode frame)
                             (list :mode
                                   (tuned-perfetto--frame-selected-mode
                                    frame))))))
                (when (memq 'counters tuned-perfetto-trace-categories)
                  (tuned-perfetto--record
                   "C" "redisplay frame" "counters" now nil nil
                   (append (list :ms (tuned-perfetto--ms duration)
                                 :interval-ms
                                 (tuned-perfetto--ms (or interval 0))
                                 :windows
                                 (tuned-perfetto--frame-windows frame))
                           (when (> pending 0)
                             (list :coalesced pending)))))))))))))

(defun tuned-perfetto--jit-lock-around (original start)
  "Time ORIGINAL, a `jit-lock-function' call starting at START."
  (if (or (not tuned-perfetto--recording) tuned-perfetto--inside)
      (funcall original start)
    (let ((begin (tuned-perfetto--elapsed))
          (buffer (and tuned-perfetto-trace-record-names (buffer-name)))
          (file (and tuned-perfetto-trace-record-names (buffer-file-name)))
          (mode (symbol-name major-mode))
          (chars (if (numberp start)
                     (max 0 (- (point-max) start))
                   0)))
      (when tuned-perfetto--frame
        (cl-incf (tuned-perfetto--frame-fontified tuned-perfetto--frame) chars))
      (unwind-protect
          (funcall original start)
        (tuned-perfetto--defer-span
         "jit-lock" begin (- (tuned-perfetto--elapsed) begin)
         (append (list :mode mode :chars chars)
                 (when buffer (list :buffer buffer))
                 (when file (list :file file))))))))

(defun tuned-perfetto--scan ()
  "Periodic work of a recording: new processes and the mode line."
  (when tuned-perfetto--recording
    (when (memq 'processes tuned-perfetto-trace-categories)
      (tuned-perfetto--wrap-new-processes))
    (tuned-perfetto--update-mode-line)))

;;; ============================================================
;;; User-named functions
;;; ============================================================

(defun tuned-perfetto--primitive-safe-p (function)
  "Return non-nil when FUNCTION may be advised without a trampoline.
A C primitive on a native-comp Emacs would have its trampoline compiled
synchronously by `advice-add', freezing the UI for about a second, so it
is refused unless native compilation is unavailable."
  (let ((definition (and (fboundp function) (indirect-function function))))
    (or (not (subr-primitive-p definition))
        (not (and (fboundp 'native-comp-available-p)
                  (native-comp-available-p))))))

(defun tuned-perfetto--check-traceable (function)
  "Signal a `user-error' when FUNCTION cannot be traced."
  (cond
   ((not (symbolp function))
    (user-error "Trace a function symbol, not %S" function))
   ((not (fboundp function))
    (user-error "No such function: %s" function))
   ((tuned-perfetto--own-function-p function)
    (user-error "tuned-perfetto does not trace itself: %s" function))
   ((not (tuned-perfetto--primitive-safe-p function))
    (user-error "Refusing to advise the C primitive %s: native-comp would compile a trampoline synchronously"
                function))))

(defun tuned-perfetto--install-function-advice (function)
  "Install the tracing advice on FUNCTION."
  (unless (advice-member-p #'tuned-perfetto--function-advice function)
    (puthash (symbol-function function) (symbol-name function)
             tuned-perfetto--function-origins)
    (advice-add function :around #'tuned-perfetto--function-advice)))

(defun tuned-perfetto--remove-function-advice (function)
  "Remove the tracing advice from FUNCTION."
  (when (advice-member-p #'tuned-perfetto--function-advice function)
    (advice-remove function #'tuned-perfetto--function-advice)))

(tuned-perfetto-define-span-advice tuned-perfetto--function-advice
  "a user-named function" (&rest args) (apply original args)
  "functions" 6
  (or (gethash original tuned-perfetto--function-origins) "function")
  (append (list :mode (symbol-name major-mode))
          (when tuned-perfetto-trace-record-names
            (list :buffer (buffer-name)))
          (when tuned-perfetto-trace-record-names
            (list :file (buffer-file-name))))
  nil "It names the function from the advice table.")

;;;###autoload
(defmacro tuned-perfetto-with-span (name &rest body)
  "Run BODY and, while recording, put it on the timeline as NAME.
Outside a recording this is just BODY plus one variable check.  A
non-local exit still ends the span."
  (declare (indent 1) (debug (form body)))
  `(if (not (and tuned-perfetto--recording
                 (memq 'functions tuned-perfetto-trace-categories)))
       (progn ,@body)
     (let ((span-name (format "%s" ,name))
           (span-start (tuned-perfetto--elapsed))
           (span-buffer (and tuned-perfetto-trace-record-names (buffer-name)))
           (span-file (and tuned-perfetto-trace-record-names (buffer-file-name)))
           (span-mode (symbol-name major-mode)))
       (unwind-protect
           (progn ,@body)
         (tuned-perfetto--record
          "X" span-name "functions" span-start
          (- (tuned-perfetto--elapsed) span-start) 6
          (append (list :mode span-mode)
                  (when span-buffer (list :buffer span-buffer))
                  (when span-file (list :file span-file))))))))

;;; ============================================================
;;; Installation and teardown
;;; ============================================================

(tuned-perfetto-define-span-advice tuned-perfetto--mx-around
  execute-extended-command (&rest args) (apply original args)
  "commands" 1
  (format "M-x %s" (or this-command "?"))
  (when tuned-perfetto-trace-record-names
    (list :buffer (buffer-name)))
  nil "It names the command that M-x ran.")

(defun tuned-perfetto--install-commands ()
  "Install the command hooks, first and last, and the M-x span."
  (add-hook 'pre-command-hook #'tuned-perfetto--pre-command -100)
  (add-hook 'post-command-hook #'tuned-perfetto--post-command 100)
  (advice-add 'execute-extended-command :around #'tuned-perfetto--mx-around)
  (push (lambda ()
          (remove-hook 'pre-command-hook #'tuned-perfetto--pre-command)
          (remove-hook 'post-command-hook #'tuned-perfetto--post-command)
          (advice-remove 'execute-extended-command #'tuned-perfetto--mx-around))
        tuned-perfetto--installed))

(defun tuned-perfetto--install-timers ()
  "Install the advice on `timer-event-handler'."
  (advice-add 'timer-event-handler :around #'tuned-perfetto--timer-advice)
  (push (lambda ()
          (advice-remove 'timer-event-handler #'tuned-perfetto--timer-advice))
        tuned-perfetto--installed))

(defun tuned-perfetto--install-processes ()
  "Wrap every live process and remember to unwrap them."
  (tuned-perfetto--wrap-new-processes)
  (push #'tuned-perfetto--unwrap-processes tuned-perfetto--installed))

(defun tuned-perfetto--install-gc ()
  "Install the hook that records garbage collections."
  (setq tuned-perfetto--gc-elapsed gc-elapsed)
  (add-hook 'post-gc-hook #'tuned-perfetto--post-gc)
  (push (lambda () (remove-hook 'post-gc-hook #'tuned-perfetto--post-gc))
        tuned-perfetto--installed))

(defconst tuned-perfetto--redisplay-hooks
  '((window-state-change-functions
     tuned-perfetto--window-state-change-begin
     tuned-perfetto--window-state-change-end)
    (window-size-change-functions
     tuned-perfetto--window-size-change-begin
     tuned-perfetto--window-size-change-end)
    (window-buffer-change-functions
     tuned-perfetto--window-buffer-change-begin
     tuned-perfetto--window-buffer-change-end)
    (window-scroll-functions
     tuned-perfetto--window-scroll-begin
     tuned-perfetto--window-scroll-end)
    (window-state-change-hook
     tuned-perfetto--state-change-hook-begin
     tuned-perfetto--state-change-hook-end))
  "Hooks that bracket the Lisp phases of a redisplay.
Each entry is (HOOK BEGIN END); BEGIN goes at the front of the hook and
END at the back, so the span covers every other function in it.")

(defun tuned-perfetto--install-redisplay-hooks ()
  "Add the phase sentinels to every redisplay hook that exists."
  (dolist (entry tuned-perfetto--redisplay-hooks)
    (when (boundp (nth 0 entry))
      (add-hook (nth 0 entry) (nth 1 entry) -100)
      (add-hook (nth 0 entry) (nth 2 entry) 100))))

(defun tuned-perfetto--remove-redisplay-hooks ()
  "Remove the phase sentinels from every redisplay hook."
  (dolist (entry tuned-perfetto--redisplay-hooks)
    (remove-hook (nth 0 entry) (nth 1 entry))
    (remove-hook (nth 0 entry) (nth 2 entry))))

(defun tuned-perfetto--install-redisplay ()
  "Install the redisplay frame, its phase hooks and the font-lock advice."
  (add-hook 'pre-redisplay-functions #'tuned-perfetto--pre-redisplay)
  (tuned-perfetto--install-redisplay-hooks)
  (push (lambda ()
          (remove-hook 'pre-redisplay-functions
                       #'tuned-perfetto--pre-redisplay)
          (tuned-perfetto--remove-redisplay-hooks)
          (when (timerp tuned-perfetto--frame-timer)
            (cancel-timer tuned-perfetto--frame-timer))
          (setq tuned-perfetto--frame-timer nil
                tuned-perfetto--frame nil
                tuned-perfetto--redisplay-phase nil))
        tuned-perfetto--installed)
  (require 'jit-lock)
  (when (fboundp 'jit-lock-function)
    (advice-add 'jit-lock-function :around #'tuned-perfetto--jit-lock-around)
    (push (lambda ()
            (advice-remove 'jit-lock-function #'tuned-perfetto--jit-lock-around))
          tuned-perfetto--installed)))

(defun tuned-perfetto--install-functions ()
  "Install tracing advice on every registered function."
  (dolist (function tuned-perfetto--traced-functions)
    (when (and (fboundp function)
               (tuned-perfetto--primitive-safe-p function))
      (tuned-perfetto--install-function-advice function)
      (push (lambda ()
              (tuned-perfetto--remove-function-advice function))
            tuned-perfetto--installed))))

(defun tuned-perfetto--install-scan ()
  "Start the timer that finds new processes and refreshes the mode line."
  (let ((interval (max 0.01 (or tuned-perfetto-trace-process-scan-interval 0.5))))
    (setq tuned-perfetto--process-scan-timer
          (run-with-timer interval interval #'tuned-perfetto--scan))
    (push (lambda ()
            (when (timerp tuned-perfetto--process-scan-timer)
              (cancel-timer tuned-perfetto--process-scan-timer))
            (setq tuned-perfetto--process-scan-timer nil))
          tuned-perfetto--installed)))

(defun tuned-perfetto--install-auto-stop (seconds)
  "Stop the recording automatically after SECONDS."
  (when (and (numberp seconds) (> seconds 0))
    (setq tuned-perfetto--auto-stop-timer
          (run-at-time seconds nil #'tuned-perfetto--auto-stop))
    (push (lambda ()
            (when (timerp tuned-perfetto--auto-stop-timer)
              (cancel-timer tuned-perfetto--auto-stop-timer))
            (setq tuned-perfetto--auto-stop-timer nil))
          tuned-perfetto--installed)))

;; Each probe names the feature it needs and the category that selects it.
;; Built-in probes use the always-present `emacs' feature; optional packages
;; attach only when they are loaded, and attach themselves if they load
;; during a recording.
(tuned-perfetto-define-probe emacs 'commands #'tuned-perfetto--install-commands)
(tuned-perfetto-define-probe timer 'timers #'tuned-perfetto--install-timers)
(tuned-perfetto-define-probe emacs 'processes #'tuned-perfetto--install-processes)
(tuned-perfetto-define-probe emacs 'gc #'tuned-perfetto--install-gc)
(tuned-perfetto-define-probe emacs 'redisplay #'tuned-perfetto--install-redisplay)
(tuned-perfetto-define-probe emacs 'functions #'tuned-perfetto--install-functions)
(tuned-perfetto-define-probe emacs 'modes #'tuned-perfetto--install-modes)
(tuned-perfetto-define-probe emacs 'completion #'tuned-perfetto--install-completion)
(tuned-perfetto-define-probe emacs 'files #'tuned-perfetto--install-files)
;; The scan timer also refreshes the mode-line indicator, so it runs
;; whatever categories are selected.
(tuned-perfetto-define-probe emacs nil #'tuned-perfetto--install-scan)

(defun tuned-perfetto--install-probes ()
  "Install every registered probe whose feature and category apply."
  (dolist (probe tuned-perfetto--probes)
    (pcase-let ((`(,feature ,category ,install) probe))
      (when (and (or (null category)
                     (memq category tuned-perfetto-trace-categories))
                 (featurep feature))
        (funcall install)))))

(defun tuned-perfetto--install ()
  "Install the instrumentation the selected categories ask for.
The package probes live in tuned-perfetto-probes.el, which is loaded here so
the core works even when that file is absent.  A package that loads later
installs its own probe."
  (require 'tuned-perfetto-probes nil t)
  (tuned-perfetto--install-probes))

(defun tuned-perfetto--uninstall-all ()
  "Run every cleanup function, most recently installed first."
  (let ((installed tuned-perfetto--installed))
    (setq tuned-perfetto--installed nil)
    (dolist (cleanup installed)
      (ignore-errors (funcall cleanup)))))

(defun tuned-perfetto--meta-events ()
  "Return the metadata events that name each track."
  (mapcar (lambda (track)
            (vector "M" "thread_name" nil 0 nil (car track)
                    (list :name (cdr track)) nil))
          tuned-perfetto--tracks))

(defun tuned-perfetto--reset-state ()
  "Forget a finished or failed recording, keeping the user settings."
  (setq tuned-perfetto--ring nil
        tuned-perfetto--capacity 0
        tuned-perfetto--write-index 0
        tuned-perfetto--count 0
        tuned-perfetto--meta-events nil
        tuned-perfetto--inside nil
        tuned-perfetto--command-start nil
        tuned-perfetto--command-name nil
        tuned-perfetto--command-buffer nil
        tuned-perfetto--command-file nil
        tuned-perfetto--command-mode nil
        tuned-perfetto--gc-elapsed 0
        tuned-perfetto--frame nil
        tuned-perfetto--frame-timer nil
        tuned-perfetto--last-frame-start nil
        tuned-perfetto--last-key-time nil
        tuned-perfetto--last-command-start nil
        tuned-perfetto--last-command-end nil
        tuned-perfetto--redisplay-phase nil
        tuned-perfetto--frame-count 0
        tuned-perfetto--jank-count 0
        tuned-perfetto--frame-max 0
        tuned-perfetto--last-recorded-frame-start nil
        tuned-perfetto--recorded-frame-count 0
        tuned-perfetto--coalesced-pending 0
        tuned-perfetto--coalesced-count 0
        tuned-perfetto--process-scan-timer nil
        tuned-perfetto--auto-stop-timer nil))

(defun tuned-perfetto--write-marker (seconds)
  "Tell other Emacs processes that a recording of SECONDS runs.  Never fails."
  (unless tuned-perfetto--follower
    (ignore-errors
      (let* ((marker tuned-perfetto-marker-file)
             (temp (concat marker ".tmp")))
        (make-directory (file-name-directory marker) t)
        (with-temp-file temp
          (prin1 (list :dir (tuned-perfetto--trace-directory)
                       :started (float-time)
                       :max seconds)
                 (current-buffer)))
        (rename-file temp marker t)))))

(defun tuned-perfetto--remove-marker ()
  "Delete the recording marker.  Never fails."
  (unless tuned-perfetto--follower
    (ignore-errors (delete-file tuned-perfetto-marker-file))))

(defun tuned-perfetto--start (&optional max-seconds)
  "Start a recording, automatically stopping after MAX-SECONDS.
Any failure rolls back the instrumentation already installed and then
signals the error."
  (let ((size (max 1 (or tuned-perfetto-trace-ring-size 100000)))
        (seconds (or max-seconds tuned-perfetto-trace-max-seconds)))
    (setq tuned-perfetto--ring (make-vector size nil)
          tuned-perfetto--capacity size
          tuned-perfetto--write-index 0
          tuned-perfetto--count 0
          tuned-perfetto--t0 (tuned-perfetto--now)
          tuned-perfetto--started (format-time-string "%Y-%m-%dT%H:%M:%S")
          tuned-perfetto--installed nil
          tuned-perfetto--inside nil
          tuned-perfetto--wrapped-processes nil
          tuned-perfetto--async-open nil
          tuned-perfetto--command-start nil
          tuned-perfetto--frame nil
          tuned-perfetto--frame-timer nil
          tuned-perfetto--last-frame-start nil
          tuned-perfetto--last-key-time nil
          tuned-perfetto--last-command-start nil
          tuned-perfetto--last-command-end nil
          tuned-perfetto--redisplay-phase nil
          tuned-perfetto--frame-count 0
          tuned-perfetto--jank-count 0
          tuned-perfetto--frame-max 0
          tuned-perfetto--last-recorded-frame-start nil
          tuned-perfetto--recorded-frame-count 0
          tuned-perfetto--coalesced-pending 0
          tuned-perfetto--coalesced-count 0
          tuned-perfetto--meta-events (tuned-perfetto--meta-events)
          tuned-perfetto--recording t)
    (condition-case error
        (progn
          (tuned-perfetto--install)
          (tuned-perfetto--install-auto-stop seconds)
          (tuned-perfetto--write-marker seconds)
          (tuned-perfetto--mode-line-on)
          t)
      ((error quit)
       (tuned-perfetto--uninstall-all)
       (tuned-perfetto--mode-line-off)
       (setq tuned-perfetto--recording nil
             tuned-perfetto--async-open nil)
       (tuned-perfetto--reset-state)
       (signal (car error) (cdr error))))))

(defun tuned-perfetto--stop ()
  "Stop the recording, write it out and return the file, or nil.
The instrumentation is removed even when writing fails."
  (when tuned-perfetto--recording
    (let ((file nil))
      (unwind-protect
          (progn
            (tuned-perfetto--redisplay-frame-end t)
            (tuned-perfetto--uninstall-all)
            (tuned-perfetto--close-async-events)
            (tuned-perfetto--mode-line-off)
            (setq file (tuned-perfetto--write-file))
            file)
        (tuned-perfetto--remove-marker)
        (setq tuned-perfetto--recording nil)
        (tuned-perfetto--reset-state)))))

(defun tuned-perfetto--auto-stop ()
  "Stop the recording because its time limit was reached."
  (when tuned-perfetto--recording
    (let ((file (tuned-perfetto--stop)))
      (when file
        (message "tuned-perfetto: recording time is up, wrote %s (%d events)"
                 file tuned-perfetto--last-count)))))

;;; ============================================================
;;; Commands
;;; ============================================================

;;; ============================================================
;;; Following: other batch processes record while this one does
;;; ============================================================

(defun tuned-perfetto--read-marker ()
  "The recording marker as a plist, or nil when there is none or it is bad."
  (let ((file tuned-perfetto-marker-file))
    (when (and (stringp file) (file-regular-p file))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let ((info (read (current-buffer))))
            (and (stringp (plist-get info :dir))
                 (numberp (plist-get info :started))
                 (numberp (plist-get info :max))
                 info)))))))

(defun tuned-perfetto-follow-marker (prefix)
  "Record in this batch process while the main Emacs records.
Call it between units of work.  When the marker appears and is not past its
time limit, start a recording into the marker's folder, with file names that
start with PREFIX.  When the marker is gone or stale, stop and write the file.
Returns the file written on a stop, or nil.  Never signals: tracing must not
break the work it watches."
  (ignore-errors
    (setq tuned-perfetto--follower t
          tuned-perfetto--file-prefix prefix)
    (let* ((info (tuned-perfetto--read-marker))
           (left (and info (- (+ (plist-get info :started) (plist-get info :max))
                              (float-time)))))
      (cond
       ((and info (> left 0) (not tuned-perfetto--recording))
        (setq tuned-perfetto-trace-directory (plist-get info :dir))
        (tuned-perfetto--start (max 1 (ceiling left)))
        nil)
       ((and tuned-perfetto--recording (or (null info) (<= left 0)))
        (tuned-perfetto--stop))))))

(defun tuned-perfetto-follow-finish ()
  "Stop this process's following recording, write it and return the file."
  (when (and tuned-perfetto--follower tuned-perfetto--recording)
    (ignore-errors (tuned-perfetto--stop))))

;;;###autoload
(defun tuned-perfetto-trace-start (&optional max-seconds)
  "Start recording Emacs Lisp activity.
With a prefix argument, ask for the longest recording time in seconds
instead of using `tuned-perfetto-trace-max-seconds'.  Starting while a
recording runs only says so; it does not restart."
  (interactive
   (list (when current-prefix-arg
           (read-number "Longest recording (seconds): "
                        tuned-perfetto-trace-max-seconds))))
  (if tuned-perfetto--recording
      (message "tuned-perfetto: already recording")
    (let ((seconds (or max-seconds tuned-perfetto-trace-max-seconds)))
      (tuned-perfetto--start seconds)
      (message "tuned-perfetto: recording, stop with %s"
               (substitute-command-keys
                "\\[tuned-perfetto-trace-stop]")))))

;;;###autoload
(defun tuned-perfetto-trace-stop ()
  "Stop recording, remove all instrumentation and write the trace file.
Return the file name.  Does nothing but report when no recording runs."
  (interactive)
  (if (not tuned-perfetto--recording)
      (progn
        (message "tuned-perfetto: not recording")
        nil)
    (let ((file (tuned-perfetto--stop)))
      (when file
        (message "tuned-perfetto: wrote %s (%d events, %d dropped)"
                 file tuned-perfetto--last-count tuned-perfetto--last-dropped))
      file)))

;;;###autoload
(defun tuned-perfetto-trace-function (function)
  "Trace FUNCTION during this and every later recording.
A Lisp API, not a command: call it from your own code or config.  The
function gets an `:around' advice while recording.  C primitives are refused
unless native compilation is unavailable."
  (tuned-perfetto--check-traceable function)
  (add-to-list 'tuned-perfetto--traced-functions function)
  (if (not tuned-perfetto--recording)
      (message "tuned-perfetto: will trace %s in the next recording" function)
    (if (not (memq 'functions tuned-perfetto-trace-categories))
        (message "tuned-perfetto: %s registered, but the functions category is off"
                 function)
      (tuned-perfetto--install-function-advice function)
      (push (lambda () (tuned-perfetto--remove-function-advice function))
            tuned-perfetto--installed)
      (message "tuned-perfetto: tracing %s" function))))

;;;###autoload
(defun tuned-perfetto-untrace-function (function)
  "Stop tracing FUNCTION, now and in later recordings.  A Lisp API."
  (setq tuned-perfetto--traced-functions
        (delq function tuned-perfetto--traced-functions))
  (tuned-perfetto--remove-function-advice function)
  (message "tuned-perfetto: no longer tracing %s" function))

;;;###autoload
(defun tuned-perfetto-trace-mark (text)
  "Put a mark named TEXT on the timeline.
This is always recorded, whatever `tuned-perfetto-trace-categories' says."
  (interactive (list (read-string "Mark: ")))
  (if (not tuned-perfetto--recording)
      (message "tuned-perfetto: not recording; the mark was not recorded")
    (tuned-perfetto--record "i" text "marks" (tuned-perfetto--elapsed) nil 7 nil)
    (message "tuned-perfetto: mark %s" text)))

(defun tuned-perfetto--reveal-file (file)
  "Show FILE in a Dired buffer, in Emacs, without an external program."
  (let ((directory (file-name-directory file)))
    (dired directory)
    (when (fboundp 'dired-goto-file)
      (ignore-errors (dired-goto-file file)))))

(defun tuned-perfetto--latest-file ()
  "Return the most recent trace file, or nil."
  (or (and tuned-perfetto--last-file (file-exists-p tuned-perfetto--last-file)
           tuned-perfetto--last-file)
      (let ((files (file-expand-wildcards
                    (expand-file-name "emacs-trace-*.json"
                                      (tuned-perfetto--trace-directory)))))
        (car (sort files (lambda (first second)
                           (file-newer-than-file-p first second)))))))

;;;###autoload
(defun tuned-perfetto-trace-open ()
  "Open ui.perfetto.dev and show the latest trace file in Emacs.
Drag the file from the Dired buffer into the page."
  (interactive)
  (browse-url "https://ui.perfetto.dev")
  (if-let* ((file (tuned-perfetto--latest-file)))
      (progn
        (tuned-perfetto--reveal-file file)
        (message "tuned-perfetto: drag %s into ui.perfetto.dev" file))
    (message "tuned-perfetto: no trace file yet; record one first")))

(provide 'tuned-perfetto-trace)
;;; tuned-perfetto-trace.el ends here
