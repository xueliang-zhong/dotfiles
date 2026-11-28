;;; tuned-perf-trace-tests.el --- ERT tests for tuned-perf-trace -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-perf-trace-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Each test records into its own temporary folder.  Tests that use timers
;; and processes wait for them, as the real recorder does.

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'tuned-perf-trace)
(require 'tuned-async)

;; The recorder only needs pyim's four entry points.  Replace them with
;; stubs even when the real package is installed: the test must not enter the
;; interactive input loop, whether it runs alone or with the profile's
;; packages activated.
(require 'pyim nil t)
(defvar pyim-process--candidates nil)
(defun pyim-input-method (&rest _) t)
(defun pyim-process-run () nil)
(defun pyim-page--refresh (&rest _) nil)
(defun pyim-select-word (&rest _) nil)
(provide 'pyim)

(defun xlpt--wait (pred &optional seconds)
  "Run the event loop until PRED returns non-nil, or SECONDS pass."
  (let ((end (+ (float-time) (or seconds 5))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defun xlpt--wait-idle (pred &optional seconds)
  "Wait for PRED while letting idle timers run, for at most SECONDS."
  (let ((end (+ (float-time) (or seconds 5))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (sit-for 0.02))
    (funcall pred)))

(defmacro xlpt--with-recording (var &rest body)
  "Run BODY while a recording writes into VAR, a temporary folder.
The recording is stopped and all instrumentation is removed afterwards,
even when BODY fails."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "tuned-perf-test-" t)))
          (tuned-perf-trace-directory ,var)
          (tuned-perf-marker-file (expand-file-name "recording.eld" ,var))
          (tuned-perf-trace-max-seconds 30)
          (tuned-perf-trace-ring-size 100000)
          (tuned-perf-trace-record-names t)
          (tuned-perf--traced-functions nil)
          (xlpt--saved-mode-string
           (and (boundp 'global-mode-string)
                (default-value 'global-mode-string))))
     (unwind-protect
         (progn ,@body)
       (when tuned-perf--recording
         (ignore-errors (tuned-perf--stop)))
       (tuned-perf--uninstall-all)
       (setq tuned-perf--recording nil)
       (tuned-perf--reset-state)
       (when (boundp 'global-mode-string)
         (set-default 'global-mode-string xlpt--saved-mode-string))
       (ignore-errors (delete-directory ,var t)))))

(defun xlpt--read (file)
  "Parse the trace FILE into hash tables."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-string (buffer-string) :object-type 'hash-table)))

(defun xlpt--events (trace)
  "Return the trace events of TRACE as a list."
  (append (gethash "traceEvents" trace) nil))

(defun xlpt--find (trace &rest fields)
  "Return the first event of TRACE whose FIELDS (key value ...) all match.
FIELD values are compared with `equal'."
  (seq-find (lambda (event)
              (cl-loop for (key value) on fields by #'cddr
                       always (equal value (gethash key event))))
            (xlpt--events trace)))

(defun xlpt--arg (event key)
  "Return KEY from EVENT's args object."
  (let ((args (gethash "args" event)))
    (and args (gethash key args))))

(defun xlpt--ring-event (ring-name)
  "Return the ring event named RING-NAME, or nil."
  (seq-find (lambda (event) (equal (aref event 1) ring-name))
            (tuned-perf--events)))

(defun xlpt--timer-ran ()
  "Marker a test timer sets when it runs."
  (setq xlpt--timer-ran t))

(defvar xlpt--timer-ran nil
  "Non-nil when the test timer has run.")

(defun xlpt--idle-timer-ran ()
  "Marker an idle test timer sets when it runs."
  (setq xlpt--idle-timer-ran t))

(defvar xlpt--idle-timer-ran nil
  "Non-nil when the idle test timer has run.")

(defun xlpt--boom ()
  "A function that raises an error, for tracing tests."
  (error "tuned-perf test boom"))

(defun xlpt--callee ()
  "A traced function called by another traced function.")

(defun xlpt--caller ()
  "A traced function that calls another traced function."
  (xlpt--callee))

(defun xlpt--span-error ()
  "Run a with-span body that raises, for the non-local exit test."
  (tuned-perf-with-span "span error"
    (error "span boom")))

;;; ============================================================
;;; Lifecycle
;;; ============================================================

(ert-deftest tuned-perf-trace-leaves-no-instrumentation ()
  "Starting and stopping restores hooks, advice, timers and process functions."
  (let ((pre (copy-sequence pre-command-hook))
        (post (copy-sequence post-command-hook))
        (gc (copy-sequence post-gc-hook))
        (redisplay (copy-sequence pre-redisplay-functions))
        (state (copy-sequence window-state-change-functions))
        (size (copy-sequence window-size-change-functions))
        (buffer-change (copy-sequence window-buffer-change-functions))
        (scroll (copy-sequence window-scroll-functions))
        (state-hook (copy-sequence window-state-change-hook))
        (buffer (generate-new-buffer " *tuned-perf-test-proc*"))
        process filter sentinel)
    (unwind-protect
        (progn
          (setq process (make-process
                         :name "tuned-perf-test-idle"
                         :command '("sleep" "5")
                         :buffer buffer
                         :filter (lambda (_process _chunk) nil)
                         :sentinel (lambda (_process _event) nil)))
          (setq filter (process-filter process)
                sentinel (process-sentinel process))
          (xlpt--with-recording _directory
            (tuned-perf-trace-start)
            (should tuned-perf--recording)
            ;; Starting again only reports; it does not restart.
            (let ((started tuned-perf--t0))
              (tuned-perf-trace-start)
              (should (= started tuned-perf--t0)))
            (should (tuned-perf-trace-stop))
            (should-not (tuned-perf-trace-stop)))
          (should (equal pre pre-command-hook))
          (should (equal post post-command-hook))
          (should (equal gc post-gc-hook))
          (should (equal redisplay pre-redisplay-functions))
          (should (equal state window-state-change-functions))
          (should (equal size window-size-change-functions))
          (should (equal buffer-change window-buffer-change-functions))
          (should (equal scroll window-scroll-functions))
          (should (equal state-hook window-state-change-hook))
          (should-not tuned-perf--frame)
          (should-not tuned-perf--frame-timer)
          (should-not tuned-perf--redisplay-phase)
          (should-not (advice-member-p #'tuned-perf--timer-advice
                                       'timer-event-handler))
          (should-not (advice-member-p #'tuned-perf--jit-lock-around
                                       'jit-lock-function))
          (should-not (advice-member-p #'tuned-perf--async-around
                                       'tuned-async-start))
          (should-not (advice-member-p #'tuned-perf--pyim-input-around
                                       'pyim-input-method))
          (should-not (advice-member-p #'tuned-perf--pyim-run-around
                                       'pyim-process-run))
          (should-not tuned-perf--process-scan-timer)
          (should-not tuned-perf--auto-stop-timer)
          (should (eq filter (process-filter process)))
          (should (eq sentinel (process-sentinel process))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest tuned-perf-trace-start-failure-rolls-back ()
  "A failure halfway through start removes what was already installed."
  (let ((pre (copy-sequence pre-command-hook))
        (post (copy-sequence post-command-hook)))
    (cl-letf (((symbol-function 'tuned-perf--install-auto-stop)
               (lambda (&rest _) (error "simulated start failure"))))
      (should-error (tuned-perf-trace-start) :type 'error))
    (should-not tuned-perf--recording)
    (should-not tuned-perf--installed)
    (should-not tuned-perf--ring)
    (should-not tuned-perf--process-scan-timer)
    (should-not tuned-perf--auto-stop-timer)
    (should (equal pre pre-command-hook))
    (should (equal post post-command-hook))
    (should-not (advice-member-p #'tuned-perf--timer-advice
                                 'timer-event-handler))))

(ert-deftest tuned-perf-trace-stops-itself-at-the-time-limit ()
  "The time limit stops the recording, writes the file and unwraps everything."
  (xlpt--with-recording directory
    (let ((tuned-perf-trace-max-seconds 0.2))
      (tuned-perf-trace-start)
      (should (xlpt--wait
               (lambda () (not tuned-perf--recording)) 3))
      (should tuned-perf--last-file)
      (should (file-exists-p tuned-perf--last-file))
      (should (string-prefix-p directory tuned-perf--last-file))
      (should-not (advice-member-p #'tuned-perf--timer-advice
                                   'timer-event-handler))
      (should-not tuned-perf--auto-stop-timer))))

;;; ============================================================
;;; Commands, timers, GC and processes
;;; ============================================================

(ert-deftest tuned-perf-trace-records-a-command-without-keys ()
  "A command becomes an X event named after it, with no key sequence."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (let ((this-command 'xlpt--command))
      (run-hooks 'pre-command-hook)
      (run-hooks 'post-command-hook))
    (let ((event (xlpt--ring-event "xlpt--command")))
      (should event)
      (should (equal "X" (aref event 0)))
      (should (>= (aref event 4) 0))
      (should (equal "fundamental-mode" (plist-get (aref event 6) :mode)))
      (should (plist-get (aref event 6) :buffer))
      (should-not (plist-member (aref event 6) :keys))
      (should-not (plist-member (aref event 6) :key)))
    (let* ((file (tuned-perf-trace-stop))
           (event (xlpt--find
                   (xlpt--read file)
                   "ph" "X" "name" "xlpt--command")))
      (should event)
      (should (equal "commands" (gethash "cat" event)))
      (should-not (gethash "keys" (gethash "args" event))))))

(ert-deftest tuned-perf-trace-records-timers-and-idle-timers ()
  "Named timers get X events, and idle timers say so."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (setq xlpt--timer-ran nil
          xlpt--idle-timer-ran nil)
    (run-at-time 0 nil #'xlpt--timer-ran)
    (should (xlpt--wait (lambda () xlpt--timer-ran) 3))
    ;; Batch Emacs never becomes idle, so an idle timer is fired directly;
    ;; its advice sees the idle delay just as it would in a real session.
    (let ((idle-timer (run-with-idle-timer 30 nil #'xlpt--idle-timer-ran)))
      (unwind-protect
          (progn
            (timer-event-handler idle-timer)
            (should xlpt--idle-timer-ran))
        (cancel-timer idle-timer)))
    (let ((event (xlpt--ring-event "xlpt--timer-ran"))
          (idle (xlpt--ring-event "xlpt--idle-timer-ran")))
      (should event)
      (should (equal "timers" (aref event 2)))
      (should-not (eq t (plist-get (aref event 6) :idle)))
      (should idle)
      (should (eq t (plist-get (aref idle 6) :idle))))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-records-garbage-collection ()
  "A garbage collection becomes an X event with a non-negative duration."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (garbage-collect)
    (let ((event (seq-find (lambda (event)
                             (equal "gc" (aref event 2)))
                           (tuned-perf--events))))
      (should event)
      (should (equal "gc" (aref event 1)))
      (should (>= (aref event 4) 0))
      (should (plist-get (aref event 6) :gcs-done)))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-records-process-filters ()
  "A process existing at start has its filter measured and restored."
  (xlpt--with-recording _directory
    (let* ((buffer (generate-new-buffer " *tuned-perf-test-out*"))
           (process (make-process
                     :name "tuned-perf-test-proc"
                     :command '("sh" "-c" "sleep 0.2; printf hi")
                     :buffer buffer
                     :filter #'ignore
                     :sentinel #'ignore))
           (filter (process-filter process))
           (sentinel (process-sentinel process)))
      (unwind-protect
          (progn
            (tuned-perf-trace-start)
            (should (xlpt--wait
                     (lambda () (not (process-live-p process))) 3))
            (let ((event (xlpt--ring-event
                          "filter tuned-perf-test-proc")))
              (should event)
              (should (= 2 (plist-get (aref event 6) :bytes)))
              (should-not (plist-member (aref event 6) :output))
              (should-not (plist-member (aref event 6) :chunk)))
            (tuned-perf-trace-stop)
            (should (eq filter (process-filter process)))
            (should (eq sentinel (process-sentinel process))))
        (when (process-live-p process) (delete-process process))
        (kill-buffer buffer)))))

(ert-deftest tuned-perf-trace-finds-processes-created-while-recording ()
  "The scan wraps a process created after the recording started."
  (xlpt--with-recording _directory
    (let ((tuned-perf-trace-process-scan-interval 0.05)
          (buffer (generate-new-buffer " *tuned-perf-test-new-out*"))
          process)
      (unwind-protect
          (progn
            (tuned-perf-trace-start)
            (setq process (make-process
                           :name "tuned-perf-test-new-proc"
                           :command '("sh" "-c" "sleep 0.3; printf hi")
                           :buffer buffer
                           :filter #'ignore
                           :sentinel #'ignore))
            ;; One scan runs before the process writes its output.
            (sit-for 0.15)
            (should (xlpt--wait
                     (lambda () (not (process-live-p process))) 3))
            (should (xlpt--ring-event
                     "filter tuned-perf-test-new-proc"))
            (tuned-perf-trace-stop))
        (when (process-live-p process) (delete-process process))
        (kill-buffer buffer)))))

(ert-deftest tuned-perf-trace-records-no-memory-counters ()
  "GC and the scan timer no longer write memory counter events."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (garbage-collect)
    (tuned-perf--scan)
    (should-not (xlpt--ring-event "memory"))
    (should-not (seq-find (lambda (event)
                            (equal "counters" (aref event 2)))
                          (tuned-perf--events)))
    (tuned-perf-trace-stop)))

;;; ============================================================
;;; Redisplay frames
;;; ============================================================

(ert-deftest tuned-perf-trace-records-a-redisplay-frame ()
  "A redisplay becomes a frame span with its windows, jank and a counter."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (run-hook-with-args 'pre-redisplay-functions (selected-window))
    (should tuned-perf--frame)
    (should (xlpt--wait-idle (lambda () (null tuned-perf--frame)) 3))
    (let ((frame (xlpt--ring-event "redisplay frame"))
          (counter (seq-find
                    (lambda (event)
                      (and (equal "C" (aref event 0))
                           (equal "redisplay frame" (aref event 1))))
                    (tuned-perf--events))))
      (should frame)
      (should (equal "redisplay" (aref frame 2)))
      (should (>= (aref frame 4) 0))
      (should (= 1 (plist-get (aref frame 6) :windows)))
      (should (plist-member (aref frame 6) :jank))
      (should counter)
      (should (numberp (plist-get (aref counter 6) :ms)))
      (should-not (plist-member (aref counter 6) :jank)))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-stop-closes-an-open-frame ()
  "Stopping while a frame is open records it once, marked cut."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (run-hook-with-args 'pre-redisplay-functions (selected-window))
    (should tuned-perf--frame)
    (let* ((file (tuned-perf-trace-stop))
           (event (xlpt--find (xlpt--read file)
                              "ph" "X" "name" "redisplay frame")))
      (should-not tuned-perf--frame)
      (should-not tuned-perf--frame-timer)
      (should event)
      (should (eq t (xlpt--arg event "cut")))
      (should (string-prefix-p directory file)))))

(ert-deftest tuned-perf-trace-coalesces-fast-frames ()
  "Frames closer than the minimum interval are merged into the next one."
  (xlpt--with-recording directory
    (let ((tuned-perf-trace-frame-min-interval 0.05))
      (tuned-perf-trace-start)
      ;; The first frame is always recorded.
      (run-hook-with-args 'pre-redisplay-functions (selected-window))
      (tuned-perf--redisplay-frame-end)
      ;; Three fast frames and their detail spans are coalesced away.
      (dotimes (_ 3)
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (run-hook-with-args 'window-scroll-functions (selected-window) 1)
        (tuned-perf--redisplay-frame-end))
      (should (= 1 tuned-perf--recorded-frame-count))
      (should (= 3 tuned-perf--coalesced-pending))
      (should-not (xlpt--ring-event "window-scroll"))
      ;; Pretend the last recorded frame was 100 ms ago, then record another.
      (setq tuned-perf--last-recorded-frame-start
            (- (tuned-perf--elapsed) 100000))
      (run-hook-with-args 'pre-redisplay-functions (selected-window))
      (run-hook-with-args 'window-scroll-functions (selected-window) 1)
      (tuned-perf--redisplay-frame-end)
      (let ((frames (seq-filter
                     (lambda (event)
                       (and (equal "X" (aref event 0))
                            (equal "redisplay frame" (aref event 1))))
                     (tuned-perf--events))))
        (should (= 2 (length frames)))
        (should (= 3 (plist-get (aref (nth 1 frames) 6) :coalesced)))
        (should (= 2 tuned-perf--recorded-frame-count))
        (should (= 3 tuned-perf--coalesced-count))
        (should (= 5 tuned-perf--frame-count))
        (should (xlpt--ring-event "window-scroll")))
      (let* ((file (tuned-perf-trace-stop))
             (metadata (gethash "metadata" (xlpt--read file))))
        (should (= 2 (gethash "redisplay-recorded-frames" metadata)))
        (should (= 3 (gethash "redisplay-coalesced-frames" metadata)))
        (should (string-prefix-p directory file))))))

(ert-deftest tuned-perf-trace-marks-each-redisplayed-window ()
  "Detail mode marks a window once per frame, and can be turned off."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (let ((window (selected-window)))
      (run-hook-with-args 'pre-redisplay-functions window)
      (run-hook-with-args 'pre-redisplay-functions window)
      (should (= 1 (tuned-perf--frame-windows tuned-perf--frame)))
      (tuned-perf--redisplay-frame-end)
      (let ((marks (seq-filter
                    (lambda (event)
                      (and (equal "i" (aref event 0))
                           (equal "redisplay" (aref event 2))))
                    (tuned-perf--events))))
        (should (= 1 (length marks)))
        (should (string-prefix-p "redisplay " (aref (car marks) 1)))
        (should (plist-get (aref (car marks) 6) :buffer))
        (should (eq t (plist-get (aref (car marks) 6) :selected)))
        (should (integerp (plist-get (aref (car marks) 6) :width)))))
    ;; With detail off, only the frame span remains.
    (let ((tuned-perf-trace-redisplay-detail nil))
      (run-hook-with-args 'pre-redisplay-functions (selected-window))
      (tuned-perf--redisplay-frame-end))
    (let ((marks (seq-filter
                  (lambda (event)
                    (and (equal "i" (aref event 0))
                         (equal "redisplay" (aref event 2))))
                  (tuned-perf--events))))
      (should (= 1 (length marks))))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-records-redisplay-phases ()
  "Window change and scroll hooks are spans inside the frame."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (run-hook-with-args 'window-size-change-functions (selected-frame))
    (run-hook-with-args 'window-scroll-functions (selected-window) 1)
    (tuned-perf--redisplay-frame-end)
    (let ((size (xlpt--ring-event "window-size-change"))
          (scroll (xlpt--ring-event "window-scroll"))
          (frame (xlpt--ring-event "redisplay frame")))
      (should size)
      (should (equal "redisplay" (aref size 2)))
      (should scroll)
      (should (plist-get (aref scroll 6) :buffer))
      (should (plist-get (aref frame 6) :resized))
      (should (plist-get (aref frame 6) :scrolled)))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-marks-a-janky-frame ()
  "A frame over the budget gets a `^' event, and the summary counts it."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (run-hook-with-args 'pre-redisplay-functions (selected-window))
    (setf (tuned-perf--frame-start tuned-perf--frame)
          (- (tuned-perf--elapsed) 50000))
    (tuned-perf--redisplay-frame-end)
    (should (= 1 tuned-perf--jank-count))
    (let* ((file (tuned-perf-trace-stop))
           (trace (xlpt--read file))
           (event (xlpt--find trace "ph" "X" "name" "redisplay frame"))
           (caret (xlpt--find trace "ph" "i" "name" "^"))
           (counter (xlpt--find trace "ph" "C" "name" "redisplay frame"))
           (metadata (gethash "metadata" trace)))
      (should (eq t (xlpt--arg event "jank")))
      (should caret)
      (should (= 5 (gethash "tid" caret)))
      (should (> (xlpt--arg caret "frame-ms") 16))
      (should-not (xlpt--arg counter "jank"))
      (should (= 1 (gethash "redisplay-jank-frames" metadata)))
      (should (> (gethash "redisplay-max-frame-ms" metadata) 16)))))

(ert-deftest tuned-perf-trace-measures-frame-latency ()
  "A frame records how long the key, the command and post-command took."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (let ((last-command-event 'return)
          (this-command 'xlpt--latency-command))
      (run-hooks 'pre-command-hook)
      (run-hooks 'post-command-hook))
    (run-hook-with-args 'pre-redisplay-functions (selected-window))
    (tuned-perf--redisplay-frame-end)
    (let ((frame (xlpt--ring-event "redisplay frame")))
      (should (plist-member (aref frame 6) :key-latency-ms))
      (should (plist-member (aref frame 6) :command-latency-ms))
      (should (plist-member (aref frame 6) :after-command-ms))
      (should (>= (plist-get (aref frame 6) :key-latency-ms) 0)))
    (tuned-perf-trace-stop)))

;;; ============================================================
;;; tuned-async and user-named functions
;;; ============================================================

(ert-deftest tuned-perf-trace-records-tuned-async-jobs ()
  "An tuned-async job has one b and one e event with the same id."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (with-temp-buffer
      (let (done)
        (tuned-async-run :command '("sh" "-c" "printf hi")
                      :name "tuned-perf-test-async"
                      :on-done (lambda (_output _code) (setq done t)))
        (should (xlpt--wait (lambda () done) 3))))
    (let* ((file (tuned-perf-trace-stop))
           (trace (xlpt--read file))
           (begin (xlpt--find trace "ph" "b" "cat" "tuned-async"))
           (end (xlpt--find trace "ph" "e" "cat" "tuned-async")))
      (should begin)
      (should end)
      (should (equal "tuned-perf-test-async" (gethash "name" begin)))
      (should (equal (gethash "id" begin) (gethash "id" end)))
      (should (equal 0 (xlpt--arg end "exit")))
      (should (= 2 (xlpt--arg end "bytes"))))))

(ert-deftest tuned-perf-trace-closes-a-cancelled-tuned-async-job ()
  "A cancelled job ends at once, with status cancelled."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (with-temp-buffer
      (tuned-async-run :command '("sleep" "5")
                    :name "tuned-perf-test-cancelled"
                    :on-done (lambda (&rest _) (error "on-done ran")))
      (tuned-async-cancel))
    (let* ((file (tuned-perf-trace-stop))
           (event (xlpt--find
                   (xlpt--read file)
                   "ph" "e" "name" "tuned-perf-test-cancelled")))
      (should event)
      (should (equal "cancelled" (xlpt--arg event "status"))))))

(ert-deftest tuned-perf-trace-user-function-end-and-errors-propagate ()
  "A traced function that raises still ends its span; the error propagates."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (tuned-perf-trace-function 'xlpt--boom)
    (should-error (xlpt--boom) :type 'error)
    (let ((event (xlpt--ring-event "xlpt--boom")))
      (should event)
      (should (>= (aref event 4) 0))
      (should (equal "functions" (aref event 2))))
    ;; A C primitive would need a synchronously compiled trampoline.
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (should-error (tuned-perf-trace-function 'make-process)
                    :type 'user-error))
    ;; The recorder never traces itself.
    (should-error (tuned-perf-trace-function 'tuned-perf-trace-start)
                  :type 'user-error)
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-nested-functions-do-not-recurse ()
  "Tracing a function that calls another traced function terminates."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (tuned-perf-trace-function 'xlpt--caller)
    (tuned-perf-trace-function 'xlpt--callee)
    (xlpt--caller)
    (let ((functions (seq-filter (lambda (event)
                                   (equal "functions" (aref event 2)))
                                 (tuned-perf--events))))
      (should (<= (length functions) 2))
      (should (xlpt--ring-event "xlpt--caller"))
      (should (xlpt--ring-event "xlpt--callee")))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-trace-with-span-records-and-ends-on-error ()
  "`tuned-perf-with-span' records a span, and an error still ends it."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (tuned-perf-with-span "span ok" (ignore))
    (should-error (xlpt--span-error) :type 'error)
    (let ((ok (xlpt--ring-event "span ok"))
          (bad (xlpt--ring-event "span error")))
      (should ok)
      (should (equal "functions" (aref ok 2)))
      (should (>= (aref ok 4) 0))
      (should bad)
      (should (>= (aref bad 4) 0)))
    (tuned-perf-trace-stop)))

(ert-deftest tuned-perf-advice-docstrings-name-their-targets ()
  "Every span advice the macro defines has a docstring naming its target."
  (require 'tuned-perf-probes)
  (dolist (pair '((tuned-perf--process-filter-around . "process-filter")
                  (tuned-perf--process-sentinel-around . "process-sentinel")
                  (tuned-perf--vertico-around . "vertico--update")
                  (tuned-perf--corfu-around . "corfu--update")
                  (tuned-perf--pyim-input-around . "pyim-input-method")
                  (tuned-perf--pyim-run-around . "pyim-process-run")
                  (tuned-perf--pyim-refresh-around . "pyim-page--refresh")
                  (tuned-perf--pyim-select-around . "pyim-select-word")
                  (tuned-perf--function-advice . "a user-named function")
                  (tuned-perf--mx-around . "execute-extended-command")))
    (let ((doc (documentation (car pair))))
      (should (and (stringp doc) (not (string-empty-p doc))))
      (should (string-match-p (regexp-quote (cdr pair)) doc)))))

;;; ============================================================
;;; Ring, output, names
;;; ============================================================

(ert-deftest tuned-perf-trace-ring-keeps-the-newest-events ()
  "A ring of 10 keeps the last 10 of 25 events and counts 15 dropped."
  (xlpt--with-recording directory
    (let ((tuned-perf-trace-ring-size 10))
      (tuned-perf-trace-start)
      (dotimes (index 25)
        (tuned-perf--record "X" (format "event-%d" index)
                             "functions" 0 1 6))
      (should (= 25 tuned-perf--count))
      (should (equal (mapcar (lambda (event) (aref event 1))
                             (tuned-perf--events))
                     (mapcar (lambda (index) (format "event-%d" index))
                             (number-sequence 15 24))))
      (let* ((file (tuned-perf-trace-stop))
             (trace (xlpt--read file)))
        (should (= 15 (gethash "dropped-events" (gethash "metadata" trace))))))))

(ert-deftest tuned-perf-trace-output-is-a-valid-trace ()
  "The file parses, has named tracks, and every event has ph, ts and pid."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (let ((this-command 'xlpt--output-command))
      (run-hooks 'pre-command-hook)
      (run-hooks 'post-command-hook))
    (garbage-collect)
    (let* ((file (tuned-perf-trace-stop))
           (trace (xlpt--read file))
           (events (xlpt--events trace)))
      (should (file-exists-p file))
      (should (equal "ms" (gethash "displayTimeUnit" trace)))
      (should (> (length events) 0))
      (dolist (event events)
        (should (gethash "ph" event))
        (should (gethash "ts" event))
        (should (gethash "pid" event))
        (when (equal "X" (gethash "ph" event))
          (should (>= (gethash "dur" event) 0))))
      (dolist (track '((1 . "commands") (2 . "timers") (3 . "processes")
                       (4 . "gc") (5 . "redisplay") (6 . "functions")
                       (7 . "marks") (8 . "modes") (9 . "completion")
                       (10 . "files") (11 . "input")))
        (let ((metadata (seq-find (lambda (event)
                                    (and (equal "M" (gethash "ph" event))
                                         (equal (car track)
                                                (gethash "tid" event))))
                                  events)))
          (should metadata)
          (should (equal (cdr track)
                         (gethash "name" (gethash "args" metadata))))))
      (let ((metadata (gethash "metadata" trace)))
        (should (gethash "emacs-version" metadata))
        (should (gethash "system-type" metadata))
        (should (gethash "started" metadata))
        (should (numberp (gethash "dropped-events" metadata)))))))

(ert-deftest tuned-perf-trace-records-pyim-input ()
  "A pyim composition shows on the input track, with its candidate count."
  (xlpt--with-recording _directory
    (tuned-perf-trace-start)
    (should (advice-member-p #'tuned-perf--pyim-input-around
                             'pyim-input-method))
    (should (advice-member-p #'tuned-perf--pyim-run-around
                             'pyim-process-run))
    (setq pyim-process--candidates '("a" "b" "c"))
    (pyim-input-method ?n)
    (pyim-process-run)
    (pyim-page--refresh)
    (pyim-select-word)
    (let ((event (xlpt--ring-event "pyim-process-run")))
      (should event)
      (should (equal "input" (aref event 2)))
      (should (= 11 (aref event 5)))
      (should (= 3 (plist-get (aref event 6) :candidates))))
    (dolist (name '("pyim-input-method" "pyim-page--refresh" "pyim-select-word"))
      (should (xlpt--ring-event name)))
    (tuned-perf-trace-stop)
    (should-not (advice-member-p #'tuned-perf--pyim-input-around
                                 'pyim-input-method))
    (should-not (advice-member-p #'tuned-perf--pyim-run-around
                                 'pyim-process-run))))

(ert-deftest tuned-perf-trace-record-names-off-omits-buffer-names ()
  "With `tuned-perf-trace-record-names' nil, no buffer name is recorded."
  (xlpt--with-recording directory
    (let ((tuned-perf-trace-record-names nil))
      (tuned-perf-trace-start)
      (let ((this-command 'xlpt--private-command))
        (run-hooks 'pre-command-hook)
        (run-hooks 'post-command-hook))
      (let* ((file (tuned-perf-trace-stop))
             (event (xlpt--find
                     (xlpt--read file)
                     "ph" "X" "name" "xlpt--private-command")))
        (should event)
        (should (equal "fundamental-mode" (xlpt--arg event "mode")))
        (should-not (xlpt--arg event "buffer"))
        (should-not (xlpt--arg event "file"))))))

;; Following: the marker the main Emacs writes, and the worker's side of it.
(ert-deftest tuned-perf-marker-is-written-while-recording ()
  "A recording writes the marker, naming its folder, and deletes it on stop."
  (xlpt--with-recording directory
    (tuned-perf-trace-start)
    (should (file-exists-p tuned-perf-marker-file))
    (let ((info (tuned-perf--read-marker)))
      (should (equal (plist-get info :dir) (tuned-perf--trace-directory)))
      (should (numberp (plist-get info :max))))
    (tuned-perf-trace-stop)
    (should-not (file-exists-p tuned-perf-marker-file))))

(ert-deftest tuned-perf-follower-records-while-the-marker-is-fresh ()
  "A follower starts when the marker appears and writes its file, named with
its prefix, when the marker goes; it signals nothing on a bad marker."
  (let* ((dir (file-name-as-directory (make-temp-file "xlpt-follow-" t)))
         (tuned-perf-marker-file (expand-file-name "recording.eld" dir))
         (tuned-perf-trace-directory dir)
         (tuned-perf--follower nil)
         (tuned-perf--file-prefix "emacs-trace")
         (tuned-perf--recording nil))
    (unwind-protect
        (progn
          (tuned-perf-follow-marker "follow-trace")
          (should-not tuned-perf--recording)
          ;; The main Emacs announces a recording.
          (with-temp-file tuned-perf-marker-file
            (prin1 (list :dir dir :started (float-time) :max 600) (current-buffer)))
          (tuned-perf-follow-marker "follow-trace")
          (should tuned-perf--recording)
          (should tuned-perf--follower)
          (should (equal tuned-perf--file-prefix "follow-trace"))
          ;; The main Emacs stops: the marker goes, the follower writes its file.
          (delete-file tuned-perf-marker-file)
          (let ((file (tuned-perf-follow-marker "follow-trace")))
            (should-not tuned-perf--recording)
            (should (string-prefix-p "follow-trace-" (file-name-nondirectory file)))
            (should (file-exists-p file)))
          ;; A marker past its time limit is stale: nothing starts.
          (with-temp-file tuned-perf-marker-file
            (prin1 (list :dir dir :started (- (float-time) 1000) :max 60) (current-buffer)))
          (tuned-perf-follow-marker "follow-trace")
          (should-not tuned-perf--recording)
          ;; A damaged marker signals nothing.
          (with-temp-file tuned-perf-marker-file (insert "(garbage"))
          (should-not (condition-case nil
                          (progn (tuned-perf-follow-marker "follow-trace") nil)
                        (error t)))
          (should-not tuned-perf--recording))
      (when tuned-perf--recording (tuned-perf--stop))
      (delete-directory dir t))))

(provide 'tuned-perf-trace-tests)
;;; tuned-perf-trace-tests.el ends here
