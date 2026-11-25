;;; xl-perfetto-trace-tests.el --- ERT tests for xl-perfetto-trace -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-perfetto-trace-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Each test records into its own temporary folder.  Tests that use timers
;; and processes wait for them, as the real recorder does.

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'xl-perfetto-trace)
(require 'xl-async)

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
  `(let* ((,var (file-name-as-directory (make-temp-file "xl-perfetto-test-" t)))
          (xl-perfetto-trace-directory ,var)
          (xl-perfetto-trace-max-seconds 30)
          (xl-perfetto-trace-ring-size 100000)
          (xl-perfetto-trace-record-names t)
          (xl-perfetto--traced-functions nil)
          (xlpt--saved-mode-string
           (and (boundp 'global-mode-string)
                (default-value 'global-mode-string))))
     (unwind-protect
         (progn ,@body)
       (when xl-perfetto--recording
         (ignore-errors (xl-perfetto--stop)))
       (xl-perfetto--uninstall-all)
       (setq xl-perfetto--recording nil)
       (xl-perfetto--reset-state)
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
            (xl-perfetto--events)))

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
  (error "xl-perfetto test boom"))

(defun xlpt--callee ()
  "A traced function called by another traced function.")

(defun xlpt--caller ()
  "A traced function that calls another traced function."
  (xlpt--callee))

(defun xlpt--span-error ()
  "Run a with-span body that raises, for the non-local exit test."
  (xl-perfetto-with-span "span error"
    (error "span boom")))

;;; ============================================================
;;; Lifecycle
;;; ============================================================

(ert-deftest xl-perfetto-trace-leaves-no-instrumentation ()
  "Starting and stopping restores hooks, advice, timers and process functions."
  (let ((pre (copy-sequence pre-command-hook))
        (post (copy-sequence post-command-hook))
        (gc (copy-sequence post-gc-hook))
        (redisplay (copy-sequence pre-redisplay-functions))
        (buffer (generate-new-buffer " *xl-perfetto-test-proc*"))
        process filter sentinel)
    (unwind-protect
        (progn
          (setq process (make-process
                         :name "xl-perfetto-test-idle"
                         :command '("sleep" "5")
                         :buffer buffer
                         :filter (lambda (_process _chunk) nil)
                         :sentinel (lambda (_process _event) nil)))
          (setq filter (process-filter process)
                sentinel (process-sentinel process))
          (xlpt--with-recording _directory
            (xl-perfetto-trace-start)
            (should xl-perfetto--recording)
            ;; Starting again only reports; it does not restart.
            (let ((started xl-perfetto--t0))
              (xl-perfetto-trace-start)
              (should (= started xl-perfetto--t0)))
            (should (xl-perfetto-trace-stop))
            (should-not (xl-perfetto-trace-stop)))
          (should (equal pre pre-command-hook))
          (should (equal post post-command-hook))
          (should (equal gc post-gc-hook))
          (should (equal redisplay pre-redisplay-functions))
          (should-not (advice-member-p #'xl-perfetto--timer-advice
                                       'timer-event-handler))
          (should-not (advice-member-p #'xl-perfetto--jit-lock-around
                                       'jit-lock-function))
          (should-not (advice-member-p #'xl-perfetto--async-around
                                       'xl-async-run))
          (should-not xl-perfetto--process-scan-timer)
          (should-not xl-perfetto--auto-stop-timer)
          (should (eq filter (process-filter process)))
          (should (eq sentinel (process-sentinel process))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest xl-perfetto-trace-start-failure-rolls-back ()
  "A failure halfway through start removes what was already installed."
  (let ((pre (copy-sequence pre-command-hook))
        (post (copy-sequence post-command-hook)))
    (cl-letf (((symbol-function 'xl-perfetto--install-auto-stop)
               (lambda (&rest _) (error "simulated start failure"))))
      (should-error (xl-perfetto-trace-start) :type 'error))
    (should-not xl-perfetto--recording)
    (should-not xl-perfetto--installed)
    (should-not xl-perfetto--ring)
    (should-not xl-perfetto--process-scan-timer)
    (should-not xl-perfetto--auto-stop-timer)
    (should (equal pre pre-command-hook))
    (should (equal post post-command-hook))
    (should-not (advice-member-p #'xl-perfetto--timer-advice
                                 'timer-event-handler))))

(ert-deftest xl-perfetto-trace-stops-itself-at-the-time-limit ()
  "The time limit stops the recording, writes the file and unwraps everything."
  (xlpt--with-recording directory
    (let ((xl-perfetto-trace-max-seconds 0.2))
      (xl-perfetto-trace-start)
      (should (xlpt--wait
               (lambda () (not xl-perfetto--recording)) 3))
      (should xl-perfetto--last-file)
      (should (file-exists-p xl-perfetto--last-file))
      (should (string-prefix-p directory xl-perfetto--last-file))
      (should-not (advice-member-p #'xl-perfetto--timer-advice
                                   'timer-event-handler))
      (should-not xl-perfetto--auto-stop-timer))))

;;; ============================================================
;;; Commands, timers, GC and processes
;;; ============================================================

(ert-deftest xl-perfetto-trace-records-a-command-without-keys ()
  "A command becomes an X event named after it, with no key sequence."
  (xlpt--with-recording directory
    (xl-perfetto-trace-start)
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
    (let* ((file (xl-perfetto-trace-stop))
           (event (xlpt--find
                   (xlpt--read file)
                   "ph" "X" "name" "xlpt--command")))
      (should event)
      (should (equal "commands" (gethash "cat" event)))
      (should-not (gethash "keys" (gethash "args" event))))))

(ert-deftest xl-perfetto-trace-records-timers-and-idle-timers ()
  "Named timers get X events, and idle timers say so."
  (xlpt--with-recording _directory
    (xl-perfetto-trace-start)
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
    (xl-perfetto-trace-stop)))

(ert-deftest xl-perfetto-trace-records-garbage-collection ()
  "A garbage collection becomes an X event with a non-negative duration."
  (xlpt--with-recording _directory
    (xl-perfetto-trace-start)
    (garbage-collect)
    (let ((event (seq-find (lambda (event)
                             (equal "gc" (aref event 2)))
                           (xl-perfetto--events))))
      (should event)
      (should (equal "gc" (aref event 1)))
      (should (>= (aref event 4) 0))
      (should (plist-get (aref event 6) :gcs-done)))
    (xl-perfetto-trace-stop)))

(ert-deftest xl-perfetto-trace-records-process-filters ()
  "A process existing at start has its filter measured and restored."
  (xlpt--with-recording _directory
    (let* ((buffer (generate-new-buffer " *xl-perfetto-test-out*"))
           (process (make-process
                     :name "xl-perfetto-test-proc"
                     :command '("sh" "-c" "sleep 0.2; printf hi")
                     :buffer buffer
                     :filter #'ignore
                     :sentinel #'ignore))
           (filter (process-filter process))
           (sentinel (process-sentinel process)))
      (unwind-protect
          (progn
            (xl-perfetto-trace-start)
            (should (xlpt--wait
                     (lambda () (not (process-live-p process))) 3))
            (let ((event (xlpt--ring-event
                          "filter xl-perfetto-test-proc")))
              (should event)
              (should (= 2 (plist-get (aref event 6) :bytes)))
              (should-not (plist-member (aref event 6) :output))
              (should-not (plist-member (aref event 6) :chunk)))
            (xl-perfetto-trace-stop)
            (should (eq filter (process-filter process)))
            (should (eq sentinel (process-sentinel process))))
        (when (process-live-p process) (delete-process process))
        (kill-buffer buffer)))))

(ert-deftest xl-perfetto-trace-finds-processes-created-while-recording ()
  "The scan wraps a process created after the recording started."
  (xlpt--with-recording _directory
    (let ((xl-perfetto-trace-process-scan-interval 0.05)
          (buffer (generate-new-buffer " *xl-perfetto-test-new-out*"))
          process)
      (unwind-protect
          (progn
            (xl-perfetto-trace-start)
            (setq process (make-process
                           :name "xl-perfetto-test-new-proc"
                           :command '("sh" "-c" "sleep 0.3; printf hi")
                           :buffer buffer
                           :filter #'ignore
                           :sentinel #'ignore))
            ;; One scan runs before the process writes its output.
            (sit-for 0.15)
            (should (xlpt--wait
                     (lambda () (not (process-live-p process))) 3))
            (should (xlpt--ring-event
                     "filter xl-perfetto-test-new-proc"))
            (xl-perfetto-trace-stop))
        (when (process-live-p process) (delete-process process))
        (kill-buffer buffer)))))

;;; ============================================================
;;; xl-async and user-named functions
;;; ============================================================

(ert-deftest xl-perfetto-trace-records-xl-async-jobs ()
  "An xl-async job has one b and one e event with the same id."
  (xlpt--with-recording directory
    (xl-perfetto-trace-start)
    (with-temp-buffer
      (let (done)
        (xl-async-run :command '("sh" "-c" "printf hi")
                      :name "xl-perfetto-test-async"
                      :on-done (lambda (_output _code) (setq done t)))
        (should (xlpt--wait (lambda () done) 3))))
    (let* ((file (xl-perfetto-trace-stop))
           (trace (xlpt--read file))
           (begin (xlpt--find trace "ph" "b" "cat" "xl-async"))
           (end (xlpt--find trace "ph" "e" "cat" "xl-async")))
      (should begin)
      (should end)
      (should (equal "xl-perfetto-test-async" (gethash "name" begin)))
      (should (equal (gethash "id" begin) (gethash "id" end)))
      (should (equal 0 (xlpt--arg end "exit")))
      (should (= 2 (xlpt--arg end "bytes"))))))

(ert-deftest xl-perfetto-trace-closes-a-cancelled-xl-async-job ()
  "A cancelled job has no e until stopping the recording closes it."
  (xlpt--with-recording directory
    (xl-perfetto-trace-start)
    (with-temp-buffer
      (xl-async-run :command '("sleep" "5")
                    :name "xl-perfetto-test-cancelled"
                    :on-done (lambda (&rest _) (error "on-done ran")))
      (xl-async-cancel))
    (let* ((file (xl-perfetto-trace-stop))
           (event (xlpt--find
                   (xlpt--read file)
                   "ph" "e" "name" "xl-perfetto-test-cancelled")))
      (should event)
      (should (eq t (xlpt--arg event "cancelled-or-running"))))))

(ert-deftest xl-perfetto-trace-user-function-end-and-errors-propagate ()
  "A traced function that raises still ends its span; the error propagates."
  (xlpt--with-recording _directory
    (xl-perfetto-trace-start)
    (xl-perfetto-trace-function 'xlpt--boom)
    (should-error (xlpt--boom) :type 'error)
    (let ((event (xlpt--ring-event "xlpt--boom")))
      (should event)
      (should (>= (aref event 4) 0))
      (should (equal "functions" (aref event 2))))
    ;; A C primitive would need a synchronously compiled trampoline.
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p)
               (not (and (fboundp 'eln-opt--trampoline-cached-p)
                         (eln-opt--trampoline-cached-p 'make-process))))
      (should-error (xl-perfetto-trace-function 'make-process)
                    :type 'user-error))
    ;; The recorder never traces itself.
    (should-error (xl-perfetto-trace-function 'xl-perfetto-trace-start)
                  :type 'user-error)
    (xl-perfetto-trace-stop)))

(ert-deftest xl-perfetto-trace-nested-functions-do-not-recurse ()
  "Tracing a function that calls another traced function terminates."
  (xlpt--with-recording _directory
    (xl-perfetto-trace-start)
    (xl-perfetto-trace-function 'xlpt--caller)
    (xl-perfetto-trace-function 'xlpt--callee)
    (xlpt--caller)
    (let ((functions (seq-filter (lambda (event)
                                   (equal "functions" (aref event 2)))
                                 (xl-perfetto--events))))
      (should (<= (length functions) 2))
      (should (xlpt--ring-event "xlpt--caller"))
      (should (xlpt--ring-event "xlpt--callee")))
    (xl-perfetto-trace-stop)))

(ert-deftest xl-perfetto-trace-with-span-records-and-ends-on-error ()
  "`xl-perfetto-with-span' records a span, and an error still ends it."
  (xlpt--with-recording _directory
    (xl-perfetto-trace-start)
    (xl-perfetto-with-span "span ok" (ignore))
    (should-error (xlpt--span-error) :type 'error)
    (let ((ok (xlpt--ring-event "span ok"))
          (bad (xlpt--ring-event "span error")))
      (should ok)
      (should (equal "functions" (aref ok 2)))
      (should (>= (aref ok 4) 0))
      (should bad)
      (should (>= (aref bad 4) 0)))
    (xl-perfetto-trace-stop)))

;;; ============================================================
;;; Ring, output, names
;;; ============================================================

(ert-deftest xl-perfetto-trace-ring-keeps-the-newest-events ()
  "A ring of 10 keeps the last 10 of 25 events and counts 15 dropped."
  (xlpt--with-recording directory
    (let ((xl-perfetto-trace-ring-size 10))
      (xl-perfetto-trace-start)
      (dotimes (index 25)
        (xl-perfetto--record "X" (format "event-%d" index)
                             "functions" 0 1 6))
      (should (= 25 xl-perfetto--count))
      (should (equal (mapcar (lambda (event) (aref event 1))
                             (xl-perfetto--events))
                     (mapcar (lambda (index) (format "event-%d" index))
                             (number-sequence 15 24))))
      (let* ((file (xl-perfetto-trace-stop))
             (trace (xlpt--read file)))
        (should (= 15 (gethash "dropped-events" (gethash "metadata" trace))))))))

(ert-deftest xl-perfetto-trace-output-is-a-valid-trace ()
  "The file parses, has named tracks, and every event has ph, ts and pid."
  (xlpt--with-recording directory
    (xl-perfetto-trace-start)
    (let ((this-command 'xlpt--output-command))
      (run-hooks 'pre-command-hook)
      (run-hooks 'post-command-hook))
    (garbage-collect)
    (let* ((file (xl-perfetto-trace-stop))
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
                       (7 . "marks")))
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

(ert-deftest xl-perfetto-trace-record-names-off-omits-buffer-names ()
  "With `xl-perfetto-trace-record-names' nil, no buffer name is recorded."
  (xlpt--with-recording directory
    (let ((xl-perfetto-trace-record-names nil))
      (xl-perfetto-trace-start)
      (let ((this-command 'xlpt--private-command))
        (run-hooks 'pre-command-hook)
        (run-hooks 'post-command-hook))
      (let* ((file (xl-perfetto-trace-stop))
             (event (xlpt--find
                     (xlpt--read file)
                     "ph" "X" "name" "xlpt--private-command")))
        (should event)
        (should (equal "fundamental-mode" (xlpt--arg event "mode")))
        (should-not (xlpt--arg event "buffer"))
        (should-not (xlpt--arg event "file"))))))

(provide 'xl-perfetto-trace-tests)
;;; xl-perfetto-trace-tests.el ends here
