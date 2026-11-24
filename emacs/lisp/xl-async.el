;;; xl-async.el --- Owned, cancellable subprocesses, job queues, and one loader -*- lexical-binding: t; -*-

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

;; This file holds two layers, used together by the Git views, the Package
;; Menu, the Babel async blocks and xl-jit: the process layer (`xl-async-run',
;; `xl-async-enqueue') and the loader (`xl-spinner-start' and its mode-line
;; slot).  The loader is in the second half of the file.

;;; Requirements

(require 'seq)
(require 'color)      ; color-name-to-rgb, color-rgb-to-hex (loader tinting)
(require 'face-remap) ; face-remap-add-relative, face-remap-remove-relative

;;; ---- Processes: owned, cancellable, queued ----

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

;;; ---- The loader: one animated spinner for any buffer ----

;; Author: Xueliang Zhong
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; The Git status buffer, the GBlame window and the Package Menu all need a
;; loader that shows work is in progress without blocking, and without
;; making the screen jump.  This file has two layers.
;;
;; The core owns the clock and the frame counter; the caller owns the place:
;;
;;   (xl-spinner-start
;;    (lambda (text)                  ; nil when the loader stops
;;      (overlay-put my-overlay 'display text)))
;;
;; The mode-line slot is the place every loader of a buffer that already
;; shows content uses, because it never moves that content:
;;
;;   (xl-spinner-mode-line-install)   ; once, in the major mode
;;   (xl-spinner-mode-line-start "Pushing main")
;;   (xl-spinner-mode-line-label "Refreshing")
;;   (xl-spinner-mode-line-stop)      ; or (... "git blame failed: ...")
;;
;; What keeps the screen still:
;;
;; - Frames come from the mode line's own font.  A glyph the font lacks is
;;   drawn from a fallback font, and in a GUI that font's height makes the
;;   mode line taller: with JetBrains Mono, ⠋ (Apple Braille) and ▰ grew it
;;   from 23 to 24 pixels, so the whole window shifted each time the loader
;;   came and went.  `xl-spinner-default-frames' uses the dot wave only when
;;   the display draws it from that font, and ASCII otherwise.  A terminal
;;   (Emacs over ssh) draws in a fixed cell grid, so it only needs UTF-8.
;; - The slot appears after `xl-spinner-show-delay' and, once shown, stays
;;   `xl-spinner-min-duration', so a fast operation never flashes it.
;; - While busy the slot never gets narrower, so a label that changes from
;;   "Staging" to "Refreshing" does not push the mode line back and forth.
;; - The tint changes the mode line's background only, never its size.

;;; Code:


(defgroup xl-spinner nil
  "Animated loaders that do not move the screen."
  :group 'convenience)

(defcustom xl-spinner-style 'auto
  "Which frames loaders use.
`auto' uses the dot wave when the display can draw it from the mode
line's own font (or, in a terminal, over UTF-8), and ASCII otherwise.
`dots' and `ascii' force one of them, e.g. `ascii' for a terminal whose
font lacks the dots."
  :type '(choice (const :tag "Dots when safe, else ASCII" auto)
                 (const :tag "Always dots" dots)
                 (const :tag "Always ASCII" ascii))
  :group 'xl-spinner)

(defcustom xl-spinner-show-delay 0.1
  "Seconds before a mode-line loader appears.
Local git operations finish sooner, so the mode line stays still."
  :type 'number
  :group 'xl-spinner)

(defcustom xl-spinner-min-duration 0.3
  "Seconds a mode-line loader stays once it has appeared.
An operation that ends just after the delay would otherwise flash it."
  :type 'number
  :group 'xl-spinner)

(defcustom xl-spinner-tint 0.2
  "How far a busy mode line's background moves toward the loader colour.
0.0 keeps it, 1.0 makes it the colour of `xl-spinner'; nil disables the
tint.  Terminals with fewer than 256 colours get no tint."
  :type '(choice (const :tag "No tint" nil) number)
  :group 'xl-spinner)

(defface xl-spinner
  '((t :inherit font-lock-keyword-face))
  "Face of the loader frames, and the colour of the mode-line tint."
  :group 'xl-spinner)

(defvar xl-spinner-frames '("[|]" "[/]" "[-]" "[\\]")
  "ASCII frames, which every display draws.")

(defvar xl-spinner-dot-frames '("●··" "·●·" "··●" "·●·")
  "Dot-wave frames, used when the display draws them from its own font.
● and · are in JetBrains Mono, Menlo and Consolas, so the wave shows on
macOS, Windows and most Linux setups alike.")

(defvar xl-spinner-interval 0.1
  "Seconds between frames.
Ten frames a second reads as motion without flicker, and keeps the
redraws cheap over ssh.")

;; ============================================================
;; Frames
;; ============================================================

(defun xl-spinner--glyph-safe-p (char)
  "Return non-nil when CHAR keeps the mode line's size on this display."
  (and (= (char-width char) 1)
       (if (display-graphic-p)
           (let ((font (char-displayable-p char))
                 (own (face-attribute 'mode-line :family nil 'default)))
             (and (fontp font)
                  (stringp own)
                  (string-equal-ignore-case
                   (format "%s" (font-get font :family)) own)))
         (let ((coding (terminal-coding-system)))
           (and coding
                (eq (coding-system-type coding) 'utf-8)
                (char-displayable-p char))))))

(defun xl-spinner-default-frames ()
  "Return the frames loaders use on the selected frame's display.
See `xl-spinner-style'."
  (pcase xl-spinner-style
    ('ascii xl-spinner-frames)
    ('dots xl-spinner-dot-frames)
    (_ (if (seq-every-p #'xl-spinner--glyph-safe-p
                        (delete-dups (string-to-list
                                      (apply #'concat xl-spinner-dot-frames))))
           xl-spinner-dot-frames
         xl-spinner-frames))))


(defvar xl-spinner-two-dot-frames
  (if (seq-every-p #'xl-spinner--glyph-safe-p "·●")
      '("··" "●·" "·●")
    '(".." "*." ".*"))
  "Three frames of two dots, for a loader that has only two cells.
Dots when the font draws them, `..', `*.', `.*' otherwise.  Every frame
is exactly two cells wide, so text beside it never moves.")

(defvar xl-spinner-two-dot-interval 0.15
  "Seconds per frame of a two-dot loader.
Three frames of two dots need a longer frame than the four-frame dot wave
of `xl-spinner-interval' to read as steady motion.")

(defvar xl-spinner-one-dot-frames
  (if (seq-every-p #'xl-spinner--glyph-safe-p "·•●")
      '("·" "•" "●" "•")
    '("." "o" "O" "o"))
  "Four frames of one dot pulsing, for a loader that has only one cell.
Dots when the font draws them, ASCII otherwise.  Every frame is one cell.")

(defvar xl-spinner-one-dot-interval 0.15
  "Seconds per frame of a one-dot loader: a four-frame pulse, 0.6 s a turn.")

;; ============================================================
;; Core: clock and frame counter
;; ============================================================

(defvar-local xl-spinner--state nil
  "Loader state of this buffer, a plist, or nil when no loader runs.")

(defun xl-spinner-running-p (&optional buffer)
  "Return non-nil when a loader runs in BUFFER, defaulting to this buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (timerp (plist-get xl-spinner--state :timer))))))

(defun xl-spinner-start (render &rest args)
  "Animate a loader in this buffer, drawing it with RENDER.
RENDER is called with the current frame string, and with nil when the
loader stops, so it can both draw and clear the loader.  ARGS is a plist:

  :frames    frames to cycle, default `xl-spinner-default-frames'
  :interval  seconds per frame, default `xl-spinner-interval'
  :limit     stop after this many frames; nil runs until stopped

Frames are drawn in the `xl-spinner' face.  The first frame is drawn
before this function returns.  Return the state."
  (xl-spinner-stop)
  (let* ((frames (mapcar (lambda (frame) (propertize frame 'face 'xl-spinner))
                         (or (plist-get args :frames)
                             (xl-spinner-default-frames))))
         (interval (or (plist-get args :interval) xl-spinner-interval))
         (buffer (current-buffer)))
    (unless frames
      (user-error "A spinner needs at least one frame"))
    (let* ((state (list :render render
                        :frames frames
                        :interval interval
                        :frame 0
                        :ticks 0
                        :limit (plist-get args :limit)))
           timer)
      (setq xl-spinner--state state)
      (funcall render (nth 0 frames))
      ;; The timer holds the buffer, and cancels itself if the buffer dies
      ;; with no one stopping the loader first.
      (setq timer (run-with-timer interval interval
                                  (lambda ()
                                    (if (buffer-live-p buffer)
                                        (xl-spinner-tick buffer)
                                      (cancel-timer timer)))))
      (plist-put state :timer timer)
      state)))

(defun xl-spinner-stop (&optional buffer)
  "Stop the loader in BUFFER, defaulting to this buffer, and clear it.
RENDER is called with nil so the caller removes whatever it drew."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((state xl-spinner--state))
          (when state
            (let ((timer (plist-get state :timer)))
              (when (timerp timer) (cancel-timer timer)))
            (setq xl-spinner--state nil)
            (funcall (plist-get state :render) nil)))))))

(defun xl-spinner-current-frame (&optional buffer)
  "Return the frame BUFFER's loader shows now, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((state xl-spinner--state))
      (nth (plist-get state :frame) (plist-get state :frames)))))

(defun xl-spinner-tick (&optional buffer)
  "Advance the loader in BUFFER, defaulting to this buffer, by one frame.
Its timer calls this; tests call it directly to avoid waiting."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((state xl-spinner--state)
               (timer (plist-get state :timer)))
          (when (timerp timer)
            (let ((ticks (1+ (plist-get state :ticks)))
                  (limit (plist-get state :limit)))
              (plist-put state :ticks ticks)
              (if (and limit (> ticks limit))
                  (xl-spinner-stop)
                (let* ((frames (plist-get state :frames))
                       (frame (mod (1+ (plist-get state :frame))
                                   (length frames))))
                  (plist-put state :frame frame)
                  (funcall (plist-get state :render) (nth frame frames)))))))))))

;; ============================================================
;; Mode-line slot
;; ============================================================
;; The slot sits at the left edge of the mode line, where a narrow split
;; window does not truncate it (`mode-line-process', after the mode name,
;; is the first thing cut off).

(defvar-local xl-spinner-mode-line nil
  "Text of this buffer's mode-line slot, or nil when it is empty.")

(defvar-local xl-spinner--slot nil
  "State of this buffer's mode-line loader, a plist, or nil when idle.
Keys: :label, :width (the widest label of this busy period), :limit,
:shown-at, :show-timer, :hide-timer and :remaps (face remap cookies).")

(defconst xl-spinner--mode-line-construct '(:eval xl-spinner-mode-line)
  "The mode line element that draws the slot.")

(defun xl-spinner-mode-line-install ()
  "Put the loader slot at the left edge of this buffer's mode line."
  (unless (and (consp mode-line-format)
               (equal (car mode-line-format) xl-spinner--mode-line-construct))
    (setq-local mode-line-format
                (list xl-spinner--mode-line-construct
                      (if (local-variable-p 'mode-line-format)
                          mode-line-format
                        (default-value 'mode-line-format))))))

(defun xl-spinner--slot-draw (frame)
  "Draw FRAME and the current label in the slot; nil empties it."
  (if (null frame)
      (setq xl-spinner-mode-line nil)
    (let* ((label (or (plist-get xl-spinner--slot :label) ""))
           (width (max (string-width label)
                       (or (plist-get xl-spinner--slot :width) 0))))
      (setq xl-spinner--slot (plist-put xl-spinner--slot :width width))
      (setq xl-spinner-mode-line
            (concat " " frame " "
                    ;; The slot is a mode line construct: `%' is a directive.
                    (string-replace "%" "%%" label)
                    (make-string (- width (string-width label)) ?\s)
                    " ")))
    (when-let* ((also (plist-get xl-spinner--slot :also)))
      (funcall also frame)))
  (force-mode-line-update))

(defun xl-spinner--tint-color (face)
  "Return FACE's background moved toward the `xl-spinner' colour, or nil."
  (when (and xl-spinner-tint
             (or (display-graphic-p) (>= (display-color-cells) 256)))
    (let ((base (color-name-to-rgb (or (face-background face nil t) "")))
          (accent (color-name-to-rgb (or (face-foreground 'xl-spinner nil t) ""))))
      (when (and base accent)
        (apply #'color-rgb-to-hex
               (append (seq-mapn (lambda (b a)
                                   (+ b (* xl-spinner-tint (- a b))))
                                 base accent)
                       '(2)))))))

(defun xl-spinner--tint-on ()
  "Tint this buffer's mode lines; only the colour changes, not the size."
  (dolist (face '(mode-line-active mode-line-inactive))
    (when-let* ((color (xl-spinner--tint-color face)))
      (push (face-remap-add-relative face :background color)
            (plist-get xl-spinner--slot :remaps)))))

(defun xl-spinner--tint-off ()
  "Remove this buffer's mode-line tint."
  (mapc #'face-remap-remove-relative (plist-get xl-spinner--slot :remaps))
  (setq xl-spinner--slot (plist-put xl-spinner--slot :remaps nil)))

(defun xl-spinner--slot-show (buffer)
  "Make BUFFER's waiting mode-line loader visible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when xl-spinner--slot
        (setq xl-spinner--slot (plist-put xl-spinner--slot :show-timer nil))
        (setq xl-spinner--slot (plist-put xl-spinner--slot :shown-at (float-time)))
        (xl-spinner--tint-on)
        (xl-spinner-start #'xl-spinner--slot-draw
                          :limit (plist-get xl-spinner--slot :limit))))))

(defun xl-spinner--slot-hide (buffer)
  "Remove BUFFER's mode-line loader and its tint."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when xl-spinner--slot
        (dolist (key '(:show-timer :hide-timer))
          (let ((timer (plist-get xl-spinner--slot key)))
            (when (timerp timer) (cancel-timer timer))))
        (xl-spinner--tint-off)
        ;; The extra drawing goes with the slot: tell it before the slot,
        ;; and with it the function, is forgotten.
        (when-let* ((also (plist-get xl-spinner--slot :also)))
          (funcall also nil))
        ;; Only a loader that appeared owns the clock; one still waiting
        ;; must not stop another loader of this buffer.
        (let ((shown (plist-get xl-spinner--slot :shown-at)))
          (setq xl-spinner--slot nil)
          (when shown (xl-spinner-stop)))
        (setq xl-spinner-mode-line nil)
        (force-mode-line-update)))))

(defun xl-spinner-mode-line-start (label &rest args)
  "Show a loader with LABEL in this buffer's mode-line slot.
It appears after `xl-spinner-show-delay'.  Starting while a loader
runs, or while it lingers for `xl-spinner-min-duration', only changes
its label, so back-to-back operations keep one steady loader.  ARGS
may give :limit, the most frames to run, for work that might never
report its end, and :also, a function called with each frame drawn in
the slot, and with nil when the slot empties.  It draws the same loader
somewhere else in the buffer, on the slot's clock: a second spinner in
this buffer would stop the first."
  (xl-spinner-mode-line-install)
  (if xl-spinner--slot
      (let ((timer (plist-get xl-spinner--slot :hide-timer)))
        (when (timerp timer) (cancel-timer timer))
        (setq xl-spinner--slot (plist-put xl-spinner--slot :hide-timer nil))
        (when (plist-member args :also)
          (setq xl-spinner--slot
                (plist-put xl-spinner--slot :also (plist-get args :also))))
        (xl-spinner-mode-line-label label))
    ;; A message left by the last stop (an error) goes with the new work.
    (setq xl-spinner-mode-line nil)
    (setq xl-spinner--slot (list :label label :width 0
                                 :limit (plist-get args :limit)
                                 :also (plist-get args :also)))
    (if (<= xl-spinner-show-delay 0)
        (xl-spinner--slot-show (current-buffer))
      (setq xl-spinner--slot
            (plist-put xl-spinner--slot :show-timer
                       (run-with-timer xl-spinner-show-delay nil
                                       #'xl-spinner--slot-show
                                       (current-buffer)))))))

(defun xl-spinner-mode-line-label (label)
  "Change the label of this buffer's mode-line loader to LABEL."
  (when xl-spinner--slot
    (setq xl-spinner--slot (plist-put xl-spinner--slot :label label))
    (when-let* ((frame (and (plist-get xl-spinner--slot :shown-at)
                            (xl-spinner-current-frame))))
      (xl-spinner--slot-draw frame))))

(defun xl-spinner-mode-line-stop (&optional text)
  "End this buffer's mode-line loader.
A loader that never appeared ends silently; one that did stays until it
has been visible for `xl-spinner-min-duration'.  With TEXT, such as an
error, the slot shows TEXT at once instead, until the next start."
  (let ((buffer (current-buffer)))
    (cond
     (text
      (xl-spinner--slot-hide buffer)
      (setq xl-spinner-mode-line (concat " " (string-replace "%" "%%" text) " "))
      (force-mode-line-update))
     ((null xl-spinner--slot) nil)
     ((not (plist-get xl-spinner--slot :shown-at))
      (xl-spinner--slot-hide buffer))
     ((not (plist-get xl-spinner--slot :hide-timer))
      (let ((left (- (+ (plist-get xl-spinner--slot :shown-at)
                        xl-spinner-min-duration)
                     (float-time))))
        (if (<= left 0)
            (xl-spinner--slot-hide buffer)
          (setq xl-spinner--slot
                (plist-put xl-spinner--slot :hide-timer
                           (run-with-timer left nil #'xl-spinner--slot-hide
                                           buffer)))))))))

(defun xl-spinner-mode-line-busy-p (&optional buffer)
  "Return non-nil while BUFFER's mode-line loader waits, runs or lingers."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'xl-spinner--slot buffer)
         t)))

(provide 'xl-async)
(provide 'xl-spinner) ; the loader's old feature name, so older requires still work

;;; xl-async.el ends here
