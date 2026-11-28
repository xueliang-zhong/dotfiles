;;; tuned-spinner.el --- Animated loaders that do not move the screen -*- lexical-binding: t; -*-

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
;;   (tuned-spinner-start
;;    (lambda (text)                  ; nil when the loader stops
;;      (overlay-put my-overlay 'display text)))
;;
;; The mode-line slot is the place every loader of a buffer that already
;; shows content uses, because it never moves that content:
;;
;;   (tuned-spinner-mode-line-install)   ; once, in the major mode
;;   (tuned-spinner-mode-line-start "Pushing main")
;;   (tuned-spinner-mode-line-label "Refreshing")
;;   (tuned-spinner-mode-line-stop)      ; or (... "git blame failed: ...")
;;
;; What keeps the screen still:
;;
;; - Frames come from the mode line's own font.  A glyph the font lacks is
;;   drawn from a fallback font, and in a GUI that font's height makes the
;;   mode line taller: with JetBrains Mono, ⠋ (Apple Braille) and ▰ grew it
;;   from 23 to 24 pixels, so the whole window shifted each time the loader
;;   came and went.  `tuned-spinner-default-frames' uses the dot wave only when
;;   the display draws it from that font, and ASCII otherwise.  A terminal
;;   (Emacs over ssh) draws in a fixed cell grid, so it only needs UTF-8.
;; - The slot appears after `tuned-spinner-show-delay' and, once shown, stays
;;   `tuned-spinner-min-duration', so a fast operation never flashes it.
;; - While busy the slot never gets narrower, so a label that changes from
;;   "Staging" to "Refreshing" does not push the mode line back and forth.
;; - The tint changes the mode line's background only, never its size.

;;; Code:

(require 'seq)
(require 'color)      ; color-name-to-rgb, color-rgb-to-hex (loader tinting)
(require 'face-remap) ; face-remap-add-relative, face-remap-remove-relative

(defgroup tuned-spinner nil
  "Animated loaders that do not move the screen."
  :group 'convenience)

(defcustom tuned-spinner-style 'auto
  "Which frames loaders use.
`auto' uses the dot wave when the display can draw it from the mode
line's own font (or, in a terminal, over UTF-8), and ASCII otherwise.
`dots' and `ascii' force one of them, e.g. `ascii' for a terminal whose
font lacks the dots."
  :type '(choice (const :tag "Dots when safe, else ASCII" auto)
                 (const :tag "Always dots" dots)
                 (const :tag "Always ASCII" ascii))
  :group 'tuned-spinner)

(defcustom tuned-spinner-show-delay 0.1
  "Seconds before a mode-line loader appears.
Local git operations finish sooner, so the mode line stays still."
  :type 'number
  :group 'tuned-spinner)

(defcustom tuned-spinner-min-duration 0.3
  "Seconds a mode-line loader stays once it has appeared.
An operation that ends just after the delay would otherwise flash it."
  :type 'number
  :group 'tuned-spinner)

(defcustom tuned-spinner-tint 0.2
  "How far a busy mode line's background moves toward the loader colour.
0.0 keeps it, 1.0 makes it the colour of `tuned-spinner'; nil disables the
tint.  Terminals with fewer than 256 colours get no tint."
  :type '(choice (const :tag "No tint" nil) number)
  :group 'tuned-spinner)

(defface tuned-spinner
  '((t :inherit font-lock-keyword-face))
  "Face of the loader frames, and the colour of the mode-line tint."
  :group 'tuned-spinner)

(defvar tuned-spinner-frames '("[|]" "[/]" "[-]" "[\\]")
  "ASCII frames, which every display draws.")

(defvar tuned-spinner-dot-frames '("●··" "·●·" "··●" "·●·")
  "Dot-wave frames, used when the display draws them from its own font.
● and · are in JetBrains Mono, Menlo and Consolas, so the wave shows on
macOS, Windows and most Linux setups alike.")

(defvar tuned-spinner-interval 0.1
  "Seconds between frames.
Ten frames a second reads as motion without flicker, and keeps the
redraws cheap over ssh.")

;; ============================================================
;; Frames
;; ============================================================

(defun tuned-spinner--glyph-safe-p (char)
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

(defun tuned-spinner-default-frames ()
  "Return the frames loaders use on the selected frame's display.
See `tuned-spinner-style'."
  (pcase tuned-spinner-style
    ('ascii tuned-spinner-frames)
    ('dots tuned-spinner-dot-frames)
    (_ (if (seq-every-p #'tuned-spinner--glyph-safe-p
                        (delete-dups (string-to-list
                                      (apply #'concat tuned-spinner-dot-frames))))
           tuned-spinner-dot-frames
         tuned-spinner-frames))))


(defvar tuned-spinner-two-dot-frames
  (if (seq-every-p #'tuned-spinner--glyph-safe-p "·●")
      '("··" "●·" "·●")
    '(".." "*." ".*"))
  "Three frames of two dots, for a loader that has only two cells.
Dots when the font draws them, `..', `*.', `.*' otherwise.  Every frame
is exactly two cells wide, so text beside it never moves.")

(defvar tuned-spinner-two-dot-interval 0.15
  "Seconds per frame of a two-dot loader.
Three frames of two dots need a longer frame than the four-frame dot wave
of `tuned-spinner-interval' to read as steady motion.")

(defvar tuned-spinner-one-dot-frames
  (if (seq-every-p #'tuned-spinner--glyph-safe-p "·•●")
      '("·" "•" "●" "•")
    '("." "o" "O" "o"))
  "Four frames of one dot pulsing, for a loader that has only one cell.
Dots when the font draws them, ASCII otherwise.  Every frame is one cell.")

(defvar tuned-spinner-one-dot-interval 0.15
  "Seconds per frame of a one-dot loader: a four-frame pulse, 0.6 s a turn.")

;; ============================================================
;; Core: clock and frame counter
;; ============================================================

(defvar-local tuned-spinner--state nil
  "Loader state of this buffer, a plist, or nil when no loader runs.")

(defun tuned-spinner-running-p (&optional buffer)
  "Return non-nil when a loader runs in BUFFER, defaulting to this buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (timerp (plist-get tuned-spinner--state :timer))))))

(defun tuned-spinner-start (render &rest args)
  "Animate a loader in this buffer, drawing it with RENDER.
RENDER is called with the current frame string, and with nil when the
loader stops, so it can both draw and clear the loader.  ARGS is a plist:

  :frames    frames to cycle, default `tuned-spinner-default-frames'
  :interval  seconds per frame, default `tuned-spinner-interval'
  :limit     stop after this many frames; nil runs until stopped

Frames are drawn in the `tuned-spinner' face.  The first frame is drawn
before this function returns.  Return the state."
  (tuned-spinner-stop)
  (let* ((frames (mapcar (lambda (frame) (propertize frame 'face 'tuned-spinner))
                         (or (plist-get args :frames)
                             (tuned-spinner-default-frames))))
         (interval (or (plist-get args :interval) tuned-spinner-interval))
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
      (setq tuned-spinner--state state)
      (funcall render (nth 0 frames))
      ;; The timer holds the buffer, and cancels itself if the buffer dies
      ;; with no one stopping the loader first.
      (setq timer (run-with-timer interval interval
                                  (lambda ()
                                    (if (buffer-live-p buffer)
                                        (tuned-spinner-tick buffer)
                                      (cancel-timer timer)))))
      (plist-put state :timer timer)
      state)))

(defun tuned-spinner-stop (&optional buffer)
  "Stop the loader in BUFFER, defaulting to this buffer, and clear it.
RENDER is called with nil so the caller removes whatever it drew."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((state tuned-spinner--state))
          (when state
            (let ((timer (plist-get state :timer)))
              (when (timerp timer) (cancel-timer timer)))
            (setq tuned-spinner--state nil)
            (funcall (plist-get state :render) nil)))))))

(defun tuned-spinner-current-frame (&optional buffer)
  "Return the frame BUFFER's loader shows now, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((state tuned-spinner--state))
      (nth (plist-get state :frame) (plist-get state :frames)))))

(defun tuned-spinner-tick (&optional buffer)
  "Advance the loader in BUFFER, defaulting to this buffer, by one frame.
Its timer calls this; tests call it directly to avoid waiting."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((state tuned-spinner--state)
               (timer (plist-get state :timer)))
          (when (timerp timer)
            (let ((ticks (1+ (plist-get state :ticks)))
                  (limit (plist-get state :limit)))
              (plist-put state :ticks ticks)
              (if (and limit (> ticks limit))
                  (tuned-spinner-stop)
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

(defvar-local tuned-spinner-mode-line nil
  "Text of this buffer's mode-line slot, or nil when it is empty.")

(defvar-local tuned-spinner--slot nil
  "State of this buffer's mode-line loader, a plist, or nil when idle.
Keys: :label, :width (the widest label of this busy period), :limit,
:shown-at, :show-timer, :hide-timer and :remaps (face remap cookies).")

(defconst tuned-spinner--mode-line-construct '(:eval tuned-spinner-mode-line)
  "The mode line element that draws the slot.")

(defun tuned-spinner-mode-line-install ()
  "Put the loader slot at the left edge of this buffer's mode line."
  (unless (and (consp mode-line-format)
               (equal (car mode-line-format) tuned-spinner--mode-line-construct))
    (setq-local mode-line-format
                (list tuned-spinner--mode-line-construct
                      (if (local-variable-p 'mode-line-format)
                          mode-line-format
                        (default-value 'mode-line-format))))))

(defun tuned-spinner--slot-draw (frame)
  "Draw FRAME and the current label in the slot; nil empties it."
  (if (null frame)
      (setq tuned-spinner-mode-line nil)
    (let* ((label (or (plist-get tuned-spinner--slot :label) ""))
           (width (max (string-width label)
                       (or (plist-get tuned-spinner--slot :width) 0))))
      (setq tuned-spinner--slot (plist-put tuned-spinner--slot :width width))
      (setq tuned-spinner-mode-line
            (concat " " frame " "
                    ;; The slot is a mode line construct: `%' is a directive.
                    (string-replace "%" "%%" label)
                    (make-string (- width (string-width label)) ?\s)
                    " ")))
    (when-let* ((also (plist-get tuned-spinner--slot :also)))
      (funcall also frame)))
  (force-mode-line-update))

(defun tuned-spinner--tint-color (face)
  "Return FACE's background moved toward the `tuned-spinner' colour, or nil."
  (when (and tuned-spinner-tint
             (or (display-graphic-p) (>= (display-color-cells) 256)))
    (let ((base (color-name-to-rgb (or (face-background face nil t) "")))
          (accent (color-name-to-rgb (or (face-foreground 'tuned-spinner nil t) ""))))
      (when (and base accent)
        (apply #'color-rgb-to-hex
               (append (seq-mapn (lambda (b a)
                                   (+ b (* tuned-spinner-tint (- a b))))
                                 base accent)
                       '(2)))))))

(defun tuned-spinner--tint-on ()
  "Tint this buffer's mode lines; only the colour changes, not the size."
  (dolist (face '(mode-line-active mode-line-inactive))
    (when-let* ((color (tuned-spinner--tint-color face)))
      (push (face-remap-add-relative face :background color)
            (plist-get tuned-spinner--slot :remaps)))))

(defun tuned-spinner--tint-off ()
  "Remove this buffer's mode-line tint."
  (mapc #'face-remap-remove-relative (plist-get tuned-spinner--slot :remaps))
  (setq tuned-spinner--slot (plist-put tuned-spinner--slot :remaps nil)))

(defun tuned-spinner--slot-show (buffer)
  "Make BUFFER's waiting mode-line loader visible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when tuned-spinner--slot
        (setq tuned-spinner--slot (plist-put tuned-spinner--slot :show-timer nil))
        (setq tuned-spinner--slot (plist-put tuned-spinner--slot :shown-at (float-time)))
        (tuned-spinner--tint-on)
        (tuned-spinner-start #'tuned-spinner--slot-draw
                          :limit (plist-get tuned-spinner--slot :limit))))))

(defun tuned-spinner--slot-hide (buffer)
  "Remove BUFFER's mode-line loader and its tint."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when tuned-spinner--slot
        (dolist (key '(:show-timer :hide-timer))
          (let ((timer (plist-get tuned-spinner--slot key)))
            (when (timerp timer) (cancel-timer timer))))
        (tuned-spinner--tint-off)
        ;; The extra drawing goes with the slot: tell it before the slot,
        ;; and with it the function, is forgotten.
        (when-let* ((also (plist-get tuned-spinner--slot :also)))
          (funcall also nil))
        ;; Only a loader that appeared owns the clock; one still waiting
        ;; must not stop another loader of this buffer.
        (let ((shown (plist-get tuned-spinner--slot :shown-at)))
          (setq tuned-spinner--slot nil)
          (when shown (tuned-spinner-stop)))
        (setq tuned-spinner-mode-line nil)
        (force-mode-line-update)))))

(defun tuned-spinner-mode-line-start (label &rest args)
  "Show a loader with LABEL in this buffer's mode-line slot.
It appears after `tuned-spinner-show-delay'.  Starting while a loader
runs, or while it lingers for `tuned-spinner-min-duration', only changes
its label, so back-to-back operations keep one steady loader.  ARGS
may give :limit, the most frames to run, for work that might never
report its end, and :also, a function called with each frame drawn in
the slot, and with nil when the slot empties.  It draws the same loader
somewhere else in the buffer, on the slot's clock: a second spinner in
this buffer would stop the first."
  (tuned-spinner-mode-line-install)
  (if tuned-spinner--slot
      (let ((timer (plist-get tuned-spinner--slot :hide-timer)))
        (when (timerp timer) (cancel-timer timer))
        (setq tuned-spinner--slot (plist-put tuned-spinner--slot :hide-timer nil))
        (when (plist-member args :also)
          (setq tuned-spinner--slot
                (plist-put tuned-spinner--slot :also (plist-get args :also))))
        (tuned-spinner-mode-line-label label))
    ;; A message left by the last stop (an error) goes with the new work.
    (setq tuned-spinner-mode-line nil)
    (setq tuned-spinner--slot (list :label label :width 0
                                 :limit (plist-get args :limit)
                                 :also (plist-get args :also)))
    (if (<= tuned-spinner-show-delay 0)
        (tuned-spinner--slot-show (current-buffer))
      (setq tuned-spinner--slot
            (plist-put tuned-spinner--slot :show-timer
                       (run-with-timer tuned-spinner-show-delay nil
                                       #'tuned-spinner--slot-show
                                       (current-buffer)))))))

(defun tuned-spinner-mode-line-label (label)
  "Change the label of this buffer's mode-line loader to LABEL."
  (when tuned-spinner--slot
    (setq tuned-spinner--slot (plist-put tuned-spinner--slot :label label))
    (when-let* ((frame (and (plist-get tuned-spinner--slot :shown-at)
                            (tuned-spinner-current-frame))))
      (tuned-spinner--slot-draw frame))))

(defun tuned-spinner-mode-line-stop (&optional text)
  "End this buffer's mode-line loader.
A loader that never appeared ends silently; one that did stays until it
has been visible for `tuned-spinner-min-duration'.  With TEXT, such as an
error, the slot shows TEXT at once instead, until the next start."
  (let ((buffer (current-buffer)))
    (cond
     (text
      (tuned-spinner--slot-hide buffer)
      (setq tuned-spinner-mode-line (concat " " (string-replace "%" "%%" text) " "))
      (force-mode-line-update))
     ((null tuned-spinner--slot) nil)
     ((not (plist-get tuned-spinner--slot :shown-at))
      (tuned-spinner--slot-hide buffer))
     ((not (plist-get tuned-spinner--slot :hide-timer))
      (let ((left (- (+ (plist-get tuned-spinner--slot :shown-at)
                        tuned-spinner-min-duration)
                     (float-time))))
        (if (<= left 0)
            (tuned-spinner--slot-hide buffer)
          (setq tuned-spinner--slot
                (plist-put tuned-spinner--slot :hide-timer
                           (run-with-timer left nil #'tuned-spinner--slot-hide
                                           buffer)))))))))

(defun tuned-spinner-mode-line-busy-p (&optional buffer)
  "Return non-nil while BUFFER's mode-line loader waits, runs or lingers."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'tuned-spinner--slot buffer)
         t)))

(provide 'tuned-spinner)
;;; tuned-spinner.el ends here
