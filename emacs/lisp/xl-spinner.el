;;; xl-spinner.el --- One animated loader for any buffer  -*- lexical-binding: t; -*-

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

(require 'color) ; color-name-to-rgb, color-rgb-to-hex
(require 'seq)   ; seq-every-p, seq-mapn
(require 'face-remap) ; face-remap-add-relative, face-remap-remove-relative

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
                    " "))))
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
report its end."
  (xl-spinner-mode-line-install)
  (if xl-spinner--slot
      (let ((timer (plist-get xl-spinner--slot :hide-timer)))
        (when (timerp timer) (cancel-timer timer))
        (setq xl-spinner--slot (plist-put xl-spinner--slot :hide-timer nil))
        (xl-spinner-mode-line-label label))
    ;; A message left by the last stop (an error) goes with the new work.
    (setq xl-spinner-mode-line nil)
    (setq xl-spinner--slot (list :label label :width 0
                                 :limit (plist-get args :limit)))
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

(provide 'xl-spinner)
;;; xl-spinner.el ends here
