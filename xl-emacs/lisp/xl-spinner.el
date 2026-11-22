;;; xl-spinner.el --- One animated ASCII loader for any buffer  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; The Git status buffer, the GBlame window and the Package Menu all needed
;; the same thing: a loader that shows work is in progress without blocking.
;; They differ only in where the frame is drawn - an overlay in the body, a
;; header line, a mode line slot - so this file owns the clock and the frame
;; counter, and the caller owns the place:
;;
;;   (xl-spinner-start
;;    (lambda (text)
;;      (if text
;;          (setq-local header-line-format (concat " " text " working"))
;;        (setq-local header-line-format nil))))
;;
;; RENDER is called with the current frame string, and once with nil when
;; the loader stops, so one function both draws and clears it.  A loader
;; whose buffer is killed cancels its own timer.

;;; Code:

(defvar xl-spinner-frames '("[|]" "[/]" "[-]" "[\\]")
  "ASCII frames of the animated loader.")

(defvar xl-spinner-interval 0.05
  "Seconds between frames of the loader.
20 frames per second reads as motion; much faster blurs the ASCII frames.")

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

  :frames    frames to cycle, default `xl-spinner-frames'
  :interval  seconds per frame, default `xl-spinner-interval'
  :limit     stop after this many frames; nil runs until stopped

The first frame is drawn before this function returns.  Return the state."
  (xl-spinner-stop)
  (let* ((frames (or (plist-get args :frames) xl-spinner-frames))
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

(provide 'xl-spinner)
;;; xl-spinner.el ends here
