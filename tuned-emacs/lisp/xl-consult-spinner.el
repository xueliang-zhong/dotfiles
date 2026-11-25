;;; xl-consult-spinner.el --- Animate consult's async indicator  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (consult "3.10") (xl-spinner "0.2.0"))
;; Keywords: convenience, completion

;;; Commentary:
;; consult shows the state of an asynchronous candidate list (consult-ripgrep,
;; consult-find, ...) as one character in front of the minibuffer prompt: `*'
;; while the process runs, `:' when it finishes.  This file animates that
;; character while the process runs, with the same clock and frames as the
;; other loaders of this config.
;;
;; It wraps `consult--async-indicator', the pipeline element consult puts in
;; every asynchronous source.  On `[indicator running]' it starts a spinner;
;; on the end states it stops the spinner and then lets consult draw its own
;; final character, so the static state is what remains.
;;
;; The frames are one cell, like consult's own character, so the typed text
;; never moves.

;;; Code:

(require 'cl-lib)
(require 'xl-async)

(declare-function consult--async-indicator "consult")

(defconst xl-consult-spinner-idle ":"
  "What the one cell shows when no search runs: consult's colon.")

(defun xl-consult-spinner--overlay ()
  "Return the indicator overlay consult put in front of the minibuffer prompt."
  (cl-find-if (lambda (ov)
                (eq (overlay-get ov 'category) 'consult-async-indicator-overlay))
              (overlays-at (max (point-min) (- (minibuffer-prompt-end) 2)))))

(defun xl-consult-spinner--draw (overlay frame)
  "Show FRAME on OVERLAY; nil leaves it alone.
FRAME keeps the `xl-spinner' face, so it is the colour of the mode-line
loaders."
  (when (and frame (overlayp overlay) (overlay-buffer overlay))
    (overlay-put overlay 'display frame)))

(defun xl-consult-spinner--idle (overlay)
  "Show the idle text on OVERLAY, one cell wide like the frames."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (overlay-put overlay 'display xl-consult-spinner-idle)))

(defun xl-consult-spinner--around-indicator (orig)
  "Return ORIG's indicator, with its running state animated.
ORIG is `consult--async-indicator', which takes no arguments and returns
the pipeline element, a function of SINK."
  (let ((inner (funcall orig)))
    (lambda (sink)
      (xl-consult-spinner--wrap-handler (funcall inner sink)))))

(defun xl-consult-spinner--wrap-handler (next)
  "Return the handler that animates the indicator, forwarding to NEXT.
Until NEXT has seen `setup', it has no indicator overlay, and consult's
handler fails on `destroy' or on an indicator state (\"Wrong type argument:
overlayp, nil\").  So those actions are not forwarded before setup, and
are forwarded only once.  Other actions always pass through."
  (let (overlay ready)
    (lambda (action)
      (pcase action
        ('setup
         (funcall next action)
         (setq ready t)
         (setq overlay (xl-consult-spinner--overlay))
         (xl-consult-spinner--idle overlay))
        ('destroy
         (when (xl-spinner-running-p)
           (xl-spinner-stop))
         (when ready
           (setq ready nil)
           (funcall next action)))
        (`[indicator running]
         (when ready
           ;; consult draws its character first; the spinner, started after,
           ;; draws its first frame over it.
           (funcall next action)
           (unless (xl-spinner-running-p)
             (xl-spinner-start (lambda (frame)
                                 (xl-consult-spinner--draw overlay frame))
                               :frames xl-spinner-one-dot-frames
                               :interval xl-spinner-one-dot-interval))))
        (`[indicator ,(or 'finished 'killed 'failed)]
         (when (xl-spinner-running-p)
           (xl-spinner-stop))
         (when ready
           ;; consult draws its one-cell character, which would shift the
           ;; text; the idle text goes over it, at the same width.
           (funcall next action)
           (xl-consult-spinner--idle overlay)))
        (`[indicator ,_]
         (when ready (funcall next action)))
        (_ (funcall next action))))))

(defun xl-consult-spinner--after-vertico-update (&rest _)
  "Redraw the spinner's frame when vertico updates the candidates.
The frame is drawn at once, on top of the timer's, so the animation and
the list stay in step."
  (when (xl-spinner-running-p)
    (xl-consult-spinner--draw (xl-consult-spinner--overlay)
                              (xl-spinner-current-frame))))

(advice-add 'vertico--update :after #'xl-consult-spinner--after-vertico-update)

(with-eval-after-load 'consult
  (advice-add 'consult--async-indicator :around
              #'xl-consult-spinner--around-indicator))

(provide 'xl-consult-spinner)
;;; xl-consult-spinner.el ends here
