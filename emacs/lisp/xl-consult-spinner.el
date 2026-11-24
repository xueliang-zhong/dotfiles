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
;; The frames are the three-cell frames of the mode-line loaders.  They are
;; wider than consult's one character, so the typed text moves two cells
;; right while the search runs.

;;; Code:

(require 'cl-lib)
(require 'xl-spinner)

(declare-function consult--async-indicator "consult")

(defcustom xl-consult-spinner-interval 0.15
  "Seconds per frame of the consult indicator.
Two cells cannot carry the three-cell pace of the other loaders: at 0.15 s
a three-frame cycle of two dots reads as a steady motion."
  :type 'number
  :group 'xl-spinner)

(defvar xl-consult-spinner-frames
  (if (seq-every-p #'xl-spinner--glyph-safe-p "·●")
      '("··" "●·" "·●")
    '(".." "*." ".*"))
  "Frames drawn in the two cells reserved in front of the prompt.
Dots when the font draws them, `..', `*.', `.*' otherwise.  Every frame,
and the idle text, is exactly two cells wide, so the typed text never moves.")

(defconst xl-consult-spinner-idle ": "
  "What the reserved cells show when no search runs: consult's colon, padded.")

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
  "Show the idle text on OVERLAY, two cells wide like the frames."
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
  "Return the handler that animates the indicator, forwarding to NEXT."
  (let (overlay)
    (lambda (action)
      (pcase action
        ('setup
         (funcall next action)
         (setq overlay (xl-consult-spinner--overlay))
         (xl-consult-spinner--idle overlay))
        ('destroy
         (when (xl-spinner-running-p)
           (xl-spinner-stop))
         (funcall next action))
        (`[indicator running]
         ;; consult draws its character first; the spinner, started after,
         ;; draws its first frame over it.
         (funcall next action)
         (unless (xl-spinner-running-p)
           (xl-spinner-start (lambda (frame)
                               (xl-consult-spinner--draw overlay frame))
                             :frames xl-consult-spinner-frames
                             :interval xl-consult-spinner-interval)))
        (`[indicator ,(or 'finished 'killed 'failed)]
         (when (xl-spinner-running-p)
           (xl-spinner-stop))
         ;; consult draws its one-cell character, which would shift the
         ;; text; the idle text goes over it, at the same width.
         (funcall next action)
         (xl-consult-spinner--idle overlay))
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
