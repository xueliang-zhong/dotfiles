;;; tuned-consult-spinner-tests.el --- ERT tests for tuned-consult-spinner -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-consult-spinner-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'tuned-spinner)
(require 'tuned-consult-spinner)

(ert-deftest tuned-consult-spinner-keeps-consults-arity ()
  "The real `consult--async-indicator' still takes no arguments and returns
a pipeline element, which takes the sink.  Calling it both ways must work."
  (require 'consult)
  (require 'tuned-consult-spinner)
  (with-temp-buffer
    (insert "Search: ")
    (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 8)))
      (let ((handler (funcall (funcall #'consult--async-indicator) #'ignore)))
        (should (functionp handler))
        (funcall handler 'setup)
        (funcall handler 'destroy)))))

(ert-deftest tuned-consult-spinner-animates-the-running-indicator ()
  "While consult's async list runs, its indicator animates at the pace of
the other loaders, and a vertico update redraws the frame.  When the list ends, consult's
own final character is drawn."
  (require 'consult)
  (require 'tuned-consult-spinner)
  (with-temp-buffer
    (insert "Search: ")
    (let* ((tuned-spinner-show-delay 0)
           (tuned-spinner-min-duration 0)
           (overlay (make-overlay 6 7))
           ;; Stands in for consult's indicator: `*' while running, `:' when done.
           (consult-side
            (lambda (_sink)
              (lambda (action)
                (pcase action
                  ('setup (overlay-put overlay 'category
                                       'consult-async-indicator-overlay))
                  (`[indicator running] (overlay-put overlay 'display "*"))
                  (`[indicator finished] (overlay-put overlay 'display ":"))))))
           ;; consult's constructor takes no arguments, like this stand-in.
           (handler (funcall (tuned-consult-spinner--around-indicator
                              (lambda () consult-side))
                             #'ignore)))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 8)))
        (funcall handler 'setup)
        ;; Idle from the start: the reserved cells are never empty.
        (should (equal tuned-consult-spinner-idle (overlay-get overlay 'display)))
        (funcall handler [indicator running])
        (should (tuned-spinner-running-p))
        (should (= tuned-spinner-one-dot-interval
                   (plist-get tuned-spinner--state :interval)))
        (should (member (substring-no-properties (overlay-get overlay 'display))
                        tuned-spinner-one-dot-frames))
        ;; Every frame and the idle text take the same one cell.
        (dolist (frame tuned-spinner-one-dot-frames)
          (should (= 1 (string-width frame))))
        (should (= 1 (string-width tuned-consult-spinner-idle)))
        ;; Same colour as the mode-line loaders: the frame keeps its face.
        (should (eq 'tuned-spinner
                    (get-text-property 0 'face (overlay-get overlay 'display))))
        (let ((first (overlay-get overlay 'display)))
          (tuned-spinner-tick (current-buffer))
          (should-not (equal first (overlay-get overlay 'display))))
        ;; A vertico update redraws the current frame at once.
        (let ((frame (tuned-spinner-current-frame)))
          (tuned-consult-spinner--after-vertico-update)
          (should (equal frame (overlay-get overlay 'display))))
        (funcall handler [indicator finished])
        (should-not (tuned-spinner-running-p))
        ;; The spinner stopped, and the idle text is back, at the same width.
        (should (equal tuned-consult-spinner-idle (overlay-get overlay 'display)))
        (should (= 1 (string-width (overlay-get overlay 'display))))
        (funcall handler 'destroy)))))
