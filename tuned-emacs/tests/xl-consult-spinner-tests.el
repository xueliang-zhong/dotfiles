;;; xl-consult-spinner-tests.el --- ERT tests for xl-consult-spinner -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-consult-spinner-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'xl-spinner)
(require 'xl-consult-spinner)

(ert-deftest xl-consult-spinner-keeps-consults-arity ()
  "The real `consult--async-indicator' still takes no arguments and returns
a pipeline element, which takes the sink.  Calling it both ways must work."
  (require 'consult)
  (require 'xl-consult-spinner)
  (with-temp-buffer
    (insert "Search: ")
    (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 8)))
      (let ((handler (funcall (funcall #'consult--async-indicator) #'ignore)))
        (should (functionp handler))
        (funcall handler 'setup)
        (funcall handler 'destroy)))))

(ert-deftest xl-consult-spinner-animates-the-running-indicator ()
  "While consult's async list runs, its indicator animates at the pace of
the other loaders, and a vertico update redraws the frame.  When the list ends, consult's
own final character is drawn."
  (require 'consult)
  (require 'xl-consult-spinner)
  (with-temp-buffer
    (insert "Search: ")
    (let* ((xl-spinner-show-delay 0)
           (xl-spinner-min-duration 0)
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
           (handler (funcall (xl-consult-spinner--around-indicator
                              (lambda () consult-side))
                             #'ignore)))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 8)))
        (funcall handler 'setup)
        ;; Idle from the start: the reserved cells are never empty.
        (should (equal xl-consult-spinner-idle (overlay-get overlay 'display)))
        (funcall handler [indicator running])
        (should (xl-spinner-running-p))
        (should (= xl-spinner-one-dot-interval
                   (plist-get xl-spinner--state :interval)))
        (should (member (substring-no-properties (overlay-get overlay 'display))
                        xl-spinner-one-dot-frames))
        ;; Every frame and the idle text take the same one cell.
        (dolist (frame xl-spinner-one-dot-frames)
          (should (= 1 (string-width frame))))
        (should (= 1 (string-width xl-consult-spinner-idle)))
        ;; Same colour as the mode-line loaders: the frame keeps its face.
        (should (eq 'xl-spinner
                    (get-text-property 0 'face (overlay-get overlay 'display))))
        (let ((first (overlay-get overlay 'display)))
          (xl-spinner-tick (current-buffer))
          (should-not (equal first (overlay-get overlay 'display))))
        ;; A vertico update redraws the current frame at once.
        (let ((frame (xl-spinner-current-frame)))
          (xl-consult-spinner--after-vertico-update)
          (should (equal frame (overlay-get overlay 'display))))
        (funcall handler [indicator finished])
        (should-not (xl-spinner-running-p))
        ;; The spinner stopped, and the idle text is back, at the same width.
        (should (equal xl-consult-spinner-idle (overlay-get overlay 'display)))
        (should (= 1 (string-width (overlay-get overlay 'display))))
        (funcall handler 'destroy)))))
