;;; tuned-spinner-tests.el --- ERT tests for tuned-spinner -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-spinner-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'tuned-spinner)

(defun tuned-test--wait-for-slot (buffer)
  "Wait until BUFFER's mode-line loader has gone, minimum time included."
  (let ((end (+ (float-time) 10)))
    (while (and (tuned-spinner-mode-line-busy-p buffer)
                (< (float-time) end))
      (accept-process-output nil 0.05)))
  (should-not (tuned-spinner-mode-line-busy-p buffer)))

(ert-deftest tuned-spinner-frames-ticks-and-stops ()
  "The shared loader draws a frame at once, advances, limits and clears."
  (with-temp-buffer
    (let ((tuned-spinner-style 'ascii)
          drawn)
      (tuned-spinner-start (lambda (text) (push text drawn)) :limit 1)
      ;; The first frame is on screen before the caller gets control back.
      (should (tuned-spinner-running-p))
      (should (equal (car tuned-spinner-frames) (car drawn)))
      (tuned-spinner-tick (current-buffer))
      (should (equal (cadr tuned-spinner-frames) (car drawn)))
      ;; The limit stops the loader on the next tick and clears it.
      (tuned-spinner-tick (current-buffer))
      (should-not (tuned-spinner-running-p))
      (should (equal '(nil "[/]" "[|]") drawn)))
    ;; Starting again resets the frames; stopping clears the drawing.
    (let ((tuned-spinner-style 'ascii)
          drawn)
      (tuned-spinner-start (lambda (text) (push text drawn)))
      (should (tuned-spinner-running-p))
      (tuned-spinner-stop)
      (should-not (tuned-spinner-running-p))
      (should (equal '(nil "[|]") drawn)))))

;;; ============================================================
;;; tuned-spinner: frames per display and the mode-line slot
;;; ============================================================

(ert-deftest tuned-spinner-frames-fall-back-to-ascii ()
  "Dots only when every dot glyph keeps the mode line's size, else ASCII."
  (let ((tuned-spinner-style 'auto))
    (cl-letf (((symbol-function 'tuned-spinner--glyph-safe-p) #'always))
      (should (equal (tuned-spinner-default-frames) tuned-spinner-dot-frames)))
    ;; One glyph from a fallback font (taller mode line) means ASCII.
    (cl-letf (((symbol-function 'tuned-spinner--glyph-safe-p)
               (lambda (char) (/= char ?●))))
      (should (equal (tuned-spinner-default-frames) tuned-spinner-frames))))
  (let ((tuned-spinner-style 'ascii))
    (should (equal (tuned-spinner-default-frames) tuned-spinner-frames)))
  (let ((tuned-spinner-style 'dots))
    (should (equal (tuned-spinner-default-frames) tuned-spinner-dot-frames)))
  ;; A terminal without UTF-8 (e.g. LANG=C over ssh) gets ASCII.
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
            ((symbol-function 'terminal-coding-system)
             (lambda (&rest _) 'us-ascii)))
    (should-not (tuned-spinner--glyph-safe-p ?●))))

(ert-deftest tuned-spinner-slot-never-flashes-a-fast-operation ()
  "Work that ends before the delay leaves the mode line untouched."
  (with-temp-buffer
    (tuned-spinner-mode-line-install)
    (tuned-spinner-mode-line-start "Staging")
    (should (tuned-spinner-mode-line-busy-p))
    (should-not (tuned-spinner-running-p))
    (should-not tuned-spinner-mode-line)
    (tuned-spinner-mode-line-stop)
    (should-not (tuned-spinner-mode-line-busy-p))
    (should-not tuned-spinner-mode-line)))

(ert-deftest tuned-spinner-slot-lingers-and-keeps-its-width ()
  "A shown loader stays its minimum time, a restart keeps it, it never narrows."
  (with-temp-buffer
    (let ((tuned-spinner-show-delay 0)
          (tuned-spinner-min-duration 0.2)
          (tuned-spinner-style 'ascii))
      (tuned-spinner-mode-line-install)
      (tuned-spinner-mode-line-start "Pushing main to origin")
      (should (equal " [|] Pushing main to origin " tuned-spinner-mode-line))
      ;; A shorter label keeps the width, so nothing after the slot moves.
      (tuned-spinner-mode-line-label "Refreshing")
      (should (equal (string-width " [|] Pushing main to origin ")
                     (string-width tuned-spinner-mode-line)))
      (should (string-prefix-p " [|] Refreshing " tuned-spinner-mode-line))
      ;; Stopping right away lingers instead of flashing off.
      (tuned-spinner-mode-line-stop)
      (should (tuned-spinner-running-p))
      ;; New work during the linger takes the same loader over.
      (tuned-spinner-mode-line-start "Staging")
      (should (string-prefix-p " [|] Staging " tuned-spinner-mode-line))
      (tuned-spinner-mode-line-stop)
      (tuned-test--wait-for-slot (current-buffer))
      (should-not (tuned-spinner-running-p))
      (should-not tuned-spinner-mode-line))))

(ert-deftest tuned-spinner-slot-also-draws-each-frame ()
  "`:also' gets every frame the slot draws, and nil when the slot empties."
  (dolist (stop '(plain with-text))
    (with-temp-buffer
      (let ((tuned-spinner-show-delay 0)
            (tuned-spinner-min-duration 0)
            (tuned-spinner-style 'ascii)
            drawn)
        (tuned-spinner-mode-line-install)
        (tuned-spinner-mode-line-start "work" :also (lambda (frame) (push frame drawn)))
        (tuned-spinner-tick (current-buffer))
        (should (equal '("[/]" "[|]") (mapcar #'substring-no-properties drawn)))
        (if (eq stop 'plain)
            (tuned-spinner-mode-line-stop)
          (tuned-spinner-mode-line-stop "failed"))
        ;; The last call is the nil that clears it, made once.
        (should (null (car drawn)))
        (should (= 1 (cl-count nil drawn)))))))

(ert-deftest tuned-spinner-slot-shows-text-after-stop ()
  "A stop with text (an error) replaces the loader at once and stays."
  (with-temp-buffer
    (let ((tuned-spinner-show-delay 0))
      (tuned-spinner-mode-line-install)
      (tuned-spinner-mode-line-start "git blame")
      (tuned-spinner-mode-line-stop "git blame failed: 100% broken")
      (should-not (tuned-spinner-running-p))
      (should-not (tuned-spinner-mode-line-busy-p))
      ;; `%' is escaped, because the slot is a mode line construct.
      (should (equal " git blame failed: 100%% broken " tuned-spinner-mode-line))
      ;; The next start clears it.
      (tuned-spinner-mode-line-start "git blame")
      (should-not (string-match-p "failed" (or tuned-spinner-mode-line ""))))))

(ert-deftest tuned-spinner-tint-blends-toward-the-loader-colour ()
  "The tint moves the background by `tuned-spinner-tint' toward the accent."
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216))
            ((symbol-function 'face-background) (lambda (&rest _) "#000000"))
            ((symbol-function 'face-foreground) (lambda (&rest _) "#ffffff")))
    (let ((tuned-spinner-tint 0.2))
      (should (equal "#333333" (tuned-spinner--tint-color 'mode-line-active))))
    (let ((tuned-spinner-tint nil))
      (should-not (tuned-spinner--tint-color 'mode-line-active)))))
