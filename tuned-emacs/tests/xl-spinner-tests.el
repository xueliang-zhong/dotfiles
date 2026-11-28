;;; xl-spinner-tests.el --- ERT tests for xl-spinner -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-spinner-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'xl-spinner)

(defun xl-test--wait-for-slot (buffer)
  "Wait until BUFFER's mode-line loader has gone, minimum time included."
  (let ((end (+ (float-time) 10)))
    (while (and (xl-spinner-mode-line-busy-p buffer)
                (< (float-time) end))
      (accept-process-output nil 0.05)))
  (should-not (xl-spinner-mode-line-busy-p buffer)))

(ert-deftest xl-spinner-frames-ticks-and-stops ()
  "The shared loader draws a frame at once, advances, limits and clears."
  (with-temp-buffer
    (let ((xl-spinner-style 'ascii)
          drawn)
      (xl-spinner-start (lambda (text) (push text drawn)) :limit 1)
      ;; The first frame is on screen before the caller gets control back.
      (should (xl-spinner-running-p))
      (should (equal (car xl-spinner-frames) (car drawn)))
      (xl-spinner-tick (current-buffer))
      (should (equal (cadr xl-spinner-frames) (car drawn)))
      ;; The limit stops the loader on the next tick and clears it.
      (xl-spinner-tick (current-buffer))
      (should-not (xl-spinner-running-p))
      (should (equal '(nil "[/]" "[|]") drawn)))
    ;; Starting again resets the frames; stopping clears the drawing.
    (let ((xl-spinner-style 'ascii)
          drawn)
      (xl-spinner-start (lambda (text) (push text drawn)))
      (should (xl-spinner-running-p))
      (xl-spinner-stop)
      (should-not (xl-spinner-running-p))
      (should (equal '(nil "[|]") drawn)))))

;;; ============================================================
;;; xl-spinner: frames per display and the mode-line slot
;;; ============================================================

(ert-deftest xl-spinner-frames-fall-back-to-ascii ()
  "Dots only when every dot glyph keeps the mode line's size, else ASCII."
  (let ((xl-spinner-style 'auto))
    (cl-letf (((symbol-function 'xl-spinner--glyph-safe-p) #'always))
      (should (equal (xl-spinner-default-frames) xl-spinner-dot-frames)))
    ;; One glyph from a fallback font (taller mode line) means ASCII.
    (cl-letf (((symbol-function 'xl-spinner--glyph-safe-p)
               (lambda (char) (/= char ?●))))
      (should (equal (xl-spinner-default-frames) xl-spinner-frames))))
  (let ((xl-spinner-style 'ascii))
    (should (equal (xl-spinner-default-frames) xl-spinner-frames)))
  (let ((xl-spinner-style 'dots))
    (should (equal (xl-spinner-default-frames) xl-spinner-dot-frames)))
  ;; A terminal without UTF-8 (e.g. LANG=C over ssh) gets ASCII.
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
            ((symbol-function 'terminal-coding-system)
             (lambda (&rest _) 'us-ascii)))
    (should-not (xl-spinner--glyph-safe-p ?●))))

(ert-deftest xl-spinner-slot-never-flashes-a-fast-operation ()
  "Work that ends before the delay leaves the mode line untouched."
  (with-temp-buffer
    (xl-spinner-mode-line-install)
    (xl-spinner-mode-line-start "Staging")
    (should (xl-spinner-mode-line-busy-p))
    (should-not (xl-spinner-running-p))
    (should-not xl-spinner-mode-line)
    (xl-spinner-mode-line-stop)
    (should-not (xl-spinner-mode-line-busy-p))
    (should-not xl-spinner-mode-line)))

(ert-deftest xl-spinner-slot-lingers-and-keeps-its-width ()
  "A shown loader stays its minimum time, a restart keeps it, it never narrows."
  (with-temp-buffer
    (let ((xl-spinner-show-delay 0)
          (xl-spinner-min-duration 0.2)
          (xl-spinner-style 'ascii))
      (xl-spinner-mode-line-install)
      (xl-spinner-mode-line-start "Pushing main to origin")
      (should (equal " [|] Pushing main to origin " xl-spinner-mode-line))
      ;; A shorter label keeps the width, so nothing after the slot moves.
      (xl-spinner-mode-line-label "Refreshing")
      (should (equal (string-width " [|] Pushing main to origin ")
                     (string-width xl-spinner-mode-line)))
      (should (string-prefix-p " [|] Refreshing " xl-spinner-mode-line))
      ;; Stopping right away lingers instead of flashing off.
      (xl-spinner-mode-line-stop)
      (should (xl-spinner-running-p))
      ;; New work during the linger takes the same loader over.
      (xl-spinner-mode-line-start "Staging")
      (should (string-prefix-p " [|] Staging " xl-spinner-mode-line))
      (xl-spinner-mode-line-stop)
      (xl-test--wait-for-slot (current-buffer))
      (should-not (xl-spinner-running-p))
      (should-not xl-spinner-mode-line))))

(ert-deftest xl-spinner-slot-also-draws-each-frame ()
  "`:also' gets every frame the slot draws, and nil when the slot empties."
  (dolist (stop '(plain with-text))
    (with-temp-buffer
      (let ((xl-spinner-show-delay 0)
            (xl-spinner-min-duration 0)
            (xl-spinner-style 'ascii)
            drawn)
        (xl-spinner-mode-line-install)
        (xl-spinner-mode-line-start "work" :also (lambda (frame) (push frame drawn)))
        (xl-spinner-tick (current-buffer))
        (should (equal '("[/]" "[|]") (mapcar #'substring-no-properties drawn)))
        (if (eq stop 'plain)
            (xl-spinner-mode-line-stop)
          (xl-spinner-mode-line-stop "failed"))
        ;; The last call is the nil that clears it, made once.
        (should (null (car drawn)))
        (should (= 1 (cl-count nil drawn)))))))

(ert-deftest xl-spinner-slot-shows-text-after-stop ()
  "A stop with text (an error) replaces the loader at once and stays."
  (with-temp-buffer
    (let ((xl-spinner-show-delay 0))
      (xl-spinner-mode-line-install)
      (xl-spinner-mode-line-start "git blame")
      (xl-spinner-mode-line-stop "git blame failed: 100% broken")
      (should-not (xl-spinner-running-p))
      (should-not (xl-spinner-mode-line-busy-p))
      ;; `%' is escaped, because the slot is a mode line construct.
      (should (equal " git blame failed: 100%% broken " xl-spinner-mode-line))
      ;; The next start clears it.
      (xl-spinner-mode-line-start "git blame")
      (should-not (string-match-p "failed" (or xl-spinner-mode-line ""))))))

(ert-deftest xl-spinner-tint-blends-toward-the-loader-colour ()
  "The tint moves the background by `xl-spinner-tint' toward the accent."
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216))
            ((symbol-function 'face-background) (lambda (&rest _) "#000000"))
            ((symbol-function 'face-foreground) (lambda (&rest _) "#ffffff")))
    (let ((xl-spinner-tint 0.2))
      (should (equal "#333333" (xl-spinner--tint-color 'mode-line-active))))
    (let ((xl-spinner-tint nil))
      (should-not (xl-spinner--tint-color 'mode-line-active)))))
