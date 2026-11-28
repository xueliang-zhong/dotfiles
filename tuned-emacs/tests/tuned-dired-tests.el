;;; tuned-dired-tests.el --- Tests for tuned-dired -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-dired-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Each test works in its own temporary folder.  The listings and previews
;; are real asynchronous processes, so the tests wait for them.

(require 'ert)
(require 'cl-lib)
(require 'tuned-dired)

(defun tuned-dired-test--wait (pred &optional seconds)
  "Run the event loop until PRED returns non-nil, or SECONDS pass."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro tuned-dired-test--with-folder (var &rest body)
  "Bind VAR to a temporary folder holding a text file, a binary file and a subfolder."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "tuned-dired-test-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "a.txt" ,var) (insert "hello\nworld\n"))
           (with-temp-file (expand-file-name "bin.dat" ,var)
             (set-buffer-multibyte nil)
             (insert (make-string 64 0)))
           (make-directory (expand-file-name "sub" ,var))
           (with-temp-file (expand-file-name "inner.txt" (expand-file-name "sub" ,var))
             (insert "x"))
           ,@body)
       (delete-directory ,var t))))

(defun tuned-dired-test--preview-text ()
  (with-current-buffer tuned-dired-preview-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun tuned-dired-test--goto (name)
  (goto-char (point-min))
  (re-search-forward (concat " " (regexp-quote name) "$"))
  (beginning-of-line))

(ert-deftest tuned-dired-listing-is-asynchronous-and-shows-the-folder ()
  "The buffer is empty until `ls' answers, then shows the listing."
  (tuned-dired-test--with-folder dir
    (let ((buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (should (with-current-buffer buf (zerop (buffer-size))))
            (should (tuned-dired-test--wait
                     (lambda () (with-current-buffer buf (> (buffer-size) 0)))))
            (with-current-buffer buf
              (should (string-match-p "a\\.txt" (buffer-string)))
              (should (string-match-p "sub" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest tuned-dired-text-file-preview-shows-its-lines ()
  "A text file's preview is the file's first lines, from `head'."
  (tuned-dired-test--with-folder dir
    (let ((buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (tuned-dired-test--goto "a.txt")
              (tuned-dired-preview))
            (should (tuned-dired-test--wait
                     (lambda ()
                       (string-match-p "world" (tuned-dired-test--preview-text)))))
            (should (string-match-p "hello" (tuned-dired-test--preview-text))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))))))

(ert-deftest tuned-dired-elc-preview-shows-text-not-a-hex-dump ()
  "A byte-compiled file previews its comments and docstrings, not hex data."
  (tuned-dired-test--with-folder dir
    (let ((file (expand-file-name "sample.elc" dir)))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert ";ELC\37\0\0\0\n;;; Compiled\n")
        (insert "#@12 A docstring.\37\n(defalias 'x \"\\301\")\n"))
      (unwind-protect
          (progn
            (tuned-dired-preview--load-file file)
            (should (tuned-dired-test--wait
                     (lambda ()
                       (string-match-p "A docstring"
                                       (tuned-dired-test--preview-text)))))
            (should (string-match-p ";;; Compiled"
                                    (tuned-dired-test--preview-text)))
            (should-not (string-match-p "Binary file"
                                        (tuned-dired-test--preview-text))))
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))))))

(ert-deftest tuned-dired-entering-the-preview-opens-the-real-file ()
  "Selecting the preview window shows the file itself, not the preview."
  (tuned-dired-test--with-folder dir
    (let* ((file (expand-file-name "a.txt" dir))
           (buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (tuned-dired-test--goto "a.txt")
              (tuned-dired-preview))
            (should (tuned-dired-test--wait
                     (lambda ()
                       (string-match-p "hello" (tuned-dired-test--preview-text)))))
            (let ((preview-window (get-buffer-window tuned-dired-preview-buffer-name t)))
              (should (window-live-p preview-window))
              (select-window preview-window)
              (run-hooks 'post-command-hook)
              (should (eq (window-buffer preview-window)
                          (find-file-noselect file)))
              (should (equal (buffer-file-name (window-buffer preview-window)) file))
              (should (eq (current-buffer) (window-buffer preview-window)))
              (should (with-current-buffer tuned-dired-preview-buffer-name
                        (zerop (buffer-size))))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))
        (when-let* ((file-buffer (find-buffer-visiting file)))
          (kill-buffer file-buffer))))))

(ert-deftest tuned-dired-entering-a-deleted-preview-leaves-it-blank ()
  "A preview whose entry disappeared is emptied, not left looking like a file."
  (tuned-dired-test--with-folder dir
    (let* ((file (expand-file-name "a.txt" dir))
           (buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (tuned-dired-test--goto "a.txt")
              (tuned-dired-preview))
            (should (tuned-dired-test--wait
                     (lambda ()
                       (string-match-p "hello" (tuned-dired-test--preview-text)))))
            (delete-file file)
            (let ((preview-window (get-buffer-window tuned-dired-preview-buffer-name t)))
              (should (window-live-p preview-window))
              (select-window preview-window)
              (tuned-dired-preview--enter preview-window)
              (should (eq (window-buffer preview-window)
                          (get-buffer tuned-dired-preview-buffer-name)))
              (should (with-current-buffer tuned-dired-preview-buffer-name
                        (zerop (buffer-size))))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))))))

(ert-deftest tuned-dired-idle-preview-does-not-replace-the-entered-file ()
  "Once a preview window holds the file, resting on that entry does not replace it."
  (tuned-dired-test--with-folder dir
    (let* ((file (expand-file-name "a.txt" dir))
           (buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (tuned-dired-test--goto "a.txt")
              (tuned-dired-preview))
            (should (tuned-dired-test--wait
                     (lambda ()
                       (string-match-p "hello" (tuned-dired-test--preview-text)))))
            (let ((preview-window (get-buffer-window tuned-dired-preview-buffer-name t)))
              (select-window preview-window)
              (tuned-dired-preview--enter preview-window)
              (select-window (get-buffer-window buf t))
              (tuned-dired--idle-preview buf)
              (should-not (get-buffer-window tuned-dired-preview-buffer-name t))
              (should (eq (window-buffer preview-window)
                          (find-file-noselect file)))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))
        (when-let* ((file-buffer (find-buffer-visiting file)))
          (kill-buffer file-buffer))))))

(ert-deftest tuned-dired-preview-mode-watches-for-focus ()
  "The preview buffer reacts when its window is selected or changes buffer."
  (let ((buffer (tuned-dired-preview--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (should (memq #'tuned-dired-preview--enter post-command-hook))
          (should (memq #'tuned-dired-preview--enter
                        window-selection-change-functions))
          (should (memq #'tuned-dired-preview--enter
                        window-buffer-change-functions)))
      (kill-buffer buffer))))

(ert-deftest tuned-dired-binary-file-shows-a-hex-dump ()
  "A binary file shows a bounded hex dump with a note, not its raw bytes."
  (tuned-dired-test--with-folder dir
    (let ((buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (tuned-dired-test--goto "bin.dat")
              (tuned-dired-preview))
            (should (tuned-dired-test--wait
                     (lambda ()
                       (string-match-p "Binary file" (tuned-dired-test--preview-text)))))
            (should (string-match-p "^00000000  " (tuned-dired-test--preview-text)))
            (should-not (string-match-p "\0" (tuned-dired-test--preview-text))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))))))

(ert-deftest tuned-dired-open-folder-reuses-the-window ()
  "Entering a folder keeps the same buffer and window; only its folder changes."
  (tuned-dired-test--with-folder dir
    (let ((buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (switch-to-buffer buf)
            (tuned-dired-test--goto "sub")
            (let ((windows (length (window-list))))
              (tuned-dired-open)
              (should (eq (current-buffer) buf))
              (should (= windows (length (window-list))))
              (should (equal (tuned-dired--directory)
                             (file-name-as-directory (expand-file-name "sub" dir))))))
        (kill-buffer buf)))))

(ert-deftest tuned-dired-tab-previews-and-the-idle-timer-is-scheduled ()
  "TAB is the preview command, and a rest of the cursor schedules one."
  (should (eq (keymap-lookup tuned-dired-mode-map "TAB") #'tuned-dired-preview))
  (with-temp-buffer
    (tuned-dired-mode)
    (tuned-dired--schedule-preview)
    (should (timerp tuned-dired--hover-timer))
    (cancel-timer tuned-dired--hover-timer)))

(ert-deftest tuned-dired-clip-keeps-the-first-lines-and-marks-a-cut ()
  "Clipping is bounded, marks a cut line, and a limit of zero keeps nothing."
  (let ((tuned-dired-preview-lines 2))
    (should (equal (tuned-dired-preview--clip "a\nb") "a\nb"))
    (should (equal (tuned-dired-preview--clip "a\nb\nc") "a\nb…")))
  (let ((tuned-dired-preview-lines 0))
    (should (equal (tuned-dired-preview--clip "a\nb") ""))))

;; Folders, `.' and `..' have no preview: TAB starts nothing and opens nothing.
(ert-deftest tuned-dired-folders-have-no-preview ()
  "A folder, `.' or `..' is never previewed; no preview process starts."
  (tuned-dired-test--with-folder dir
    (let ((buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (when (get-buffer tuned-dired-preview-buffer-name)
              (kill-buffer tuned-dired-preview-buffer-name))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (dolist (name '("sub" "." ".."))
                (tuned-dired-test--goto name)
                (tuned-dired-preview)
                (should-not (get-buffer tuned-dired-preview-buffer-name)))
              (should-not (tuned-dired-preview--maybe-load
                           (expand-file-name "sub" dir)))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))))))

;; Windows has none of head, hexdump, od or ls by default.  These tests
;; make `executable-find' miss those programs, as on such a machine.
(defmacro tuned-dired-test--without (programs &rest body)
  "Run BODY as if the PROGRAMS (a list of names) were not installed."
  (declare (indent 1))
  `(let ((real (symbol-function 'executable-find)))
     (cl-letf (((symbol-function 'executable-find)
                (lambda (name &rest args)
                  (unless (member name ',programs)
                    (apply real name args)))))
       ,@body)))

(ert-deftest tuned-dired-text-preview-works-without-head ()
  "Without `head' (Windows), Emacs reads the first bytes itself."
  (tuned-dired-test--with-folder dir
    (let ((buf (tuned-dired-noselect dir)))
      (unwind-protect
          (progn
            (tuned-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (let ((file (expand-file-name "a.txt" dir)))
              (tuned-dired-preview--prepare file)
              (tuned-dired-test--without ("head" "sed")
                (tuned-dired-preview--load-text file)))
            (should (string-match-p "hello" (tuned-dired-test--preview-text))))
        (kill-buffer buf)
        (when (get-buffer tuned-dired-preview-buffer-name)
          (kill-buffer tuned-dired-preview-buffer-name))))))

(ert-deftest tuned-dired-hex-dump-works-without-hexdump-or-od ()
  "Without `hexdump' or `od', Emacs makes the hex dump itself."
  (tuned-dired-test--with-folder dir
    (let ((file (expand-file-name "bin.dat" dir)))
      (tuned-dired-preview--prepare file)
      (tuned-dired-test--without ("hexdump" "od")
        (tuned-dired-preview--load-hex file)))
    (should (string-match-p "^00000000  " (tuned-dired-test--preview-text)))
    (should (string-match-p "Binary file" (tuned-dired-test--preview-text)))
    (when (get-buffer tuned-dired-preview-buffer-name)
      (kill-buffer tuned-dired-preview-buffer-name))))

(ert-deftest tuned-dired-hex-lines-show-hex-and-an-ascii-column ()
  "Each line: an offset, the bytes in hex, and the printable characters."
  (let ((text (tuned-dired-preview--hex-lines (unibyte-string ?A ?\0 ?B))))
    (should (string-match-p "^00000000  41 00 42 +|A.B|$" text))))

(ert-deftest tuned-dired-listing-says-when-ls-is-missing ()
  "Without the listing program the listing is refused with a message.
The program is `insert-directory-program', which is `gls' on some Macs."
  (tuned-dired-test--with-folder dir
    (let ((insert-directory-program "tuned-dired-no-such-ls"))
      (should-error (tuned-dired-noselect dir) :type 'user-error))
    (when-let* ((buf (get-buffer (tuned-dired--buffer-name dir))))
      (kill-buffer buf))))

;; The binary check is a regexp scan with an early exit; these pin its rules.
(ert-deftest tuned-dired-binary-check-follows-its-rules ()
  "NUL is binary; ordinary whitespace is not; many control characters are."
  (should-not (tuned-dired-preview--binary-text-p "a\tb\nc\rd\fe\bf\n"))
  (should (tuned-dired-preview--binary-text-p "ab\0cd"))
  (should (tuned-dired-preview--binary-text-p (make-string 200 1)))
  (should-not (tuned-dired-preview--binary-text-p
               (concat (make-string 39 ?x) "\1")))
  (should-not (tuned-dired-preview--binary-text-p (make-string 1000 ?a)))
  (should-not (tuned-dired-preview--binary-text-p "")))

(provide 'tuned-dired-tests)
;;; tuned-dired-tests.el ends here
