;;; xl-dired-tests.el --- ERT tests for xl-dired -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L emacs/lisp -l emacs/tests/xl-dired-tests.el \
;;        -f ert-run-tests-batch-and-exit
;; Each test works in its own temporary folder.  The listings and previews
;; are real asynchronous processes, so the tests wait for them.

(require 'ert)
(require 'cl-lib)
(require 'xl-dired)

(defun xl-dired-test--wait (pred &optional seconds)
  "Run the event loop until PRED returns non-nil, or SECONDS pass."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro xl-dired-test--with-folder (var &rest body)
  "Bind VAR to a temporary folder holding a text file, a binary file and a subfolder."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "xl-dired-test-" t))))
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

(defun xl-dired-test--preview-text ()
  (with-current-buffer xl-dired-preview-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun xl-dired-test--goto (name)
  (goto-char (point-min))
  (re-search-forward (concat " " (regexp-quote name) "$"))
  (beginning-of-line))

(ert-deftest xl-dired-listing-is-asynchronous-and-shows-the-folder ()
  "The buffer is empty until `ls' answers, then shows the listing."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (should (with-current-buffer buf (zerop (buffer-size))))
            (should (xl-dired-test--wait
                     (lambda () (with-current-buffer buf (> (buffer-size) 0)))))
            (with-current-buffer buf
              (should (string-match-p "a\\.txt" (buffer-string)))
              (should (string-match-p "sub" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest xl-dired-text-file-preview-shows-its-lines ()
  "A text file's preview is the file's first lines, from `head'."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "a.txt")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "world" (xl-dired-test--preview-text)))))
            (should (string-match-p "hello" (xl-dired-test--preview-text))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))))))

(ert-deftest xl-dired-elc-preview-shows-text-not-a-hex-dump ()
  "A byte-compiled file previews its comments and docstrings, not hex data."
  (xl-dired-test--with-folder dir
    (let ((file (expand-file-name "sample.elc" dir)))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert ";ELC\37\0\0\0\n;;; Compiled\n")
        (insert "#@12 A docstring.\37\n(defalias 'x \"\\301\")\n"))
      (unwind-protect
          (progn
            (xl-dired-preview--load-file file)
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "A docstring"
                                       (xl-dired-test--preview-text)))))
            (should (string-match-p ";;; Compiled"
                                    (xl-dired-test--preview-text)))
            (should-not (string-match-p "Binary file"
                                        (xl-dired-test--preview-text))))
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))))))

(ert-deftest xl-dired-entering-the-preview-opens-the-real-file ()
  "Selecting the preview window shows the file itself, not the preview."
  (xl-dired-test--with-folder dir
    (let* ((file (expand-file-name "a.txt" dir))
           (buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "a.txt")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "hello" (xl-dired-test--preview-text)))))
            (let ((preview-window (get-buffer-window xl-dired-preview-buffer-name t)))
              (should (window-live-p preview-window))
              (select-window preview-window)
              (run-hooks 'post-command-hook)
              (should (eq (window-buffer preview-window)
                          (find-file-noselect file)))
              (should (equal (buffer-file-name (window-buffer preview-window)) file))
              (should (eq (current-buffer) (window-buffer preview-window)))
              (should (with-current-buffer xl-dired-preview-buffer-name
                        (zerop (buffer-size))))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))
        (when-let* ((file-buffer (find-buffer-visiting file)))
          (kill-buffer file-buffer))))))

(ert-deftest xl-dired-entering-a-deleted-preview-leaves-it-blank ()
  "A preview whose entry disappeared is emptied, not left looking like a file."
  (xl-dired-test--with-folder dir
    (let* ((file (expand-file-name "a.txt" dir))
           (buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "a.txt")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "hello" (xl-dired-test--preview-text)))))
            (delete-file file)
            (let ((preview-window (get-buffer-window xl-dired-preview-buffer-name t)))
              (should (window-live-p preview-window))
              (select-window preview-window)
              (xl-dired-preview--enter preview-window)
              (should (eq (window-buffer preview-window)
                          (get-buffer xl-dired-preview-buffer-name)))
              (should (with-current-buffer xl-dired-preview-buffer-name
                        (zerop (buffer-size))))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))))))

(ert-deftest xl-dired-entering-a-directory-preview-opens-xl-dired ()
  "Selecting a directory preview opens that directory in `xl-dired-mode'."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir))
          (sub (file-name-as-directory (expand-file-name "sub" dir))))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "sub")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "inner" (xl-dired-test--preview-text)))))
            (let ((preview-window (get-buffer-window xl-dired-preview-buffer-name t)))
              (should (window-live-p preview-window))
              (select-window preview-window)
              (xl-dired-preview--enter preview-window)
              (with-current-buffer (window-buffer preview-window)
                (should (derived-mode-p 'xl-dired-mode))
                (should (equal (xl-dired--directory) sub)))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))
        (when-let* ((sub-buffer (get-buffer (xl-dired--buffer-name sub))))
          (kill-buffer sub-buffer))))))

(ert-deftest xl-dired-idle-preview-does-not-replace-the-entered-file ()
  "Once a preview window holds the file, resting on that entry does not replace it."
  (xl-dired-test--with-folder dir
    (let* ((file (expand-file-name "a.txt" dir))
           (buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "a.txt")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "hello" (xl-dired-test--preview-text)))))
            (let ((preview-window (get-buffer-window xl-dired-preview-buffer-name t)))
              (select-window preview-window)
              (xl-dired-preview--enter preview-window)
              (select-window (get-buffer-window buf t))
              (xl-dired--idle-preview buf)
              (should-not (get-buffer-window xl-dired-preview-buffer-name t))
              (should (eq (window-buffer preview-window)
                          (find-file-noselect file)))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))
        (when-let* ((file-buffer (find-buffer-visiting file)))
          (kill-buffer file-buffer))))))

(ert-deftest xl-dired-preview-mode-watches-for-focus ()
  "The preview buffer reacts when its window is selected or changes buffer."
  (let ((buffer (xl-dired-preview--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (should (memq #'xl-dired-preview--enter post-command-hook))
          (should (memq #'xl-dired-preview--enter
                        window-selection-change-functions))
          (should (memq #'xl-dired-preview--enter
                        window-buffer-change-functions)))
      (kill-buffer buffer))))

(ert-deftest xl-dired-binary-file-shows-a-hex-dump ()
  "A binary file shows a bounded hex dump with a note, not its raw bytes."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "bin.dat")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda ()
                       (string-match-p "Binary file" (xl-dired-test--preview-text)))))
            (should (string-match-p "^00000000  " (xl-dired-test--preview-text)))
            (should-not (string-match-p "\0" (xl-dired-test--preview-text))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))))))

(ert-deftest xl-dired-directory-preview-lists-its-entries ()
  "Previewing a folder shows its `ls' listing in the same preview buffer."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (xl-dired-test--goto "sub")
              (xl-dired-preview))
            (should (xl-dired-test--wait
                     (lambda () (string-match-p "inner" (xl-dired-test--preview-text))))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))))))

(ert-deftest xl-dired-open-folder-reuses-the-window ()
  "Entering a folder keeps the same buffer and window; only its folder changes."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (switch-to-buffer buf)
            (xl-dired-test--goto "sub")
            (let ((windows (length (window-list))))
              (xl-dired-open)
              (should (eq (current-buffer) buf))
              (should (= windows (length (window-list))))
              (should (equal (xl-dired--directory)
                             (file-name-as-directory (expand-file-name "sub" dir))))))
        (kill-buffer buf)))))

(ert-deftest xl-dired-tab-previews-and-the-idle-timer-is-scheduled ()
  "TAB is the preview command, and a rest of the cursor schedules one."
  (should (eq (keymap-lookup xl-dired-mode-map "TAB") #'xl-dired-preview))
  (with-temp-buffer
    (xl-dired-mode)
    (xl-dired--schedule-preview)
    (should (timerp xl-dired--hover-timer))
    (cancel-timer xl-dired--hover-timer)))

(ert-deftest xl-dired-clip-keeps-the-first-lines-and-marks-a-cut ()
  "Clipping is bounded, marks a cut line, and a limit of zero keeps nothing."
  (let ((xl-dired-preview-lines 2))
    (should (equal (xl-dired-preview--clip "a\nb") "a\nb"))
    (should (equal (xl-dired-preview--clip "a\nb\nc") "a\nb…")))
  (let ((xl-dired-preview-lines 0))
    (should (equal (xl-dired-preview--clip "a\nb") ""))))

;; Windows has none of head, hexdump, od or ls by default.  These tests
;; make `executable-find' miss those programs, as on such a machine.
(defmacro xl-dired-test--without (programs &rest body)
  "Run BODY as if the PROGRAMS (a list of names) were not installed."
  (declare (indent 1))
  `(let ((real (symbol-function 'executable-find)))
     (cl-letf (((symbol-function 'executable-find)
                (lambda (name &rest args)
                  (unless (member name ',programs)
                    (apply real name args)))))
       ,@body)))

(ert-deftest xl-dired-text-preview-works-without-head ()
  "Without `head' (Windows), Emacs reads the first bytes itself."
  (xl-dired-test--with-folder dir
    (let ((buf (xl-dired-noselect dir)))
      (unwind-protect
          (progn
            (xl-dired-test--wait (lambda () (with-current-buffer buf (> (buffer-size) 0))))
            (let ((file (expand-file-name "a.txt" dir)))
              (xl-dired-preview--prepare file)
              (xl-dired-test--without ("head" "sed")
                (xl-dired-preview--load-text file)))
            (should (string-match-p "hello" (xl-dired-test--preview-text))))
        (kill-buffer buf)
        (when (get-buffer xl-dired-preview-buffer-name)
          (kill-buffer xl-dired-preview-buffer-name))))))

(ert-deftest xl-dired-hex-dump-works-without-hexdump-or-od ()
  "Without `hexdump' or `od', Emacs makes the hex dump itself."
  (xl-dired-test--with-folder dir
    (let ((file (expand-file-name "bin.dat" dir)))
      (xl-dired-preview--prepare file)
      (xl-dired-test--without ("hexdump" "od")
        (xl-dired-preview--load-hex file)))
    (should (string-match-p "^00000000  " (xl-dired-test--preview-text)))
    (should (string-match-p "Binary file" (xl-dired-test--preview-text)))
    (when (get-buffer xl-dired-preview-buffer-name)
      (kill-buffer xl-dired-preview-buffer-name))))

(ert-deftest xl-dired-hex-lines-show-hex-and-an-ascii-column ()
  "Each line: an offset, the bytes in hex, and the printable characters."
  (let ((text (xl-dired-preview--hex-lines (unibyte-string ?A ?\0 ?B))))
    (should (string-match-p "^00000000  41 00 42 +|A.B|$" text))))

(ert-deftest xl-dired-listing-says-when-ls-is-missing ()
  "Without the listing program the listing is refused with a message.
The program is `insert-directory-program', which is `gls' on some Macs."
  (xl-dired-test--with-folder dir
    (let ((insert-directory-program "xl-dired-no-such-ls"))
      (should-error (xl-dired-noselect dir) :type 'user-error))
    (when-let* ((buf (get-buffer (xl-dired--buffer-name dir))))
      (kill-buffer buf))))

;; The binary check is a regexp scan with an early exit; these pin its rules.
(ert-deftest xl-dired-binary-check-follows-its-rules ()
  "NUL is binary; ordinary whitespace is not; many control characters are."
  (should-not (xl-dired-preview--binary-text-p "a\tb\nc\rd\fe\bf\n"))
  (should (xl-dired-preview--binary-text-p "ab\0cd"))
  (should (xl-dired-preview--binary-text-p (make-string 200 1)))
  (should-not (xl-dired-preview--binary-text-p
               (concat (make-string 39 ?x) "\1")))
  (should-not (xl-dired-preview--binary-text-p (make-string 1000 ?a)))
  (should-not (xl-dired-preview--binary-text-p "")))

(provide 'xl-dired-tests)
;;; xl-dired-tests.el ends here
