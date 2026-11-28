;;; xl-git-commands-tests.el --- ERT tests for xl-git-commands -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-git-commands-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'xl-async)
(require 'xl-spinner)
(require 'xl-git-status)
(require 'xl-git-commands)

(defun xl-test--git-repo ()
  "Create a temporary git repo with commits, edits and an untracked file."
  (let ((dir (make-temp-file "xl-test-git-" t))
        (git (executable-find "git")))
    (unless git
      (error "git is not installed"))
    (cl-flet ((run (&rest args)
                (apply #'call-process git nil nil nil
                       "-C" dir "-c" "user.email=test@example.com"
                       "-c" "user.name=Test" args)))
      (run "init" "-q")
      (dotimes (i 8)
        (with-temp-file (expand-file-name (format "file-%d.txt" i) dir)
          (insert (format "line %d\n" i))))
      (run "add" ".")
      (run "commit" "-qm" "initial")
      (dotimes (i 3)
        (with-temp-file (expand-file-name (format "file-%d.txt" i) dir)
          (insert "changed\n")))
      (with-temp-file (expand-file-name "untracked.txt" dir)
        (insert "new\n")))
    dir))
(defun xl-test--git-repo-hunks ()
  "Create a repository whose file.txt has two separate unstaged hunks."
  (let ((dir (make-temp-file "xl-test-git-hunks-" t))
        (git (executable-find "git")))
    (unless git
      (error "git is not installed"))
    (cl-flet ((run (&rest args)
                (apply #'call-process git nil nil nil
                       "-C" dir "-c" "user.email=test@example.com"
                       "-c" "user.name=Test" args)))
      (run "init" "-q")
      (with-temp-file (expand-file-name "file.txt" dir)
        (dotimes (i 20)
          (insert (format "line %d\n" i))))
      (run "add" "file.txt")
      (run "commit" "-qm" "initial")
      ;; Lines 1 and 18 are far enough apart to stay in separate hunks.
      (with-temp-file (expand-file-name "file.txt" dir)
        (dotimes (i 20)
          (insert (if (memq i '(1 18))
                      (format "changed %d\n" i)
                    (format "line %d\n" i))))))
    dir))
(defun xl-test--git-repo-blame ()
  "Create a repository where file.txt spans two commits plus one edit."
  (let ((dir (make-temp-file "xl-test-git-blame-" t))
        (git (executable-find "git")))
    (unless git
      (error "git is not installed"))
    (cl-flet ((run (&rest args)
                (apply #'call-process git nil nil nil
                       "-C" dir "-c" "user.email=test@example.com"
                       "-c" "user.name=Test" args)))
      (run "init" "-q")
      (with-temp-file (expand-file-name "file.txt" dir)
        (insert "one\ntwo\nthree\nfour\n"))
      (run "add" "file.txt")
      (run "commit" "-qm" "initial")
      ;; A second commit owns the last two lines.
      (with-temp-file (expand-file-name "file.txt" dir)
        (insert "one\ntwo\nTHREE\nFOUR\n"))
      (run "commit" "-qam" "second")
      ;; The first line is then edited but not committed.
      (with-temp-file (expand-file-name "file.txt" dir)
        (insert "ONE\ntwo\nTHREE\nFOUR\n")))
    dir))
(defun xl-test--wait-for-gblame (buffer)
  "Wait until the GBlame BUFFER has finished loading."
  (with-current-buffer buffer
    (let ((end (+ (float-time) 10)))
      (while (and (xl-async-busy-p 'blame buffer)
                  (< (float-time) end))
        (accept-process-output nil 0.05)))
    (should-not (xl-async-busy-p 'blame buffer))))
(defun xl-test--wait-for-slot (buffer)
  "Wait until BUFFER's mode-line loader has gone, minimum time included."
  (let ((end (+ (float-time) 10)))
    (while (and (xl-spinner-mode-line-busy-p buffer)
                (< (float-time) end))
      (accept-process-output nil 0.05)))
  (should-not (xl-spinner-mode-line-busy-p buffer)))

(ert-deftest xl-gblame-shows-and-syncs ()
  "GBlame lists one blame line per source line and follows the window."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-hunks))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (should (buffer-live-p xl-gblame--buffer))
          (should (get-buffer-window xl-gblame--buffer))
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            (should (derived-mode-p 'xl-gblame-mode))
            (should buffer-read-only)
            ;; One line of blame for each of the 20 source lines.
            (should (= 20 (how-many "\n" (point-min) (point-max))))
            ;; The first commit gets the first palette face.
            (should (eq (get-text-property (point-min) 'face)
                        (car xl-gblame-hash-faces)))
            (should (string-match-p
                     "\\`[0-9a-f]\\{8\\}"
                     (buffer-substring (point-min) (+ (point-min) 8))))
            (goto-char (point-min))
            (should (search-forward "Test" (line-end-position) t))
            ;; The working-tree change on line 2 has no commit yet.
            (goto-char (point-min))
            (forward-line 1)
            (should (string-match-p
                     "\\`0+\\'" (get-text-property (point) 'xl-gblame-hash)))
            (should (eq (get-text-property (point) 'face)
                        'xl-git-status-comment))
            (should-error (xl-gblame-show-commit) :type 'user-error))
          ;; Moving point moves the highlight; scrolling moves the window.
          (goto-char (point-min))
          (forward-line 9)
          (xl-gblame--sync (get-buffer-window buf t))
          (with-current-buffer xl-gblame--buffer
            (should (= 10 (line-number-at-pos
                           (overlay-start xl-gblame--highlight)))))
          (with-current-buffer buf
            (set-window-start
             (get-buffer-window buf t)
             (save-excursion (goto-char (point-min)) (forward-line 14) (point)))
            (xl-gblame--sync (get-buffer-window buf t)))
          (with-current-buffer xl-gblame--buffer
            (let ((window (get-buffer-window xl-gblame--buffer t)))
              (should (= 15 (line-number-at-pos (window-start window))))
              ;; The blame window's point stays inside the visible lines,
              ;; so redisplay does not scroll it back to point-min.
              (should (<= 15 (line-number-at-pos (window-point window))))
              (should (< (line-number-at-pos (window-point window))
                         (+ 15 (window-body-height window))))))
          ;; RET shows the commit of a committed line.
          (let (shown)
            (cl-letf (((symbol-function 'xl-git-status--show-commit)
                       (lambda (hash) (setq shown hash))))
              (with-current-buffer xl-gblame--buffer
                (goto-char (point-min))
                (xl-gblame-show-commit))
              (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'" shown))))
          ;; :Gblame is the vim-fugitive command name.
          (should (eq (cdr (assoc "Gblame" evil-ex-commands)) 'xl-gblame))
          ;; The G* family lives in xl-git-commands.el, not in example/xueliang.el.
          (dolist (cmd '(Gstatus Gwrite Gcommit xl-gblame))
            (should (equal "xl-git-commands"
                           (file-name-sans-extension
                            (file-name-nondirectory (symbol-file cmd 'defun))))))
          (should (eq (cdr (assoc "Gstatus" evil-ex-commands)) 'Gstatus))
          (should (eq (cdr (assoc "Gwrite" evil-ex-commands)) 'Gwrite))
          (should (eq (cdr (assoc "Gcommit" evil-ex-commands)) 'Gcommit))
          ;; The blame window is an ordinary split, not an Emacs side window,
          ;; and it is not hidden from `other-window' (C-w C-w).
          (let ((window (get-buffer-window xl-gblame--buffer t)))
            (should-not (window-parameter window 'window-side))
            (should-not (window-parameter window 'no-other-window))
            (should (memq window (window-list))))
          ;; :only (delete-other-windows) closes it; SPC g b opens it again.
          (select-window (get-buffer-window buf t))
          (delete-other-windows)
          (should-not (get-buffer-window xl-gblame--buffer t))
          (xl-gblame)
          (should (get-buffer-window xl-gblame--buffer t))
          ;; Toggling closes the window and removes the source hooks.
          (xl-gblame)
          (should-not (buffer-live-p xl-gblame--buffer))
          (should-not (memq #'xl-gblame--sync post-command-hook))
          (should-not (memq #'xl-gblame--refresh after-save-hook)))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-blank-window-shows-a-centered-loader ()
  "A first GBlame shows the loader in the middle of its blank window too.
The loader is a before-string: no line is added, so the placeholders stay
aligned.  It goes as soon as the blame is in, and a refresh of a window
that shows blame has the mode-line loader only."
  (skip-unless (executable-find "git"))
  (let* ((xl-spinner-show-delay 0)
         (repo (xl-test--git-repo-blame))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (let ((blame xl-gblame--buffer))
            (with-current-buffer blame
              (should xl-gblame--blank)
              ;; The window exists now; the next frame draws in it.
              (xl-spinner-tick (current-buffer))
              (should (overlayp xl-gblame--center))
              (let ((first (overlay-get xl-gblame--center 'before-string)))
                (should (string-match-p "git blame…" first))
                (should (= (overlay-start xl-gblame--center)
                           (overlay-end xl-gblame--center)))
                (xl-spinner-tick (current-buffer))
                (should-not (equal first (overlay-get xl-gblame--center
                                                      'before-string))))
              ;; One placeholder per source line: nothing was inserted.
              (should (= (count-lines (point-min) (point-max))
                         (with-current-buffer buf
                           (count-lines (point-min) (point-max))))))
            (xl-test--wait-for-gblame blame)
            (with-current-buffer blame
              (should-not xl-gblame--blank)
              (should-not xl-gblame--center)
              ;; A refresh keeps the blame on screen: no middle loader.
              (xl-gblame-refresh)
              (xl-spinner-tick (current-buffer))
              (should-not xl-gblame--center))
            (xl-test--wait-for-gblame blame)))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-loads-asynchronously ()
  "GBlame returns at once: placeholders and a loader, then the blame."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-blame))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (with-current-buffer xl-gblame--buffer
            ;; The command returned before git did: the UI was not blocked,
            ;; and the placeholders already line up with the source file.
            (should (xl-spinner-mode-line-busy-p))
            (should (equal '(:eval xl-spinner-mode-line)
                           (car mode-line-format)))
            (should (= (count-lines (point-min) (point-max))
                       (with-current-buffer buf
                         (count-lines (point-min) (point-max)))))
            (should-not (get-text-property (point-min) 'xl-gblame-hash)))
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (xl-test--wait-for-slot xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            ;; Done: the loader is gone and the blame is in place.
            (should-not xl-spinner-mode-line)
            (should-not (xl-spinner-running-p))
            (should (get-text-property (point-min) 'xl-gblame-hash))))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-reports-failures-in-the-mode-line ()
  "A failed `git blame' keeps the error at the left of the mode line."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo))
         (buf (find-file-noselect (expand-file-name "untracked.txt" repo))))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (xl-gblame)
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            (should (string-match-p "git blame failed" xl-spinner-mode-line))
            (should-not (xl-spinner-running-p))
            ;; The placeholders stay, so the window remains aligned.
            (should (= (count-lines (point-min) (point-max))
                       (with-current-buffer buf
                         (count-lines (point-min) (point-max)))))))
      (let ((blame (get-buffer "*GBlame: untracked.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))

(ert-deftest xl-gblame-colours-different-commits ()
  "Neighbouring commits get different hash faces; one commit keeps one."
  (skip-unless (executable-find "git"))
  (let* ((repo (xl-test--git-repo-blame))
         (file (expand-file-name "file.txt" repo))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (xl-gblame)
          (xl-test--wait-for-gblame xl-gblame--buffer)
          (with-current-buffer xl-gblame--buffer
            (let (faces)
              (goto-char (point-min))
              (dotimes (_ 4)
                (push (get-text-property (point) 'face) faces)
                (forward-line 1))
              (setq faces (nreverse faces))
              ;; Line 1 is uncommitted, line 2 is the initial commit,
              ;; lines 3 and 4 belong to the second commit.
              (should (eq (nth 0 faces) 'xl-git-status-comment))
              (should (memq (nth 1 faces) xl-gblame-hash-faces))
              (should (memq (nth 2 faces) xl-gblame-hash-faces))
              ;; Different adjacent commits use different colours...
              (should-not (eq (nth 1 faces) (nth 2 faces)))
              ;; ...while one commit keeps one colour.
              (should (eq (nth 2 faces) (nth 3 faces))))))
      (let ((blame (get-buffer "*GBlame: file.txt*")))
        (when (buffer-live-p blame) (kill-buffer blame)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory repo t))))


