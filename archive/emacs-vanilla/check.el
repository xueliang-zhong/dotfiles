;;; check.el --- Batch checks for the standalone init.el -*- lexical-binding: t; -*-
;;
;; Run after loading emacs/vanilla/init.el:
;;   emacs --batch --init-directory ~/.emacs-profiles/vanilla \
;;     -l ~/.emacs-profiles/vanilla/init.el \
;;     -l ~/workspace/dotfiles/emacs/vanilla/check.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'seq)

(defun xl-check--collect-commands (map)
  "Return every command bound in MAP, descending into prefix keymaps."
  (let (commands)
    (map-keymap
     (lambda (_event def)
       (cond
        ((keymapp def)
         (setq commands (append (xl-check--collect-commands def) commands)))
        ((and (consp def) (keymapp (cdr def)))
         (setq commands (append (xl-check--collect-commands (cdr def)) commands)))
        ((and (consp def) (eq (car def) 'menu-item))
         (let ((cmd (nth 2 def)))
           (cond ((keymapp cmd)
                  (setq commands (append (xl-check--collect-commands cmd) commands)))
                 ((commandp cmd) (push cmd commands)))))
        ((and (consp def) (commandp (cdr def)))
         (push (cdr def) commands))
        ((commandp def) (push def commands))))
     map)
    commands))

(ert-deftest xl-check-profile-is-isolated ()
  (should (equal user-emacs-directory
                 (expand-file-name "~/.emacs-profiles/vanilla/")))
  (should (string-prefix-p user-emacs-directory package-user-dir))
  (should (file-directory-p package-user-dir)))

(ert-deftest xl-check-packages-installed ()
  (dolist (pkg xl-packages)
    (should (memq pkg package-selected-packages))
    (should (package-installed-p pkg))))

(ert-deftest xl-check-modes-on ()
  (should (bound-and-true-p evil-mode))
  (should (bound-and-true-p which-key-mode))
  (should (bound-and-true-p vertico-mode))
  (should (bound-and-true-p marginalia-mode))
  (should (bound-and-true-p global-corfu-mode))
  (should (bound-and-true-p savehist-mode))
  (should (bound-and-true-p recentf-mode))
  (should (bound-and-true-p global-auto-revert-mode))
  (should (bound-and-true-p winner-mode))
  (should (bound-and-true-p save-place-mode))
  (should (bound-and-true-p column-number-mode)))

(ert-deftest xl-check-font-settings ()
  (should (eq (default-value 'indent-tabs-mode) nil))
  (should (eq (default-value 'tab-width) 2))
  (should (eq (default-value 'truncate-lines) t))
  (should (eq (default-value 'bidi-display-reordering) nil)))

(ert-deftest xl-check-evil-leader ()
  (should (eq (keymap-lookup evil-normal-state-map "SPC") xl-leader-map))
  (should (eq (keymap-lookup evil-motion-state-map "SPC") xl-leader-map))
  (should (eq (keymap-lookup evil-visual-state-map "SPC") xl-leader-map))
  (should (eq (keymap-lookup evil-insert-state-map "M-m") xl-leader-map))
  (should (eq (keymap-lookup evil-emacs-state-map "M-m") xl-leader-map)))

(ert-deftest xl-check-leader-bindings ()
  (dolist (spec '(("SPC" . consult-buffer)
                  ("RET" . consult-buffer)
                  ("*" . xueliang-search-project-at-point)
                  ("/" . consult-line)
                  ("x" . execute-extended-command)
                  ("f f" . find-file)
                  ("f r" . consult-recent-file)
                  ("f p" . xueliang-find-file-in-dotfiles)
                  ("i s" . xueliang-insert-snippet)
                  ("i S" . xueliang-new-snippet)
                  ("b b" . consult-buffer)
                  ("b h" . xl-sk-home-buffer)
                  ("b s" . xueliang-open-scratch-buffer-window)
                  ("g s" . magit-status)
                  ("g g" . xueliang-magit-status-window)
                  ("g f" . project-find-file)
                  ("j j" . avy-goto-char-timer)
                  ("p s" . consult-ripgrep)
                  ("r l" . vertico-repeat)
                  ("r y" . consult-yank-pop)
                  ("s s" . consult-line)
                  ("w v" . xueliang-split-right)
                  ("w s" . xueliang-split-below)
                  ("T n" . xl-sk-cycle-theme)
                  ("1" . winum-select-window-1)
                  ("9" . winum-select-window-9)))
    (should (eq (keymap-lookup xl-leader-map (car spec)) (cdr spec)))))

(ert-deftest xl-check-leader-commands-exist ()
  (let ((commands (delete-dups (xl-check--collect-commands xl-leader-map))))
    (should (> (length commands) 80))
    (dolist (cmd commands)
      (should (commandp cmd))
      (should (fboundp cmd)))))

(ert-deftest xl-check-major-leader ()
  (with-temp-buffer
    (org-mode)
    (should (eq (xl-sk--major-map) xl-org-leader-map)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (eq (xl-sk--major-map) xl-elisp-leader-map)))
  (with-temp-buffer
    (fundamental-mode)
    (should-not (xl-sk--major-map))))

(ert-deftest xl-check-uk-hash-key ()
  (should (eq (keymap-lookup evil-insert-state-map "M-3")
              'xueliang-insert-hash))
  (should (eq (keymap-lookup minibuffer-local-map "M-3")
              'xueliang-insert-hash))
  (should (eq (keymap-lookup isearch-mode-map "M-3")
              'xueliang-isearch-insert-hash)))

(ert-deftest xl-check-org-setup ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (should (bound-and-true-p org-indent-mode))
    (should (bound-and-true-p org-superstar-mode))
    (should (bound-and-true-p evil-org-mode))
    (should (eq org-log-done nil))
    (should (equal org-todo-keywords
                   '((sequence "FOCUS(f)" "TODO(t)" "STORY(s)"
                               "PROG(p)" "|" "DONE(d)"))))
    (should (fboundp 'org-tempo-complete-tag))))

(ert-deftest xl-check-dired-setup ()
  (require 'dired)
  (with-temp-buffer
    (dired-mode)
    (should (eq (keymap-lookup dired-mode-map "TAB") 'dired-display-file))))

(ert-deftest xl-check-eshell-aliases ()
  (should (eq (symbol-function 'eshell/e) 'find-file-other-window))
  (should (eq (symbol-function 'eshell/vi) 'eshell/e)))

(ert-deftest xl-check-home-buffer ()
  (let ((buf (xl-sk-home-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'special-mode))
          (should (string-match-p "Emacs .* standalone init.el test" (buffer-string)))
          (should (string-match-p "Open a File" (buffer-string)))
          (should (keymapp evil-normal-state-local-map))
          (should (commandp (keymap-lookup evil-normal-state-local-map "1")))
          (should (commandp (keymap-lookup evil-normal-state-local-map "9"))))
      (kill-buffer buf))))

(ert-deftest xl-check-home-recent-file-key ()
  (let* ((dir (make-temp-file "xl-vanilla-home-" t))
         (file (expand-file-name "recent-note.txt" dir))
         (old-recent recentf-list))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\n"))
          (setq recentf-list (cons file recentf-list))
          (let ((home (xl-sk-home-buffer)))
            (unwind-protect
                (with-current-buffer home
                  (should (equal (car xl-sk--home-files)
                                 (abbreviate-file-name file)))
                  (call-interactively
                   (keymap-lookup evil-normal-state-local-map "1"))
                  (should (equal buffer-file-name file)))
              (kill-buffer home)
              (when-let* ((buf (find-buffer-visiting file)))
                (kill-buffer buf)))))
      (setq recentf-list old-recent)
      (ignore-errors (delete-directory dir t)))))

(ert-deftest xl-check-initial-buffer-choice ()
  (should (eq initial-buffer-choice 'xl-sk-home-buffer)))

(ert-deftest xl-check-uk-hash-inserts ()
  (with-temp-buffer
    (xueliang-insert-hash)
    (should (equal (buffer-string) "#"))))

(ert-deftest xl-check-tab-command ()
  (with-temp-buffer
    (fundamental-mode)
    (xueliang-tab-or-corfu-next)
    (should (equal (buffer-string) "  "))))

(ert-deftest xl-check-theme ()
  (should (or (memq 'spacemacs-dark custom-enabled-themes)
              (memq 'doom-tokyo-night custom-enabled-themes))))

(ert-deftest xl-check-org-agenda-files ()
  (should (equal org-directory "~/org"))
  (should (equal org-agenda-files (list "~/org"))))

(ert-deftest xl-check-org-tempo-expands ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "<s")
    (xueliang-tab-or-corfu-next)
    (should (string-match-p "#\\+begin_src" (buffer-string)))
    (should (string-match-p "#\\+end_src" (buffer-string)))))

(ert-deftest xl-check-org-evil-ret ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (should (eq (keymap-lookup
                 (evil-get-auxiliary-keymap org-mode-map 'normal t)
                 "RET")
                #'org-open-at-point))))

(ert-deftest xl-check-dired-evil-keys ()
  (require 'dired)
  (with-temp-buffer
    (dired-mode)
    (let ((map (evil-get-auxiliary-keymap dired-mode-map 'normal t)))
      (should (eq (keymap-lookup map "h") #'dired-up-directory))
      (should (eq (keymap-lookup map "l") #'xueliang-dired-open))
      (should (eq (keymap-lookup map "RET") #'xueliang-dired-open)))))

(ert-deftest xl-check-eshell-mode-keys ()
  (require 'eshell)
  (with-temp-buffer
    (eshell-mode)
    (should (eq (keymap-lookup evil-insert-state-local-map "RET")
                #'eshell-send-input))
    (should (eq (keymap-lookup evil-insert-state-local-map "C-a")
                #'eshell-bol))))

(ert-deftest xl-check-magit-pruning ()
  (require 'magit-status)
  (dolist (fn '(magit-insert-stashes
                magit-insert-merge-log
                magit-insert-unpulled-from-upstream
                magit-insert-unpulled-from-pushremote
                magit-insert-unpushed-to-pushremote
                magit-insert-rebase-sequence))
    (should-not (memq fn magit-status-sections-hook)))
  (should (memq 'magit-insert-unpushed-to-upstream-or-recent
                magit-status-sections-hook))
  (dolist (fn '(magit-insert-tags-header
                magit-insert-diff-filter-header
                magit-insert-upstream-branch-header
                magit-insert-push-branch-header))
    (should-not (memq fn magit-status-headers-hook)))
  (should (memq 'magit-insert-head-branch-header
                magit-status-headers-hook))
  (should (eq (keymap-lookup magit-mode-map "h") #'evil-backward-char))
  (should (eq (keymap-lookup magit-mode-map "j") #'magit-next-line))
  (should (eq (keymap-lookup magit-mode-map "k") #'magit-previous-line))
  (should (eq (keymap-lookup magit-mode-map "l") #'evil-forward-char)))

(ert-deftest xl-check-magit-status-opens ()
  (skip-unless (executable-find "git"))
  (let* ((dir (make-temp-file "xl-vanilla-git-" t))
         (git (executable-find "git")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" dir)
            (insert "a\n"))
          (call-process git nil nil nil "-C" dir "init" "-q")
          (call-process git nil nil nil "-C" dir "-c" "user.email=t@e"
                        "-c" "user.name=T" "add" ".")
          (call-process git nil nil nil "-C" dir "-c" "user.email=t@e"
                        "-c" "user.name=T" "commit" "-qm" "init")
          (let ((buf (magit-status-setup-buffer dir)))
            (unwind-protect
                (with-current-buffer buf
                  (should (derived-mode-p 'magit-status-mode))
                  (should (save-excursion
                            (goto-char (point-min))
                            (search-forward "Head:" nil t))))
              (kill-buffer buf))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest xl-check-diff-hl ()
  (require 'diff-hl)
  (should (bound-and-true-p global-diff-hl-mode)))

(ert-deftest xl-check-evil-extras ()
  (should (bound-and-true-p evil-escape-mode))
  (should (bound-and-true-p global-evil-surround-mode))
  (should (bound-and-true-p winum-mode))
  (should (equal evil-escape-key-sequence "fd"))
  (should (eq evil-undo-system 'undo-redo)))

(ert-deftest xl-check-corfu-settings ()
  (should corfu-auto)
  (should (= corfu-auto-prefix 2))
  (should (memq #'xueliang-tab-or-corfu-next corfu-continue-commands))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (functionp (car completion-at-point-functions)))))

(ert-deftest xl-check-theme-cycle ()
  (unwind-protect
      (progn
        (xl-sk-cycle-theme)
        (should (memq 'doom-tokyo-night custom-enabled-themes))
        (should (facep 'corfu-default)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'spacemacs-dark t)))

;;; check.el ends here
