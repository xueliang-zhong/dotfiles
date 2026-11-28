;;; tuned-tagbar-tests.el --- ERT tests for tuned-tagbar -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/tuned-tagbar-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'tuned-tagbar)

(defmacro tuned-tagbar-tests--with-source (content &rest body)
  "Show a temporary Emacs Lisp buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(let ((source (generate-new-buffer "tuned-tagbar-test")))
     (unwind-protect
         (progn
           (with-current-buffer source
             (emacs-lisp-mode)
             (insert ,content)
             (goto-char (point-min)))
           (switch-to-buffer source)
           ,@body)
       (when (window-live-p tuned-tagbar--window)
         (tuned-tagbar-close))
       (when (buffer-live-p source)
         (kill-buffer source)))))

(defconst tuned-tagbar-tests--two-definitions
  "(defvar my-var 1)\n\n(defun my-fun ()\n  \"Doc.\"\n  (interactive)\n  (message \"hi\"))\n")

(ert-deftest tuned-tagbar-lists-sections-and-tags ()
  "The tagbar lists the imenu sections and their tags, read-only."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (should (tuned-tagbar--live-p))
    (with-current-buffer tuned-tagbar--buffer-name
      (should (eq major-mode 'tuned-tagbar-mode))
      (should buffer-read-only)
      (should (equal (mapcar (lambda (group) (plist-get group :title))
                             tuned-tagbar--groups)
                     '("Variables" "Functions")))
      (should (string-match-p "my-var" (buffer-string)))
      (should (string-match-p "my-fun" (buffer-string)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :name))
                             tuned-tagbar--entries)
                     '("my-var" "my-fun"))))))

(ert-deftest tuned-tagbar-icons-by-type ()
  "Each tag kind has a Nerd Font glyph and a Unicode fallback."
  (let ((tuned-tagbar-icon-style 'unicode))
    (should (equal (tuned-tagbar--icon 'function nil) "ƒ"))
    (should (equal (tuned-tagbar--icon 'variable nil) "𝑥"))
    (should (equal (tuned-tagbar--icon 'macro nil) "μ"))
    (should (equal (tuned-tagbar--icon 'type nil) "◈"))
    (should (equal (tuned-tagbar--icon 'package nil) "▣"))
    (should (equal (tuned-tagbar--icon 'heading nil) "§"))
    (should (equal (tuned-tagbar--icon 'unknown nil) "")))
  (let ((tuned-tagbar-icon-style 'nerd))
    (should (equal (tuned-tagbar--icon 'function t) "\xea8c")))
  (let ((tuned-tagbar-icon-style 'none))
    (should (equal (tuned-tagbar--icon 'function nil) "")))
  (should (eq (tuned-tagbar--item-kind
               (list (propertize "Functions foo" 'consult--type ?f))
               "Functions")
              'function))
  (should (eq (tuned-tagbar--item-kind
               (list (propertize "Heading" 'org-imenu t))
               "Tags")
              'heading))
  (should (eq (tuned-tagbar--kind-from-title "Classes") 'class))
  (should (eq (tuned-tagbar--kind-from-title "Macros") 'macro))
  (should (eq (tuned-tagbar--kind-from-title "Whatever") 'tag)))

(ert-deftest tuned-tagbar-renders-icons ()
  "Rendered sections and tags are prefixed with their type icon."
  (let ((tuned-tagbar-icon-style 'unicode))
    (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
      (tuned-tagbar-open)
      (with-current-buffer tuned-tagbar--buffer-name
        (should (string-match-p "▾ 𝑥 Variables" (buffer-string)))
        (should (string-match-p "  𝑥 my-var" (buffer-string)))
        (should (string-match-p "▾ ƒ Functions" (buffer-string)))
        (should (string-match-p "  ƒ my-fun" (buffer-string)))))))

(ert-deftest tuned-tagbar-jumps-to-a-tag ()
  "RET on a tag line moves to its definition in the source buffer."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (with-current-buffer tuned-tagbar--buffer-name
      (goto-char (point-min))
      (search-forward "my-fun")
      (tuned-tagbar--jump-at-point))
    (should (eq (current-buffer) (window-buffer tuned-tagbar--source-window)))
    (should (looking-at-p "(defun my-fun"))))

(ert-deftest tuned-tagbar-next-and-previous ()
  "n and p move from tag to tag and preview it in the source window."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (select-window tuned-tagbar--window)
    (with-current-buffer tuned-tagbar--buffer-name
      (goto-char (point-min))
      (tuned-tagbar-next)
      (should (eq (current-buffer) (get-buffer tuned-tagbar--buffer-name)))
      (should (with-current-buffer (window-buffer tuned-tagbar--source-window)
                (looking-at-p "(defvar my-var")))
      (tuned-tagbar-next)
      (should (with-current-buffer (window-buffer tuned-tagbar--source-window)
                (looking-at-p "(defun my-fun")))
      (tuned-tagbar-previous)
      (should (with-current-buffer (window-buffer tuned-tagbar--source-window)
                (looking-at-p "(defvar my-var"))))))

(ert-deftest tuned-tagbar-folds-a-section ()
  "TAB on a heading hides its tags, and TAB again shows them."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (with-current-buffer tuned-tagbar--buffer-name
      (goto-char (point-min))
      (tuned-tagbar--toggle-at-point)
      (should (plist-get (car tuned-tagbar--groups) :folded))
      (should (invisible-p (plist-get (car tuned-tagbar--entries) :start)))
      (tuned-tagbar--toggle-at-point)
      (should-not (plist-get (car tuned-tagbar--groups) :folded))
      (should-not (invisible-p (plist-get (car tuned-tagbar--entries) :start))))))

(ert-deftest tuned-tagbar-refreshes-after-a-change ()
  "A new definition appears after the source buffer changes."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (with-current-buffer (window-buffer tuned-tagbar--source-window)
      (goto-char (point-max))
      (insert "(defun newly-added ())\n"))
    (tuned-tagbar--refresh-if-dirty)
    (with-current-buffer tuned-tagbar--buffer-name
      (should (string-match-p "newly-added" (buffer-string))))))

(ert-deftest tuned-tagbar-highlights-the-enclosing-tag ()
  "The current overlay sits on the definition that contains point."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (with-current-buffer (window-buffer tuned-tagbar--source-window)
      (goto-char (point-max))
      (set-window-point (selected-window) (point)))
    (tuned-tagbar--update-current)
    (with-current-buffer tuned-tagbar--buffer-name
      (let* ((last-entry (car (last tuned-tagbar--entries)))
             (overlay tuned-tagbar--current-overlay)
             (icon (tuned-tagbar--icon 'function (tuned-tagbar--nerd-icons-p))))
        (should (overlayp overlay))
        (should (= (overlay-start overlay) (plist-get last-entry :start)))
        (should (equal (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay))
                       (concat "  " icon " my-fun\n")))))))

(ert-deftest tuned-tagbar-follows-a-buffer-switch ()
  "Switching the watched window's buffer re-attaches the tagbar."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (let ((other (generate-new-buffer "tuned-tagbar-other")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (emacs-lisp-mode)
              (insert "(defun other-fun ())\n"))
            (tuned-tagbar-open)
            (set-window-buffer tuned-tagbar--source-window other)
            (tuned-tagbar--post-command)
            (should (eq tuned-tagbar--source-buffer other))
            (with-current-buffer tuned-tagbar--buffer-name
              (should (string-match-p "other-fun" (buffer-string)))
              (should-not (string-match-p "my-fun" (buffer-string)))))
        (when (buffer-live-p other) (kill-buffer other))))))

(ert-deftest tuned-tagbar-toggle-opens-on-the-right-and-closes ()
  "F8's command opens a right side window and closes it again."
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-toggle)
    (should (window-live-p tuned-tagbar--window))
    (should (eq (window-parameter tuned-tagbar--window 'window-side) 'right))
    (should (= (window-width tuned-tagbar--window) tuned-tagbar-width))
    (tuned-tagbar-toggle)
    (should-not (window-live-p tuned-tagbar--window))
    (should-not (get-buffer tuned-tagbar--buffer-name))))

(ert-deftest tuned-tagbar-embark-targets-the-tag ()
  "Embark sees the tag name at point, not the whole line."
  (require 'embark)
  (tuned-tagbar-tests--with-source tuned-tagbar-tests--two-definitions
    (tuned-tagbar-open)
    (with-current-buffer tuned-tagbar--buffer-name
      (goto-char (point-min))
      (search-forward "my-fun")
      (should (equal (tuned-tagbar--embark-target) '(symbol . "my-fun"))))))

(ert-deftest tuned-tagbar-keys ()
  "The tagbar has its own jump, fold, refresh, close and Embark keys."
  (should (eq (keymap-lookup tuned-tagbar-mode-map "RET")
              #'tuned-tagbar--jump-at-point))
  (should (eq (keymap-lookup tuned-tagbar-mode-map "TAB")
              #'tuned-tagbar--toggle-at-point))
  (should (eq (keymap-lookup tuned-tagbar-mode-map "n") #'tuned-tagbar-next))
  (should (eq (keymap-lookup tuned-tagbar-mode-map "p") #'tuned-tagbar-previous))
  (should (eq (keymap-lookup tuned-tagbar-mode-map "g") #'tuned-tagbar-refresh))
  (should (eq (keymap-lookup tuned-tagbar-mode-map "q") #'tuned-tagbar-close))
  (should (eq (keymap-lookup tuned-tagbar-mode-map "C-.") #'embark-act)))

;;; tuned-tagbar-tests.el ends here
