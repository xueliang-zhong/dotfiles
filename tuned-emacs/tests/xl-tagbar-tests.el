;;; xl-tagbar-tests.el --- ERT tests for xl-tagbar -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-tagbar-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'xl-tagbar)

(defmacro xl-tagbar-tests--with-source (content &rest body)
  "Show a temporary Emacs Lisp buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(let ((source (generate-new-buffer "xl-tagbar-test")))
     (unwind-protect
         (progn
           (with-current-buffer source
             (emacs-lisp-mode)
             (insert ,content)
             (goto-char (point-min)))
           (switch-to-buffer source)
           ,@body)
       (when (window-live-p xl-tagbar--window)
         (xl-tagbar-close))
       (when (buffer-live-p source)
         (kill-buffer source)))))

(defconst xl-tagbar-tests--two-definitions
  "(defvar my-var 1)\n\n(defun my-fun ()\n  \"Doc.\"\n  (interactive)\n  (message \"hi\"))\n")

(ert-deftest xl-tagbar-lists-sections-and-tags ()
  "The tagbar lists the imenu sections and their tags, read-only."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (should (xl-tagbar--live-p))
    (with-current-buffer xl-tagbar--buffer-name
      (should (eq major-mode 'xl-tagbar-mode))
      (should buffer-read-only)
      (should (equal (mapcar (lambda (group) (plist-get group :title))
                             xl-tagbar--groups)
                     '("Variables" "Functions")))
      (should (string-match-p "my-var" (buffer-string)))
      (should (string-match-p "my-fun" (buffer-string)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :name))
                             xl-tagbar--entries)
                     '("my-var" "my-fun"))))))

(ert-deftest xl-tagbar-icons-by-type ()
  "Each tag kind has a Nerd Font glyph and a Unicode fallback."
  (let ((xl-tagbar-icon-style 'unicode))
    (should (equal (xl-tagbar--icon 'function nil) "ƒ"))
    (should (equal (xl-tagbar--icon 'variable nil) "𝑥"))
    (should (equal (xl-tagbar--icon 'macro nil) "μ"))
    (should (equal (xl-tagbar--icon 'type nil) "◈"))
    (should (equal (xl-tagbar--icon 'package nil) "▣"))
    (should (equal (xl-tagbar--icon 'heading nil) "§"))
    (should (equal (xl-tagbar--icon 'unknown nil) "")))
  (let ((xl-tagbar-icon-style 'nerd))
    (should (equal (xl-tagbar--icon 'function t) "\xea8c")))
  (let ((xl-tagbar-icon-style 'none))
    (should (equal (xl-tagbar--icon 'function nil) "")))
  (should (eq (xl-tagbar--item-kind
               (list (propertize "Functions foo" 'consult--type ?f))
               "Functions")
              'function))
  (should (eq (xl-tagbar--item-kind
               (list (propertize "Heading" 'org-imenu t))
               "Tags")
              'heading))
  (should (eq (xl-tagbar--kind-from-title "Classes") 'class))
  (should (eq (xl-tagbar--kind-from-title "Macros") 'macro))
  (should (eq (xl-tagbar--kind-from-title "Whatever") 'tag)))

(ert-deftest xl-tagbar-renders-icons ()
  "Rendered sections and tags are prefixed with their type icon."
  (let ((xl-tagbar-icon-style 'unicode))
    (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
      (xl-tagbar-open)
      (with-current-buffer xl-tagbar--buffer-name
        (should (string-match-p "▾ 𝑥 Variables" (buffer-string)))
        (should (string-match-p "  𝑥 my-var" (buffer-string)))
        (should (string-match-p "▾ ƒ Functions" (buffer-string)))
        (should (string-match-p "  ƒ my-fun" (buffer-string)))))))

(ert-deftest xl-tagbar-jumps-to-a-tag ()
  "RET on a tag line moves to its definition in the source buffer."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (with-current-buffer xl-tagbar--buffer-name
      (goto-char (point-min))
      (search-forward "my-fun")
      (xl-tagbar--jump-at-point))
    (should (eq (current-buffer) (window-buffer xl-tagbar--source-window)))
    (should (looking-at-p "(defun my-fun"))))

(ert-deftest xl-tagbar-next-and-previous ()
  "n and p move from tag to tag and preview it in the source window."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (select-window xl-tagbar--window)
    (with-current-buffer xl-tagbar--buffer-name
      (goto-char (point-min))
      (xl-tagbar-next)
      (should (eq (current-buffer) (get-buffer xl-tagbar--buffer-name)))
      (should (with-current-buffer (window-buffer xl-tagbar--source-window)
                (looking-at-p "(defvar my-var")))
      (xl-tagbar-next)
      (should (with-current-buffer (window-buffer xl-tagbar--source-window)
                (looking-at-p "(defun my-fun")))
      (xl-tagbar-previous)
      (should (with-current-buffer (window-buffer xl-tagbar--source-window)
                (looking-at-p "(defvar my-var"))))))

(ert-deftest xl-tagbar-folds-a-section ()
  "TAB on a heading hides its tags, and TAB again shows them."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (with-current-buffer xl-tagbar--buffer-name
      (goto-char (point-min))
      (xl-tagbar--toggle-at-point)
      (should (plist-get (car xl-tagbar--groups) :folded))
      (should (invisible-p (plist-get (car xl-tagbar--entries) :start)))
      (xl-tagbar--toggle-at-point)
      (should-not (plist-get (car xl-tagbar--groups) :folded))
      (should-not (invisible-p (plist-get (car xl-tagbar--entries) :start))))))

(ert-deftest xl-tagbar-refreshes-after-a-change ()
  "A new definition appears after the source buffer changes."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (with-current-buffer (window-buffer xl-tagbar--source-window)
      (goto-char (point-max))
      (insert "(defun newly-added ())\n"))
    (xl-tagbar--refresh-if-dirty)
    (with-current-buffer xl-tagbar--buffer-name
      (should (string-match-p "newly-added" (buffer-string))))))

(ert-deftest xl-tagbar-highlights-the-enclosing-tag ()
  "The current overlay sits on the definition that contains point."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (with-current-buffer (window-buffer xl-tagbar--source-window)
      (goto-char (point-max))
      (set-window-point (selected-window) (point)))
    (xl-tagbar--update-current)
    (with-current-buffer xl-tagbar--buffer-name
      (let* ((last-entry (car (last xl-tagbar--entries)))
             (overlay xl-tagbar--current-overlay)
             (icon (xl-tagbar--icon 'function (xl-tagbar--nerd-icons-p))))
        (should (overlayp overlay))
        (should (= (overlay-start overlay) (plist-get last-entry :start)))
        (should (equal (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay))
                       (concat "  " icon " my-fun\n")))))))

(ert-deftest xl-tagbar-follows-a-buffer-switch ()
  "Switching the watched window's buffer re-attaches the tagbar."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (let ((other (generate-new-buffer "xl-tagbar-other")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (emacs-lisp-mode)
              (insert "(defun other-fun ())\n"))
            (xl-tagbar-open)
            (set-window-buffer xl-tagbar--source-window other)
            (xl-tagbar--post-command)
            (should (eq xl-tagbar--source-buffer other))
            (with-current-buffer xl-tagbar--buffer-name
              (should (string-match-p "other-fun" (buffer-string)))
              (should-not (string-match-p "my-fun" (buffer-string)))))
        (when (buffer-live-p other) (kill-buffer other))))))

(ert-deftest xl-tagbar-toggle-opens-on-the-right-and-closes ()
  "F8's command opens a right side window and closes it again."
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-toggle)
    (should (window-live-p xl-tagbar--window))
    (should (eq (window-parameter xl-tagbar--window 'window-side) 'right))
    (should (= (window-width xl-tagbar--window) xl-tagbar-width))
    (xl-tagbar-toggle)
    (should-not (window-live-p xl-tagbar--window))
    (should-not (get-buffer xl-tagbar--buffer-name))))

(ert-deftest xl-tagbar-embark-targets-the-tag ()
  "Embark sees the tag name at point, not the whole line."
  (require 'embark)
  (xl-tagbar-tests--with-source xl-tagbar-tests--two-definitions
    (xl-tagbar-open)
    (with-current-buffer xl-tagbar--buffer-name
      (goto-char (point-min))
      (search-forward "my-fun")
      (should (equal (xl-tagbar--embark-target) '(symbol . "my-fun"))))))

(ert-deftest xl-tagbar-keys ()
  "The tagbar has its own jump, fold, refresh, close and Embark keys."
  (should (eq (keymap-lookup xl-tagbar-mode-map "RET")
              #'xl-tagbar--jump-at-point))
  (should (eq (keymap-lookup xl-tagbar-mode-map "TAB")
              #'xl-tagbar--toggle-at-point))
  (should (eq (keymap-lookup xl-tagbar-mode-map "n") #'xl-tagbar-next))
  (should (eq (keymap-lookup xl-tagbar-mode-map "p") #'xl-tagbar-previous))
  (should (eq (keymap-lookup xl-tagbar-mode-map "g") #'xl-tagbar-refresh))
  (should (eq (keymap-lookup xl-tagbar-mode-map "q") #'xl-tagbar-close))
  (should (eq (keymap-lookup xl-tagbar-mode-map "C-.") #'embark-act)))

;;; xl-tagbar-tests.el ends here
