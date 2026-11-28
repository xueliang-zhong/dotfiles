;;; xl-tagbar.el --- Tagbar-like imenu sidebar  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (consult "3.0"))
;; Keywords: convenience, tools

;;; Commentary:
;; A Tagbar for Emacs: a narrow side window on the right that lists the
;; imenu tags of the buffer shown in the window it watches.  The tags,
;; their sections and their faces come from `consult-imenu'
;; (`consult-imenu--items'), so every mode with an imenu backend works:
;; Emacs Lisp functions and variables, Org headings, and so on.
;; `consult-imenu--jump' does the jumping, and Embark (`C-.') acts on the
;; tag at point.
;;
;; F8 toggles the sidebar (`xl-tagbar-toggle').  The list follows edits
;; after a short idle delay, saves and buffer switches.  The item that
;; encloses point is highlighted, and the sidebar scrolls to keep it
;; visible.
;;
;; Keys in the tagbar:
;;   RET, mouse-1  jump to the tag
;;   n, p          next and previous tag
;;   TAB           fold or unfold the section heading at point
;;   g             refresh
;;   q             close
;;   C-.           Embark actions on the tag
;;
;; The module is an adapter: it needs Consult for the tags and Embark
;; only for `C-.'.

;;; Code:

(require 'cl-lib)
(require 'consult-imenu)
(require 'seq)
(require 'subr-x)

(defgroup xl-tagbar nil
  "Tagbar-like imenu sidebar."
  :group 'tools
  :prefix "xl-tagbar-")

(defcustom xl-tagbar-side 'right
  "Side of the frame on which the tagbar is shown."
  :type '(choice (const right) (const left))
  :group 'xl-tagbar)

(defcustom xl-tagbar-width 34
  "Width of the tagbar side window, in columns."
  :type 'integer
  :group 'xl-tagbar)

(defcustom xl-tagbar-refresh-delay 0.3
  "Seconds of idle time before changed tags are rebuilt."
  :type 'number
  :group 'xl-tagbar)

(defcustom xl-tagbar-follow-point t
  "Non-nil highlights and reveals the tag that encloses point."
  :type 'boolean
  :group 'xl-tagbar)

(defface xl-tagbar-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headings in the tagbar."
  :group 'xl-tagbar)

(defface xl-tagbar-current
  '((t :inherit highlight :extend t))
  "Face for the tag that encloses point."
  :group 'xl-tagbar)

(defcustom xl-tagbar-icon-style 'auto
  "Which icons to draw before tag names.
`auto' uses the Nerd Font icons when `xl-tagbar-nerd-font' is
installed and the frame can display it, and plain Unicode symbols
otherwise.  `nerd' and `unicode' force one of the two, and `none'
shows no icons."
  :type '(choice (const :tag "Auto (Nerd Font when available)" auto)
                 (const :tag "Nerd Font icons" nerd)
                 (const :tag "Unicode symbols" unicode)
                 (const :tag "No icons" none))
  :group 'xl-tagbar)

(defcustom xl-tagbar-nerd-font "Symbols Nerd Font Mono"
  "Font family used for the Nerd Font icon glyphs.
`Symbols Nerd Font Mono' is the icon-only font that ships with
Nerd Fonts; a patched programming font such as
`JetBrainsMono Nerd Font' works too."
  :type 'string
  :group 'xl-tagbar)

(defcustom xl-tagbar-icons
  '((function  "\xea8c" "ƒ")
    (method    "\xea8c" "ƒ")
    (macro     "\xf12b" "μ")
    (variable  "\xea88" "𝑥")
    (field     "\xeb5f" "▪")
    (property  "\xeb65" "▪")
    (constant  "\xeb5d" "≡")
    (type      "\xeb5b" "◈")
    (class     "\xeb5b" "◈")
    (interface "\xeb61" "◇")
    (enum      "\xea95" "≡")
    (struct    "\xea91" "◈")
    (namespace "\xea8b" "▦")
    (module    "\xf1b2" "▣")
    (package   "\xf1b2" "▣")
    (heading   "\xf02d" "§")
    (tag       "\xf02b" "•"))
  "Icons per tag kind, as (KIND NERD-FONT UNICODE).
The kind is chosen from the `consult-imenu' type key or from the
section title.  The Nerd Font glyph is used when the icon font is
available, and the Unicode symbol is the fallback."
  :type '(repeat (list symbol string string))
  :group 'xl-tagbar)

(defconst xl-tagbar--buffer-name "*Tagbar*"
  "Name of the single tagbar buffer.")

(defvar xl-tagbar--window nil
  "The tagbar window while it is open.")

(defvar xl-tagbar--source-window nil
  "The window whose buffer the tagbar describes.")

(defvar xl-tagbar--source-buffer nil
  "The buffer the tagbar is attached to.")

(defvar xl-tagbar--refresh-timer nil
  "Pending idle refresh of the tagbar.")

(defvar-local xl-tagbar--entries nil
  "Rendered tags, as plists with :item, :start and :end.")

(defvar-local xl-tagbar--groups nil
  "Rendered sections, as plists with :title, :start, :end and fold state.")

(defvar-local xl-tagbar--folded nil
  "Hash table of section titles that are folded.")

(defvar-local xl-tagbar--current-overlay nil
  "Overlay that marks the tag enclosing point.")

(defvar-local xl-tagbar--rendered-tick nil
  "Source `buffer-chars-modified-tick' of the last render.")

(defvar-local xl-tagbar--last-point nil
  "Source point at the last current-tag update.")

(declare-function consult-imenu--group "consult-imenu")
(declare-function consult-imenu--items-safe "consult-imenu")
(declare-function consult-imenu--jump "consult-imenu")
(declare-function consult-imenu--narrow "consult-imenu")

;;; Buttons and keys

(define-button-type 'xl-tagbar-item-button
  'action #'xl-tagbar--button-jump
  'follow-link t
  'help-echo "Jump to this tag")

(define-button-type 'xl-tagbar-group-button
  'action #'xl-tagbar--button-toggle
  'follow-link t
  'face 'xl-tagbar-heading
  'help-echo "Fold or unfold this section")

(defvar-keymap xl-tagbar-heading-map
  :doc "Keys on a tagbar section heading."
  "TAB" #'xl-tagbar--toggle-at-point
  "<backtab>" #'xl-tagbar--toggle-at-point)

(defvar-keymap xl-tagbar-mode-map
  :doc "Keymap for `xl-tagbar-mode'."
  :parent special-mode-map
  "RET" #'xl-tagbar--jump-at-point
  "TAB" #'xl-tagbar--toggle-at-point
  "n" #'xl-tagbar-next
  "p" #'xl-tagbar-previous
  "g" #'xl-tagbar-refresh
  "q" #'xl-tagbar-close
  "C-." #'embark-act)

(define-derived-mode xl-tagbar-mode special-mode "Tagbar"
  "Major mode for the Tagbar sidebar.

The buffer is read-only and lists the imenu tags of the source
buffer.  See `xl-tagbar-toggle' for the commands."
  :group 'xl-tagbar
  (setq-local truncate-lines t)
  (setq-local buffer-invisibility-spec '(t))
  (setq-local xl-tagbar--folded (make-hash-table :test #'equal))
  (setq-local xl-tagbar--current-overlay
              (let ((overlay (make-overlay 1 1)))
                (overlay-put overlay 'face 'xl-tagbar-current)
                (overlay-put overlay 'priority 1)
                overlay))
  (add-hook 'kill-buffer-hook #'xl-tagbar--buffer-killed nil t))

;;; Small helpers

(defun xl-tagbar--buffer ()
  "Return the live tagbar buffer, or nil."
  (let ((buffer (get-buffer xl-tagbar--buffer-name)))
    (and (buffer-live-p buffer) buffer)))

(defun xl-tagbar--live-p ()
  "Return non-nil when the tagbar is open and watching a live buffer."
  (and (window-live-p xl-tagbar--window)
       (window-live-p xl-tagbar--source-window)
       (eq (window-buffer xl-tagbar--window) (xl-tagbar--buffer))
       (buffer-live-p (window-buffer xl-tagbar--source-window))))

(defun xl-tagbar--item-position (item)
  "Return the buffer position of ITEM, or nil."
  (let ((target (cdr item)))
    (cond
     ((markerp target) (marker-position target))
     ((consp target) (let ((pos (car target)))
                       (if (markerp pos) (marker-position pos) pos)))
     ((integerp target) target))))

(defun xl-tagbar--entry-at-point ()
  "Return the rendered tag on the current line, or nil.
Tags always start a line, so the line start is enough."
  (seq-find (lambda (entry)
              (eql (plist-get entry :start) (line-beginning-position)))
            xl-tagbar--entries))

(defun xl-tagbar--group-at-point ()
  "Return the rendered section whose heading is on the current line."
  (seq-find (lambda (group)
              (eql (plist-get group :heading) (line-beginning-position)))
            xl-tagbar--groups))

(defun xl-tagbar--item-face (string)
  "Return a usable face from imenu candidate STRING."
  (let ((face (get-text-property 0 'face string)))
    ;; A grouped candidate starts with the `consult-imenu-prefix' face;
    ;; the face of the name itself comes after that prefix.
    (when (eq face 'consult-imenu-prefix)
      (let ((next (next-single-property-change 0 'face string)))
        (setq face (and next (get-text-property next 'face string)))))
    (or face 'default)))

(defun xl-tagbar--display-name (item group-function)
  "Return ITEM's printable name, without the section prefix when grouped.
The text properties that carry the face are kept."
  (if group-function
      (funcall group-function (car item) t)
    (car item)))

(defun xl-tagbar--group-title (item group-function)
  "Return ITEM's section title, or \"Tags\" when there is none."
  (or (and group-function
           (funcall group-function (car item) nil))
      "Tags"))

(defun xl-tagbar--grouped (items group-function)
  "Group ITEMS by section, preserving the order of first appearance."
  (let (order)
    (dolist (item items)
      (let ((title (xl-tagbar--group-title item group-function)))
        (unless (assoc title order)
          (push (cons title nil) order))
        (setcdr (assoc title order)
                (cons item (cdr (assoc title order))))))
    (mapcar (lambda (entry)
              (cons (car entry) (nreverse (cdr entry))))
            (nreverse order))))

;;; Icons

(defconst xl-tagbar--type-kinds
  '((?f . function)
    (?m . macro)
    (?v . variable)
    (?p . package)
    (?t . type)
    (?c . class)
    (?i . interface)
    (?e . enum)
    (?s . struct)
    (?n . namespace)
    (?k . constant))
  "Map `consult-imenu' narrowing keys to icon kinds.")

(defun xl-tagbar--kind-from-title (title)
  "Return the icon kind for a section TITLE."
  (let ((name (downcase (or title ""))))
    (cond
     ((string-match-p "method\\|procedure\\|routine" name) 'method)
     ((string-match-p "field\\|attribute" name) 'field)
     ((string-match-p "property" name) 'property)
     ((string-match-p "function" name) 'function)
     ((string-match-p "variable" name) 'variable)
     ((string-match-p "macro" name) 'macro)
     ((string-match-p "constant" name) 'constant)
     ((string-match-p "interface" name) 'interface)
     ((string-match-p "enum" name) 'enum)
     ((string-match-p "struct" name) 'struct)
     ((string-match-p "class" name) 'class)
     ((string-match-p "type" name) 'type)
     ((string-match-p "namespace\\|module" name) 'namespace)
     ((string-match-p "package" name) 'package)
     ((string-match-p "heading\\|headline\\|section" name) 'heading)
     (t 'tag))))

(defun xl-tagbar--item-kind (item section-title)
  "Return the icon kind of ITEM in SECTION-TITLE.
The `consult-imenu' type key is precise; modes without a type
configuration fall back to the section name."
  (or (cdr (assq (get-text-property 0 'consult--type (car item))
                 xl-tagbar--type-kinds))
      ;; Org's imenu backend tags its candidates itself.
      (and (get-text-property 0 'org-imenu (car item)) 'heading)
      (xl-tagbar--kind-from-title section-title)))

(defun xl-tagbar--section-kind (section-title items)
  "Return the icon kind for SECTION-TITLE with ITEMS."
  (if items
      (xl-tagbar--item-kind (car items) section-title)
    (xl-tagbar--kind-from-title section-title)))

(defun xl-tagbar--nerd-icons-p ()
  "Return non-nil when the current render should use Nerd Font icons."
  (pcase xl-tagbar-icon-style
    ('nerd t)
    ('unicode nil)
    ('none nil)
    (_ (and (display-graphic-p)
            (find-font (font-spec :family xl-tagbar-nerd-font))))))

(defun xl-tagbar--icon (kind nerd)
  "Return the icon for KIND.
NERD non-nil selects the Nerd Font glyph, otherwise the Unicode
fallback.  An empty string means that icons are turned off."
  (if (eq xl-tagbar-icon-style 'none)
      ""
    (let ((glyphs (cdr (assq kind xl-tagbar-icons))))
      (or (and nerd (nth 0 glyphs))
          (nth 1 glyphs)
          (nth 0 glyphs)
          ""))))

(defun xl-tagbar--kind-face (kind)
  "Return the colour face used by the icon and name of KIND."
  (pcase kind
    ((or 'function 'method) 'font-lock-function-name-face)
    ((or 'variable 'field 'property) 'font-lock-variable-name-face)
    ((or 'type 'class 'struct 'interface 'enum) 'font-lock-type-face)
    ((or 'constant 'package 'namespace 'module) 'font-lock-constant-face)
    ((or 'macro 'heading) 'font-lock-keyword-face)
    (_ 'font-lock-comment-face)))

(defun xl-tagbar--icon-face (kind nerd)
  "Return a face for the icon of KIND, using the Nerd font when NERD."
  (if nerd
      (list (list :family xl-tagbar-nerd-font)
            (xl-tagbar--kind-face kind))
    (xl-tagbar--kind-face kind)))

;;; Rendering

(defun xl-tagbar-refresh ()
  "Rebuild the tagbar from its source buffer."
  (interactive)
  (when (xl-tagbar--live-p)
    (xl-tagbar--render)))

(defun xl-tagbar--render ()
  "Draw the tags of the attached source buffer in the tagbar."
  (let* ((source (window-buffer xl-tagbar--source-window))
         (tagbar (window-buffer xl-tagbar--window))
         (items (with-current-buffer source (consult-imenu--items-safe)))
         (group-function (with-current-buffer source (consult-imenu--group)))
         (groups (xl-tagbar--grouped items group-function)))
    (with-current-buffer tagbar
      (let ((inhibit-read-only t)
            (entries nil)
            (index 0)
            (nerd (xl-tagbar--nerd-icons-p)))
        ;; Delete the old fold overlays before their text disappears.
        (dolist (group xl-tagbar--groups)
          (when-let* ((overlay (plist-get group :overlay)))
            (delete-overlay overlay))
          (when-let* ((overlay (plist-get group :arrow-overlay)))
            (delete-overlay overlay)))
        (erase-buffer)
        (setq xl-tagbar--entries nil
              xl-tagbar--groups nil)
        (setq header-line-format
              (format " %s" (buffer-name source)))
        (if (null groups)
            (insert (propertize "No tags in this buffer\n"
                                'face 'font-lock-comment-face))
          (dolist (section groups)
            (let* ((title (car section))
                   (section-items (cdr section))
                   (heading-start (point))
                   (folded (gethash title xl-tagbar--folded))
                   (kind (xl-tagbar--section-kind title section-items))
                   (icon (xl-tagbar--icon kind nerd))
                   (icon-p (not (string-empty-p icon)))
                   (arrow (if folded "▸ " "▾ "))
                   (button (insert-text-button
                            (concat arrow
                                    (if icon-p (concat icon " ") "")
                                    title)
                            :type 'xl-tagbar-group-button
                            :group index)))
              (when icon-p
                (let ((icon-start (+ heading-start (length arrow))))
                  (put-text-property icon-start (+ icon-start (length icon))
                                     'face (xl-tagbar--icon-face kind nerd))))
              (button-put button 'keymap
                          (make-composed-keymap xl-tagbar-heading-map
                                                button-map))
              (insert "\n")
              (let ((body-start (point)))
                (dolist (item section-items)
                  (let* ((start (point))
                         (display (xl-tagbar--display-name item group-function))
                         (label (substring-no-properties display))
                         (face (xl-tagbar--item-face display))
                         (kind (xl-tagbar--item-kind item title))
                         (icon (xl-tagbar--icon kind nerd))
                         (icon-p (not (string-empty-p icon))))
                    (insert "  ")
                    (let ((button-start (point)))
                      (insert-text-button
                       (concat (if icon-p (concat icon " ") "") label)
                       :type 'xl-tagbar-item-button
                       :item item
                       :name label
                       :face face)
                      (when icon-p
                        (put-text-property button-start
                                           (+ button-start (length icon))
                                           'face
                                           (xl-tagbar--icon-face kind nerd))))
                    (insert "\n")
                    (push (list :item item
                                :name label
                                :start start
                                :end (point))
                          entries)))
                (push (list :title title
                            :start body-start
                            :end (point)
                            :heading heading-start
                            :folded folded
                            :overlay nil
                            :arrow-overlay nil)
                      xl-tagbar--groups))
              (cl-incf index)))
          (setq xl-tagbar--entries (nreverse entries)
                xl-tagbar--groups (nreverse xl-tagbar--groups))
          (dolist (group xl-tagbar--groups)
            (when (plist-get group :folded)
              (xl-tagbar--fold group t))))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq xl-tagbar--rendered-tick (buffer-chars-modified-tick source)
              xl-tagbar--last-point nil)
        (when xl-tagbar-follow-point
          (xl-tagbar--update-current))))))

;;; Folding

(defun xl-tagbar--fold (group fold)
  "Fold GROUP when FOLD is non-nil; otherwise unfold it."
  (let* ((title (plist-get group :title))
         (overlay (plist-get group :overlay))
         (arrow (plist-get group :arrow-overlay)))
    (if fold
        (unless overlay
          (setq overlay (make-overlay (plist-get group :start)
                                      (plist-get group :end)))
          (overlay-put overlay 'invisible t)
          (plist-put group :overlay overlay))
      (when overlay
        (delete-overlay overlay)
        (plist-put group :overlay nil)))
    (unless arrow
      (setq arrow (make-overlay (plist-get group :heading)
                                (1+ (plist-get group :heading))))
      (plist-put group :arrow-overlay arrow))
    (overlay-put arrow 'display (if fold "▸" "▾"))
    (when xl-tagbar--folded
      (puthash title fold xl-tagbar--folded))
    (setf (plist-get group :folded) fold)))

(defun xl-tagbar--button-toggle (button)
  "Fold or unfold the section whose heading is BUTTON."
  (let ((group-index (button-get button 'group)))
    (when-let* ((group (nth group-index xl-tagbar--groups)))
      (xl-tagbar--fold group (not (plist-get group :folded))))))

(defun xl-tagbar--toggle-at-point ()
  "Fold a section at point, or jump to the tag at point."
  (interactive)
  (let ((group (xl-tagbar--group-at-point)))
    (if group
        (xl-tagbar--fold group (not (plist-get group :folded)))
      (xl-tagbar--jump-at-point))))

;;; Jumping and navigation

(defun xl-tagbar--jump-item (item)
  "Jump to ITEM in its source buffer."
  (unless (window-live-p xl-tagbar--source-window)
    (user-error "The tagbar's source window is gone"))
  (select-window xl-tagbar--source-window)
  (consult-imenu--jump item))

(defun xl-tagbar--button-jump (button)
  "Jump to the tag of BUTTON."
  (xl-tagbar--jump-item (button-get button 'item)))

(defun xl-tagbar--jump-at-point ()
  "Jump to the tag at point."
  (interactive)
  (if-let* ((entry (xl-tagbar--entry-at-point)))
      (xl-tagbar--jump-item (plist-get entry :item))
    (user-error "No tag here")))

(defun xl-tagbar--preview-item (item)
  "Show ITEM in the source window, but keep the tagbar selected."
  (let ((tagbar (selected-window)))
    (xl-tagbar--jump-item item)
    (when (window-live-p tagbar)
      (select-window tagbar))))

(defun xl-tagbar-next ()
  "Jump to the next tag."
  (interactive)
  (let ((entry (seq-find (lambda (entry)
                           (> (plist-get entry :start) (point)))
                         xl-tagbar--entries)))
    (if entry
        (progn
          (goto-char (plist-get entry :start))
          (xl-tagbar--preview-item (plist-get entry :item)))
      (user-error "No next tag"))))

(defun xl-tagbar-previous ()
  "Jump to the previous tag."
  (interactive)
  (let ((entry (car (last (seq-filter
                           (lambda (entry)
                             (< (plist-get entry :start) (point)))
                           xl-tagbar--entries)))))
    (if entry
        (progn
          (goto-char (plist-get entry :start))
          (xl-tagbar--preview-item (plist-get entry :item)))
      (user-error "No previous tag"))))

;;; Following point

(defun xl-tagbar--entry-enclosing-point (point)
  "Return the rendered entry that encloses POINT."
  (let (best best-position)
    (dolist (entry xl-tagbar--entries)
      (let ((position (xl-tagbar--item-position (plist-get entry :item))))
        (when (and position
                   (<= position point)
                   (or (null best) (> position best-position)))
          (setq best entry
                best-position position))))
    best))

(defun xl-tagbar--update-current ()
  "Highlight the tag that encloses point in the source buffer."
  (when (and xl-tagbar-follow-point
             (xl-tagbar--live-p)
             (buffer-live-p (window-buffer xl-tagbar--window)))
    (let ((tagbar (window-buffer xl-tagbar--window))
          (point (window-point xl-tagbar--source-window)))
      (with-current-buffer tagbar
        ;; A command that did not move point cannot change the current tag.
        (unless (eql point xl-tagbar--last-point)
          (setq xl-tagbar--last-point point)
          (let ((entry (xl-tagbar--entry-enclosing-point point)))
            (unless (and xl-tagbar--current-overlay
                         (overlayp xl-tagbar--current-overlay))
              (setq xl-tagbar--current-overlay
                    (let ((overlay (make-overlay 1 1)))
                      (overlay-put overlay 'face 'xl-tagbar-current)
                      (overlay-put overlay 'priority 1)
                      overlay)))
            (if entry
                (let ((start (plist-get entry :start)))
                  (move-overlay xl-tagbar--current-overlay
                                start (plist-get entry :end))
                  ;; Keep the current tag visible without stealing the selection.
                  (let ((window xl-tagbar--window))
                    (when (and (window-live-p window)
                               (not (invisible-p start))
                               (not (pos-visible-in-window-p start window)))
                      (with-selected-window window
                        (goto-char start)
                        (recenter)))))
              (move-overlay xl-tagbar--current-overlay 1 1))))))))

;;; Refresh hooks

(defun xl-tagbar--schedule-refresh ()
  "Rebuild the tagbar after a short idle delay."
  (when (timerp xl-tagbar--refresh-timer)
    (cancel-timer xl-tagbar--refresh-timer))
  (setq xl-tagbar--refresh-timer
        (run-with-idle-timer xl-tagbar-refresh-delay nil
                             #'xl-tagbar--refresh-if-dirty)))

(defun xl-tagbar--refresh-if-dirty ()
  "Refresh the tagbar when the source buffer changed since the last render."
  (setq xl-tagbar--refresh-timer nil)
  (when (xl-tagbar--live-p)
    (let ((source (window-buffer xl-tagbar--source-window))
          (tagbar (window-buffer xl-tagbar--window)))
      (unless (and (buffer-live-p tagbar)
                   (with-current-buffer tagbar
                     (eql xl-tagbar--rendered-tick
                          (buffer-chars-modified-tick source))))
        (xl-tagbar-refresh)))))

(defun xl-tagbar--after-change (&rest _)
  "Schedule a refresh after a change in the source buffer."
  (when (xl-tagbar--live-p)
    (xl-tagbar--schedule-refresh)))

(defun xl-tagbar--after-save (&rest _)
  "Refresh the tagbar after the source buffer is saved."
  (when (xl-tagbar--live-p)
    (xl-tagbar-refresh)))

(defun xl-tagbar--source-killed ()
  "Close the tagbar when its source buffer is killed."
  (when (eq (current-buffer) xl-tagbar--source-buffer)
    (xl-tagbar-close)))

(defun xl-tagbar--post-command ()
  "Follow buffer switches and point changes in the source window."
  (if (not (xl-tagbar--live-p))
      (xl-tagbar-close)
    (let ((buffer (window-buffer xl-tagbar--source-window)))
      (unless (eq buffer xl-tagbar--source-buffer)
        (xl-tagbar--set-source buffer)
        (xl-tagbar-refresh))
      (xl-tagbar--update-current))))

(defun xl-tagbar--remove-source-hooks ()
  "Remove the refresh hooks from the buffer the tagbar watched."
  (when (buffer-live-p xl-tagbar--source-buffer)
    (with-current-buffer xl-tagbar--source-buffer
      (remove-hook 'after-change-functions #'xl-tagbar--after-change t)
      (remove-hook 'after-save-hook #'xl-tagbar--after-save t)
      (remove-hook 'after-revert-hook #'xl-tagbar--after-save t)
      (remove-hook 'kill-buffer-hook #'xl-tagbar--source-killed t))))

(defun xl-tagbar--set-source (buffer)
  "Attach the tagbar to BUFFER, moving its refresh hooks."
  (xl-tagbar--remove-source-hooks)
  (setq xl-tagbar--source-buffer buffer)
  (with-current-buffer buffer
    (add-hook 'after-change-functions #'xl-tagbar--after-change nil t)
    (add-hook 'after-save-hook #'xl-tagbar--after-save nil t)
    (add-hook 'after-revert-hook #'xl-tagbar--after-save nil t)
    (add-hook 'kill-buffer-hook #'xl-tagbar--source-killed nil t)))

;;; Opening and closing

(defun xl-tagbar--buffer-killed ()
  "Forget the tagbar window and its source hooks when its buffer is killed."
  (setq xl-tagbar--window nil)
  (remove-hook 'post-command-hook #'xl-tagbar--post-command)
  (xl-tagbar--remove-source-hooks)
  (setq xl-tagbar--source-buffer nil))

;;;###autoload
(defun xl-tagbar-open (&optional buffer)
  "Show the tagbar for BUFFER, or for the selected window's buffer."
  (interactive)
  (let ((buffer (or buffer (window-buffer (selected-window)))))
    (unless (buffer-live-p buffer)
      (user-error "No live buffer to describe"))
    (when (minibufferp buffer)
      (user-error "The tagbar cannot describe the minibuffer"))
    (when (eq buffer (xl-tagbar--buffer))
      (user-error "The tagbar cannot describe itself"))
    ;; A second call, from another window, describes that window's buffer.
    (when (window-live-p xl-tagbar--window)
      (xl-tagbar-close))
    (let ((tagbar (get-buffer-create xl-tagbar--buffer-name)))
      (with-current-buffer tagbar
        (xl-tagbar-mode)
        (setq xl-tagbar--entries nil
              xl-tagbar--groups nil
              xl-tagbar--rendered-tick nil))
      (setq xl-tagbar--source-window (selected-window)
            xl-tagbar--window
            (display-buffer-in-side-window
             tagbar
             `((side . ,xl-tagbar-side)
               (slot . 0)
               (window-width . ,xl-tagbar-width)))
            xl-tagbar--source-buffer nil)
      (when (window-live-p xl-tagbar--window)
        (set-window-dedicated-p xl-tagbar--window t)
        (window-preserve-size xl-tagbar--window t)
        (xl-tagbar--set-source buffer)
        (add-hook 'post-command-hook #'xl-tagbar--post-command)
        (xl-tagbar-refresh)
        (message "Tagbar: %s" (buffer-name buffer)))
      xl-tagbar--window)))

;;;###autoload
(defun xl-tagbar-close ()
  "Close the tagbar."
  (interactive)
  (remove-hook 'post-command-hook #'xl-tagbar--post-command)
  (when (timerp xl-tagbar--refresh-timer)
    (cancel-timer xl-tagbar--refresh-timer))
  (setq xl-tagbar--refresh-timer nil)
  (xl-tagbar--remove-source-hooks)
  (setq xl-tagbar--source-buffer nil)
  (let ((window xl-tagbar--window)
        (buffer (xl-tagbar--buffer)))
    (setq xl-tagbar--window nil
          xl-tagbar--source-window nil)
    (when (window-live-p window)
      (ignore-errors (delete-window window)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

;;;###autoload
(defun xl-tagbar-toggle ()
  "Toggle the Tagbar-like imenu sidebar."
  (interactive)
  (if (and (window-live-p xl-tagbar--window)
           (eq (window-buffer xl-tagbar--window) (xl-tagbar--buffer)))
      (xl-tagbar-close)
    (xl-tagbar-open)))

;;; Embark: act on the tag at point

(defun xl-tagbar--embark-target ()
  "Return the tag at point as an Embark symbol target."
  (when (derived-mode-p 'xl-tagbar-mode)
    (when-let* ((entry (xl-tagbar--entry-at-point))
                (name (plist-get entry :name)))
      (cons 'symbol name))))

(defun xl-tagbar--register-embark ()
  "Make `embark-act' see tagbar entries."
  (when (boundp 'embark-target-finders)
    (add-to-list 'embark-target-finders #'xl-tagbar--embark-target)))

(with-eval-after-load 'embark
  (xl-tagbar--register-embark))

;;; Evil: the normal state would take n, p, g and q first.

(require 'evil nil t)
(with-eval-after-load 'evil
  (evil-set-initial-state 'xl-tagbar-mode 'normal)
  (evil-define-key 'normal xl-tagbar-mode-map
    (kbd "RET") #'xl-tagbar--jump-at-point
    (kbd "<return>") #'xl-tagbar--jump-at-point
    (kbd "TAB") #'xl-tagbar--toggle-at-point
    (kbd "<backtab>") #'xl-tagbar--toggle-at-point
    "n" #'xl-tagbar-next
    "p" #'xl-tagbar-previous
    "g" #'xl-tagbar-refresh
    "q" #'xl-tagbar-close
    (kbd "C-.") #'embark-act))

(provide 'xl-tagbar)
;;; xl-tagbar.el ends here
