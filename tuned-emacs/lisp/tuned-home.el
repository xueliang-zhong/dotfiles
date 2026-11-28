;;; tuned-home.el --- The tuned-emacs home screen  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience

;;; Commentary:
;; The buffer Emacs opens on: the logo, a greeting, the recent files (keys 1
;; to 9 open them), a few links, and how long startup took.  It replaces
;; Crafted's splash screen, keeping its look, so the distribution shows its
;; own name.
;;
;; The logo is hexagram 35, Jin (晋): Li, fire and the sun, over Kun, the
;; earth.  The sun stands for Li; the broken, narrowing lines below it, drawn
;; like a reflection on water, stand for Kun.  The image says "the light rises over the earth", which is the
;; distribution's aim: steady, visible progress.  It is drawn from
;; assets/tuned-emacs-logo.svg in a graphical frame, and as text in a
;; terminal.
;;
;; `tuned-home' draws and shows it; `SPC b h' and the start of Emacs call it.

;;; Code:

(require 'seq)
(require 'recentf)
(require 'evil nil t) ; optional: digit keys in normal and motion state

;; ============================================================
;; Options
;; ============================================================

(defgroup tuned-home nil
  "The tuned-emacs home screen."
  :group 'convenience)

(defcustom tuned-home-logo-file
  (expand-file-name "../assets/tuned-emacs-logo.svg"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The image shown at the top of the home screen in a graphical frame."
  :type 'file)

(defcustom tuned-home-recent-count 9
  "How many recent files the home screen lists.  At most 9 have a key."
  :type 'natnum)

(defface tuned-home-greeting
  '((t (:inherit font-lock-comment-face :weight bold :height 1.5)))
  "The greeting under the logo.")

(defconst tuned-home-buffer-name "*Tuned Emacs*")

;; The logo in text, for terminals: the sun over Kun's broken lines.
(defconst tuned-home-ascii-logo
  '("  \\    |    /  "
    "──   ╭───╮   ──"
    "━━━━ ━━━━━━ ━━━"
    "━━ ━━ ━━━━━━ ━━"
    " ━━━━ ━━━ ━━━━ "
    "     ━ ━━━     "
    "      ━━ ━     "))

;; ============================================================
;; Drawing
;; ============================================================

(defvar-local tuned-home--files nil "The recent files listed, in key order.")

(defun tuned-home--insert-centered (text face)
  "Insert TEXT in FACE, centred in the window, then a newline.
Centred by pixel width, so a variable-pitch or larger face is centred too."
  (let ((width (string-pixel-width (propertize text 'face face))))
    (insert (propertize " " 'display `(space :align-to (- center (,(/ width 2))))))
    (insert (propertize text 'face face) "\n")))

(defun tuned-home--insert-logo ()
  "Insert the logo: the image in a graphical frame, text otherwise."
  (let ((image (and (display-graphic-p)
                    (image-type-available-p 'svg)
                    (file-readable-p tuned-home-logo-file)
                    (create-image tuned-home-logo-file 'svg nil :ascent 'center))))
    (if image
        (progn
          (insert (propertize " " 'display
                              `(space :align-to (+ center (-0.5 . ,image)))))
          (insert-image image "Tuned Emacs")
          (insert "\n"))
      (dolist (line tuned-home-ascii-logo)
        (tuned-home--insert-centered line 'fixed-pitch)))
    (insert "\n")))

(defun tuned-home--insert-head ()
  "Insert the logo, the greeting and the guide link."
  (tuned-home--insert-logo)
  (tuned-home--insert-centered "Welcome to Tuned Emacs!" 'tuned-home-greeting)
  (insert "\n\n")
  (fancy-splash-insert
   :face 'variable-pitch
   :link `("View the Tuned Emacs guide"
           ,(lambda (_button) (tuned-home--open-readme))
           "Open README.md, the entry point to the documentation")
   "\n"))

(defun tuned-home--open-readme ()
  "Open the distribution's README.md, read-only."
  (let ((readme (expand-file-name "../README.md"
                                  (file-name-directory tuned-home-logo-file))))
    (find-file-read-only readme)))

(defun tuned-home--insert-recent ()
  "Insert the recent files, each with the digit that opens it."
  ;; One spelling per file: /Users/me/x and ~/x are the same entry.
  (setq recentf-list (seq-uniq (mapcar #'abbreviate-file-name recentf-list)))
  (setq tuned-home--files (seq-take recentf-list tuned-home-recent-count))
  (when tuned-home--files
    (fancy-splash-insert :face '(variable-pitch font-lock-string-face italic)
                         "\n\n\nRecent Files:\n")
    (seq-do-indexed
     (lambda (file i)
       (fancy-splash-insert
        :face 'default (if (< i 9) (format "[%d]  " (1+ i)) "     ")
        :link (list file (lambda (_button) (find-file file)))
        "\n"))
     tuned-home--files)))

(defvar tuned-dotfiles)

(defun tuned-home--insert-tail ()
  "Insert the quick links, then the startup time."
  (fancy-splash-insert
   :face 'variable-pitch
   "\nTo start...     "
   :link `("Open a File" ,(lambda (_button) (call-interactively #'find-file)))
   "     "
   :link `("Open Home Directory" ,(lambda (_button) (dired "~")))
   "     "
   :link `("Open Dotfiles" ,(lambda (_button) (dired tuned-dotfiles)))
   "\n")
  (fancy-splash-insert
   :face '(variable-pitch font-lock-comment-face italic)
   (format "\n\n\nEmacs loaded in %.2f seconds\n"
           (float-time (time-subtract after-init-time before-init-time)))))

;; ============================================================
;; The buffer
;; ============================================================

(defvar-keymap tuned-home-mode-map
  :parent special-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode tuned-home-mode special-mode "Home"
  "The tuned-emacs home screen.
\\<tuned-home-mode-map>Keys 1 to 9 open the listed recent files.
\\[forward-button] moves to the next link and \\[revert-buffer] redraws."
  (setq-local revert-buffer-function (lambda (&rest _) (tuned-home--draw)))
  (setq tab-width 22))

(defun tuned-home--open-nth (n)
  "Return a command that opens the Nth listed recent file."
  (lambda () (interactive)
    (when-let* ((file (nth n tuned-home--files)))
      (find-file file))))

(defun tuned-home--bind-digits ()
  "Bind 1 to 9 in this buffer, in Emacs and in Evil's normal and motion states."
  (dotimes (n 9)
    (let ((key (number-to-string (1+ n)))
          (command (tuned-home--open-nth n)))
      (local-set-key (kbd key) command)
      (when (fboundp 'evil-local-set-key)
        (dolist (state '(normal motion))
          (evil-local-set-key state (kbd key) command)))))
  (when (fboundp 'evil-local-set-key)
    (dolist (state '(normal motion))
      (evil-local-set-key state (kbd "TAB") #'forward-button)
      (evil-local-set-key state (kbd "<backtab>") #'backward-button))))

(defun tuned-home--draw ()
  "Draw the home screen into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (tuned-home--insert-head)
    (tuned-home--insert-recent)
    (insert "\n")
    (tuned-home--insert-tail)
    (set-buffer-modified-p nil)
    ;; Start on the first link, the guide, as Crafted's screen did.
    (goto-char (point-min))
    (ignore-errors (forward-button 1))))

;;;###autoload
(defun tuned-home ()
  "Draw the home screen, show it and return its buffer."
  (interactive)
  (let ((buffer (get-buffer-create tuned-home-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tuned-home-mode) (tuned-home-mode))
      (setq default-directory command-line-default-directory)
      ;; Draw once the buffer is in a window: centring needs the window width.
      (switch-to-buffer buffer)
      (tuned-home--draw)
      (tuned-home--bind-digits))
    buffer))

;; The home screen lists recent files, so recentf must be on before it draws.
;; Its size and cleanup settings are personal, in example/xueliang.el.
(recentf-mode 1)

(provide 'tuned-home)
;;; tuned-home.el ends here
