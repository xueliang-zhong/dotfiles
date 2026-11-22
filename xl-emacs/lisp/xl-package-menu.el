;;; xl-package-menu.el --- Animated loader for the Package Menu  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; Emacs 31 already refreshes the archives asynchronously
;; (`package-menu-async', `package--downloads-in-progress'), and
;; `list-packages' shows its cached list while the downloads run.  What it
;; lacks is a moving indicator: the mode line only says ":Loading".
;;
;; This file keeps that asynchronous core untouched and animates the mode
;; line instead.  The frames come from the same idea as `xl-git-status':
;; the loader is visible while the work runs and disappears when the
;; archives are in, so `list-packages' never looks stuck.

;;; Code:

(require 'package)
(require 'xl-spinner)

(defvar-local xl-package-menu--indicator ""
  "Mode line string of this Package Menu's loader.")

(defun xl-package-menu--draw-loader (text)
  "Show TEXT in this Package Menu's mode line; nil removes it.
The string is the first element of `mode-line-process', which
`package-menu-mode' gives to its static \":Loading\"."
  (setq xl-package-menu--indicator (if text (concat " " text " Loading") ""))
  (when (and (consp mode-line-process)
             (stringp (car mode-line-process)))
    (setcar mode-line-process xl-package-menu--indicator))
  (force-mode-line-update))

(defun xl-package-menu--start ()
  "Start the loader in this Package Menu buffer.
A refresh that never reports completion gives up after a minute of
frames, so a broken archive cannot leave the loader spinning forever."
  (xl-spinner-start #'xl-package-menu--draw-loader :limit 1200))

(defun xl-package-menu--stop ()
  "Stop the loader in this Package Menu buffer."
  (xl-spinner-stop))

(defun xl-package-menu--on-refresh (&optional _async)
  "Show the loader in the Package Menu while the archives download.
Runs from `package-refresh-contents-hook', which both the asynchronous
and the synchronous paths go through."
  (when package-archives
    (when-let* ((buffer (get-buffer "*Packages*")))
      (with-current-buffer buffer
        (when (derived-mode-p 'package-menu-mode)
          (xl-package-menu--start))))))

(defun xl-package-menu--on-done ()
  "Stop the loader once the archives have been downloaded.
Runs from `package--post-download-archives-hook', which package.el runs
when the last download finishes."
  (when-let* ((buffer (get-buffer "*Packages*")))
    (with-current-buffer buffer
      (when (derived-mode-p 'package-menu-mode)
        (xl-package-menu--stop)))))

(defun xl-package-menu-setup ()
  "Put the animated loader in this Package Menu's mode line.
`package-menu-mode' already displays `:Loading' while
`package--downloads-in-progress' is non-nil; this replaces that slot with
a plain string, so the loader can be animated, and leaves the transaction
status where it was."
  (setq-local xl-package-menu--indicator "")
  (setq-local mode-line-process
              (list ""
                    '(package-menu--transaction-status
                      package-menu--transaction-status)))
  (add-hook 'kill-buffer-hook #'xl-package-menu--stop nil t))

(add-hook 'package-menu-mode-hook #'xl-package-menu-setup)
(add-hook 'package-refresh-contents-hook #'xl-package-menu--on-refresh)
(add-hook 'package--post-download-archives-hook #'xl-package-menu--on-done)

(provide 'xl-package-menu)
;;; xl-package-menu.el ends here
