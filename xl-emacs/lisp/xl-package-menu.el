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

(defvar xl-package-menu-spinner-frames
  '("[|]" "[/]" "[-]" "[\\]")
  "ASCII frames of the loader in the Package Menu mode line.")

(defvar xl-package-menu-spinner-interval 0.05
  "Seconds between frames of the Package Menu loader.
The same 20 frames per second as the Git status and GBlame loaders.")

(defvar-local xl-package-menu--refreshing nil
  "Non-nil while this Package Menu waits for the archives.")

(defvar-local xl-package-menu--indicator ""
  "Mode line string of this Package Menu's loader.")

(defvar-local xl-package-menu--timer nil
  "Timer advancing this Package Menu's loader.")

(defvar-local xl-package-menu--frame 0
  "Index of the current loader frame in this Package Menu.")

(defvar-local xl-package-menu--ticks 0
  "Ticks since this Package Menu's loader started.")

(defun xl-package-menu--set-indicator (text)
  "Show TEXT in this Package Menu's mode line.
The string is the first element of `mode-line-process', which
`package-menu-mode' does not otherwise use."
  (setq xl-package-menu--indicator text)
  (when (and (consp mode-line-process)
             (stringp (car mode-line-process)))
    (setcar mode-line-process text))
  (force-mode-line-update))

(defun xl-package-menu--start ()
  "Start the loader in this Package Menu buffer."
  (setq xl-package-menu--refreshing t
        xl-package-menu--ticks 0
        xl-package-menu--frame 0)
  (xl-package-menu--set-indicator
   (concat " " (nth 0 xl-package-menu-spinner-frames) " Loading"))
  (unless (timerp xl-package-menu--timer)
    (let ((buffer (current-buffer)))
      (setq xl-package-menu--timer
            (run-with-timer xl-package-menu-spinner-interval
                            xl-package-menu-spinner-interval
                            #'xl-package-menu--tick buffer)))))

(defun xl-package-menu--stop ()
  "Stop the loader in this Package Menu buffer."
  (when (timerp xl-package-menu--timer)
    (cancel-timer xl-package-menu--timer))
  (setq xl-package-menu--refreshing nil
        xl-package-menu--timer nil
        xl-package-menu--ticks 0
        xl-package-menu--frame 0)
  (xl-package-menu--set-indicator ""))

(defun xl-package-menu--tick (buffer)
  "Advance the loader of BUFFER, stopping it when the refresh is over.
A refresh that never reports completion gives up after a minute, so a
broken archive cannot leave the loader spinning forever."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq xl-package-menu--ticks (1+ xl-package-menu--ticks))
      (cond
       ((not xl-package-menu--refreshing)
        (xl-package-menu--stop))
       ((> xl-package-menu--ticks 300)
        (xl-package-menu--stop))
       (t
        (setq xl-package-menu--frame
              (mod (1+ xl-package-menu--frame)
                   (length xl-package-menu-spinner-frames)))
        (xl-package-menu--set-indicator
         (concat " " (nth xl-package-menu--frame
                          xl-package-menu-spinner-frames)
                 " Loading")))))))

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
