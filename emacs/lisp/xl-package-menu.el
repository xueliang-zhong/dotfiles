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
;; This file keeps that asynchronous core untouched and shows the same
;; mode-line loader as the Git buffers instead: the `xl-spinner' slot at the
;; left edge of the mode line, visible while the archives download, so
;; `list-packages' never looks stuck and the list itself never moves.

;;; Code:

(require 'package)
(require 'xl-spinner)

(defun xl-package-menu--start ()
  "Start the loader in this Package Menu buffer.
A refresh that never reports completion gives up after a minute of
frames, so a broken archive cannot leave the loader spinning forever."
  (xl-spinner-mode-line-start "Loading archives" :limit 600))

(defun xl-package-menu--stop ()
  "Stop the loader in this Package Menu buffer."
  (xl-spinner-mode-line-stop))

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
`package-menu-mode' displays a static `:Loading' in `mode-line-process'
while `package--downloads-in-progress' is non-nil; the loader replaces
it, so that slot keeps only the transaction status."
  (xl-spinner-mode-line-install)
  (setq-local mode-line-process
              '(package-menu--transaction-status
                package-menu--transaction-status)))

(add-hook 'package-menu-mode-hook #'xl-package-menu-setup)
(add-hook 'package-refresh-contents-hook #'xl-package-menu--on-refresh)
(add-hook 'package--post-download-archives-hook #'xl-package-menu--on-done)

(provide 'xl-package-menu)
;;; xl-package-menu.el ends here
