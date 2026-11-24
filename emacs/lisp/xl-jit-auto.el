;;; xl-jit-auto.el --- start xl-jit from the running Emacs once it stays open -*- lexical-binding: t; -*-
;; The only part of xl-jit that lives in the main Emacs (emacs/ideas/xl-jit.md).
;; It uses timers only, no hooks.  After Emacs has been open for
;; `xl-jit-auto-uptime', and the user has then been idle for `xl-jit-auto-idle',
;; it starts one background batch Emacs that runs `xl-jit-run' (lock,
;; fingerprint, audit, compile) at background priority.  Nothing is loaded into
;; this Emacs: new elns take effect on the next start.  Without native
;; compilation (no libgccjit), no timer is ever armed.

(require 'cl-lib)

;; M-x entry points defined in xl-jit.el; autoloaded so startup does not load it.
(autoload 'xl-jit-native-report "xl-jit" "Show the native-compiled functions." t)
(autoload 'xl-jit-report "xl-jit-rank" "Show the xl-jit ranking." t)

(defgroup xl-jit nil
  "Background native compilation of the hot files."
  :group 'comp)

(defcustom xl-jit-auto-enable t
  "Non-nil means the running Emacs starts xl-jit in the background."
  :type 'boolean)

(defcustom xl-jit-auto-uptime (* 10 60)
  "Seconds Emacs must have been open before a background run is considered.
A short-lived Emacs (a quick edit, `emacsclient -a \"\"') never starts one."
  :type 'integer)

(defcustom xl-jit-auto-idle 60
  "Seconds of idleness after which the background run starts."
  :type 'integer)

(defcustom xl-jit-auto-interval (* 24 3600)
  "Least time between two background runs, from the last run's record."
  :type 'integer)

(defcustom xl-jit-auto-retry (* 30 60)
  "When a run is not allowed now (on battery, one still running), check again
after this many seconds."
  :type 'integer)

(defvar xl-jit-auto--timer nil "The pending timer, idle or not.")
(defvar xl-jit-auto--process nil "The background batch Emacs, while it runs.")
(defvar xl-jit-auto-last-result nil
  "What the last background run printed, for example (ran :compiled 1 ...).")

;;; Conditions

(defun xl-jit-auto--last-run-file ()
  "The record the background run writes.  Its mtime says when it last ran."
  (expand-file-name (concat "xl-jit/" comp-native-version-dir "/last-run.eld")
                    user-emacs-directory))

(defun xl-jit-auto--seconds-since-last-run ()
  "Seconds since the last recorded run, or nil if there is none."
  (let ((attrs (and (boundp 'comp-native-version-dir)
                    (file-attributes (xl-jit-auto--last-run-file)))))
    (and attrs (float-time (time-since (file-attribute-modification-time attrs))))))

(defun xl-jit-auto--on-battery-p ()
  "Non-nil when the machine runs on battery.  Unknown counts as not."
  (require 'battery nil t)
  (let* ((fn (bound-and-true-p battery-status-function))
         (line (and fn (ignore-errors (cdr (assq ?L (funcall fn)))))))
    (and (stringp line)
         (string-match-p "\\`\\(bat\\|off-line\\)" (downcase line)))))

(defun xl-jit-auto--uptime ()
  (float-time (time-since before-init-time)))

(defun xl-jit-auto--decision ()
  "Nil when a background run may start now, otherwise a symbol saying why not.
`disabled' and `unavailable' are final for this session; the others are
retried later."
  (let ((since (xl-jit-auto--seconds-since-last-run)))
    (cond ((or (not xl-jit-auto-enable) (equal (getenv "XL_JIT") "0")) 'disabled)
          ((not (and (fboundp 'native-comp-available-p) (native-comp-available-p)))
           'unavailable)
          ((process-live-p xl-jit-auto--process) 'running)
          ((< (xl-jit-auto--uptime) xl-jit-auto-uptime) 'too-early)
          ((and since (< since xl-jit-auto-interval)) 'recent)
          ((xl-jit-auto--on-battery-p) 'battery)
          (t nil))))

(defun xl-jit-auto--delay (why)
  "Seconds to wait before checking again after WHY, or nil to stop."
  (pcase why
    ((or 'disabled 'unavailable) nil)
    ('too-early (max 1 (- xl-jit-auto-uptime (xl-jit-auto--uptime))))
    ('recent (max xl-jit-auto-retry
                  (- xl-jit-auto-interval (or (xl-jit-auto--seconds-since-last-run) 0))))
    (_ xl-jit-auto-retry)))

;;; Timers

(defun xl-jit-auto--cancel ()
  (when (timerp xl-jit-auto--timer) (cancel-timer xl-jit-auto--timer))
  (setq xl-jit-auto--timer nil))

(defun xl-jit-auto--schedule (seconds)
  (xl-jit-auto--cancel)
  (setq xl-jit-auto--timer (run-with-timer seconds nil #'xl-jit-auto--tick)))

(defun xl-jit-auto--tick ()
  "Check the conditions.  If they hold, wait for the user to go idle."
  (xl-jit-auto--cancel)
  (let ((why (xl-jit-auto--decision)))
    (if (null why)
        (setq xl-jit-auto--timer
              (run-with-idle-timer xl-jit-auto-idle nil #'xl-jit-auto--start))
      (let ((delay (xl-jit-auto--delay why)))
        (when delay (xl-jit-auto--schedule delay))))))

(defun xl-jit-auto-arm ()
  "Arm the first check.  Does nothing in batch, so setup, tests, and the
background run itself (which loads this init too) never start another."
  (unless noninteractive
    (xl-jit-auto--schedule (max 1 (- xl-jit-auto-uptime (xl-jit-auto--uptime))))))

;;; The background run

(defun xl-jit-auto--command ()
  "Command line of the background run: the same binary and profile, at
background priority (macOS `taskpolicy -b' puts it on the efficiency cores)."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory))
         (emacs (list exe "--batch" "--init-directory" profile
                      "-l" (expand-file-name "early-init.el" profile)
                      "--eval" "(run-hooks (quote before-init-hook))"
                      "-l" (expand-file-name "init.el" profile)
                      "--eval" "(progn (require 'xl-jit) (prin1 (xl-jit-run)) (terpri))")))
    (cond ((executable-find "taskpolicy") (append '("taskpolicy" "-b") emacs))
          ((executable-find "nice") (append '("nice" "-n" "19") emacs))
          (t emacs))))

(defun xl-jit-auto--sentinel (proc _event)
  (unless (process-live-p proc)
    (setq xl-jit-auto--process nil
          xl-jit-auto-last-result
          (with-current-buffer (process-buffer proc)
            (save-excursion
              (goto-char (point-max))
              ;; The result is the last line that reads as a form.
              (when (re-search-backward "^\\((ran .*)\\|unchanged\\|busy\\|unavailable\\)$" nil t)
                (ignore-errors (let ((read-eval nil)) (read (match-string 0))))))))
    (xl-jit-auto--schedule xl-jit-auto-interval)))

(defun xl-jit-auto--start ()
  "Start the background run, unless the conditions changed while idle."
  (xl-jit-auto--cancel)
  (let ((why (xl-jit-auto--decision)))
    (if why
        (let ((delay (xl-jit-auto--delay why)))
          (when delay (xl-jit-auto--schedule delay)))
      (let ((buf (get-buffer-create " *xl-jit*")))
        (with-current-buffer buf (erase-buffer))
        (setq xl-jit-auto--process
              (make-process :name "xl-jit" :buffer buf :noquery t
                            :connection-type 'pipe
                            :command (xl-jit-auto--command)
                            :sentinel #'xl-jit-auto--sentinel))))))

(provide 'xl-jit-auto)
;;; xl-jit-auto.el ends here
