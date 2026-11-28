;;; tuned-perf-probes.el --- package probes for tuned-perf-trace -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tuned-perf-trace "0.1.0"))
;; Keywords: tools, profiling

;;; Commentary:
;; Probes for packages outside Emacs core: tuned-async, vertico, corfu and pyim.
;; The recorder loads this file when a recording starts, so the core works
;; without it, and a package that loads during a recording installs its probe
;; at that moment.  The ring, JSON output and built-in probes stay in
;; tuned-perf-trace.el.

;;; Code:

(require 'cl-lib)
(require 'tuned-perf-trace)

(declare-function tuned-async-finally "tuned-async" (proc fn))
(declare-function tuned-async-status "tuned-async" (proc))
(declare-function tuned-async-exit-code "tuned-async" (proc))
(declare-function tuned-async-output "tuned-async" (proc))

;;; ============================================================
;;; tuned-async jobs
;;; ============================================================

;; Advised on `tuned-async-start', the one place every job starts, so jobs made
;; directly (package installs, native compiles) are recorded too.  The end is
;; recorded by a finally handler on the process, which runs however the job ends.

(defun tuned-perf--async-name (command args)
  "Return a display name for a job started with COMMAND and ARGS."
  (let ((name (plist-get args :name)))
    (or (and name (format "%s" name))
        (and (consp command) (car command)
             (file-name-nondirectory (car command)))
        "tuned-async")))

(defun tuned-perf--async-around (original command &rest args)
  "Record the job that `tuned-async-start' starts for COMMAND and ARGS.
ORIGINAL is `tuned-async-start'.  Returns the process it returns."
  (if (or (not tuned-perf--recording) tuned-perf--inside)
      (apply original command args)
    (let* ((id (format "tuned-async-%d" (cl-incf tuned-perf--async-counter)))
           (name (tuned-perf--async-name command args))
           (process (apply original command args)))
      (when (processp process)
        (tuned-perf--record "b" name "tuned-async" (tuned-perf--elapsed)
                             nil 3 nil id)
        (push (cons id (list :name name)) tuned-perf--async-open)
        (tuned-async-finally process
                          (lambda (proc)
                            (tuned-perf--async-finish id proc))))
      process)))

(defun tuned-perf--async-finish (id process)
  "Close the asynchronous event ID for PROCESS, which has ended.
Records its status, exit code and output size."
  (let ((entry (assoc id tuned-perf--async-open)))
    (when entry
      (setq tuned-perf--async-open
            (assq-delete-all id tuned-perf--async-open))
      (when tuned-perf--recording
        (tuned-perf--record
         "e" (plist-get (cdr entry) :name) "tuned-async"
         (tuned-perf--elapsed) nil 3
         (list :status (format "%s" (tuned-async-status process))
               :exit (tuned-async-exit-code process)
               :bytes (string-bytes (or (tuned-async-output process) "")))
         id)))))

;; Minibuffer completion (vertico, and consult lists shown through it) does not go
;; through `completion-at-point', so it is timed on `vertico--update', the function
;; that recomputes and shows the candidates on each change of input.

(tuned-perf-define-span-advice tuned-perf--vertico-around
  vertico--update (&rest args) (apply original args)
  "completion" 9 "vertico--update"
  (list :candidates (if (boundp 'vertico--total) vertico--total 0))
  nil "It includes the candidate count.")

(tuned-perf-define-span-advice tuned-perf--corfu-around
  corfu--update (&rest args) (apply original args)
  "completion" 9 "corfu--update"
  (append (list :candidates (if (boundp 'corfu--total) corfu--total 0))
          (when tuned-perf-trace-record-names
            (list :buffer (buffer-name))))
  nil "The in-buffer popup computes its candidates here on every keystroke.")

(defun tuned-perf--install-corfu ()
  "Advise `corfu--update' when corfu is loaded."
  (when (fboundp 'corfu--update)
    (advice-add 'corfu--update :around #'tuned-perf--corfu-around)
    (push (lambda ()
            (advice-remove 'corfu--update #'tuned-perf--corfu-around))
          tuned-perf--installed)))

(defun tuned-perf--install-vertico ()
  "Advise `vertico--update' when vertico is loaded."
  (when (fboundp 'vertico--update)
    (advice-add 'vertico--update :around #'tuned-perf--vertico-around)
    (push (lambda ()
            (advice-remove 'vertico--update #'tuned-perf--vertico-around))
          tuned-perf--installed)))

;;; ============================================================
;;; Chinese input (pyim)
;;; ============================================================

;; pyim runs in Lisp.  A composition enters `pyim-input-method', which reads
;; further keys until the text is committed; each key rebuilds the candidate
;; list (`pyim-process-run') and redraws the popup (`pyim-page--refresh'), and
;; the commit inserts the text (`pyim-select-word').  These spans fill the gap
;; the command track cannot see: the keys are read inside the input method, so
;; no command runs until the text is inserted.
;; Only durations and candidate counts are recorded, never the pinyin or text.

(tuned-perf-define-span-advice tuned-perf--pyim-input-around
  pyim-input-method (&rest args) (apply original args)
  "input" 11 "pyim-input-method"
  (append (list :mode (symbol-name major-mode))
          (when tuned-perf-trace-record-names
            (list :buffer (buffer-name))))
  nil "It covers a whole composition, from the first key to the commit.")

(tuned-perf-define-span-advice tuned-perf--pyim-run-around
  pyim-process-run (&rest args) (apply original args)
  "input" 11 "pyim-process-run"
  (list :candidates (if (boundp 'pyim-process--candidates)
                        (length pyim-process--candidates)
                      0))
  nil "The candidate count is the size of the list pyim just built.")

(tuned-perf-define-span-advice tuned-perf--pyim-refresh-around
  pyim-page--refresh (&rest args) (apply original args)
  "input" 11 "pyim-page--refresh"
  nil nil "It measures the popup redraw.")

(tuned-perf-define-span-advice tuned-perf--pyim-select-around
  pyim-select-word (&rest args) (apply original args)
  "input" 11 "pyim-select-word"
  nil nil "Its duration covers inserting the text and the after-change hooks.")

(defun tuned-perf--install-input ()
  "Advise pyim's entry points.  Loads pyim when it is installed."
  (when (require 'pyim nil t)
    (dolist (pair '((pyim-input-method . tuned-perf--pyim-input-around)
                    (pyim-process-run . tuned-perf--pyim-run-around)
                    (pyim-page--refresh . tuned-perf--pyim-refresh-around)
                    (pyim-select-word . tuned-perf--pyim-select-around)))
      (let ((function (car pair))
            (advice (cdr pair)))
        (when (fboundp function)
          (advice-add function :around advice)
          (push (lambda () (advice-remove function advice))
                tuned-perf--installed))))))

(defun tuned-perf--install-async ()
  "Advise `tuned-async-start' when tuned-async is loaded."
  (when (fboundp 'tuned-async-start)
    (advice-add 'tuned-async-start :around #'tuned-perf--async-around)
    (push (lambda ()
            (advice-remove 'tuned-async-start #'tuned-perf--async-around))
          tuned-perf--installed)))


(tuned-perf-define-probe tuned-async 'tuned-async #'tuned-perf--install-async)
(tuned-perf-define-probe vertico 'completion #'tuned-perf--install-vertico)
(tuned-perf-define-probe corfu 'completion #'tuned-perf--install-corfu)
;; The installer loads pyim itself, as before, so a recording made before pyim
;; was first used still covers typing Chinese.
(tuned-perf-define-probe emacs 'input #'tuned-perf--install-input)

(provide 'tuned-perf-probes)
;;; tuned-perf-probes.el ends here
