;;; xl-perfetto-probes.el --- package probes for xl-perfetto-trace -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-perfetto-trace "0.1.0"))
;; Keywords: tools, profiling

;;; Commentary:
;; Probes for packages outside Emacs core: xl-async, vertico, corfu and pyim.
;; The recorder loads this file when a recording starts, so the core works
;; without it, and a package that loads during a recording installs its probe
;; at that moment.  The ring, JSON output and built-in probes stay in
;; xl-perfetto-trace.el.

;;; Code:

(require 'cl-lib)
(require 'xl-perfetto-trace)

(declare-function xl-async-finally "xl-async" (proc fn))
(declare-function xl-async-status "xl-async" (proc))
(declare-function xl-async-exit-code "xl-async" (proc))
(declare-function xl-async-output "xl-async" (proc))

;;; ============================================================
;;; xl-async jobs
;;; ============================================================

;; Advised on `xl-async-start', the one place every job starts, so jobs made
;; directly (package installs, native compiles) are recorded too.  The end is
;; recorded by a finally handler on the process, which runs however the job ends.

(defun xl-perfetto--async-name (command args)
  "Return a display name for a job started with COMMAND and ARGS."
  (let ((name (plist-get args :name)))
    (or (and name (format "%s" name))
        (and (consp command) (car command)
             (file-name-nondirectory (car command)))
        "xl-async")))

(defun xl-perfetto--async-around (original command &rest args)
  "Record the job that `xl-async-start' starts for COMMAND and ARGS.
ORIGINAL is `xl-async-start'.  Returns the process it returns."
  (if (or (not xl-perfetto--recording) xl-perfetto--inside)
      (apply original command args)
    (let* ((id (format "xl-async-%d" (cl-incf xl-perfetto--async-counter)))
           (name (xl-perfetto--async-name command args))
           (process (apply original command args)))
      (when (processp process)
        (xl-perfetto--record "b" name "xl-async" (xl-perfetto--elapsed)
                             nil 3 nil id)
        (push (cons id (list :name name)) xl-perfetto--async-open)
        (xl-async-finally process
                          (lambda (proc)
                            (xl-perfetto--async-finish id proc))))
      process)))

(defun xl-perfetto--async-finish (id process)
  "Close the asynchronous event ID for PROCESS, which has ended.
Records its status, exit code and output size."
  (let ((entry (assoc id xl-perfetto--async-open)))
    (when entry
      (setq xl-perfetto--async-open
            (assq-delete-all id xl-perfetto--async-open))
      (when xl-perfetto--recording
        (xl-perfetto--record
         "e" (plist-get (cdr entry) :name) "xl-async"
         (xl-perfetto--elapsed) nil 3
         (list :status (format "%s" (xl-async-status process))
               :exit (xl-async-exit-code process)
               :bytes (string-bytes (or (xl-async-output process) "")))
         id)))))

;; Minibuffer completion (vertico, and consult lists shown through it) does not go
;; through `completion-at-point', so it is timed on `vertico--update', the function
;; that recomputes and shows the candidates on each change of input.

(xl-perfetto-define-span-advice xl-perfetto--vertico-around
  vertico--update (&rest args) (apply original args)
  "completion" 9 "vertico--update"
  (list :candidates (if (boundp 'vertico--total) vertico--total 0))
  nil "It includes the candidate count.")

(xl-perfetto-define-span-advice xl-perfetto--corfu-around
  corfu--update (&rest args) (apply original args)
  "completion" 9 "corfu--update"
  (append (list :candidates (if (boundp 'corfu--total) corfu--total 0))
          (when xl-perfetto-trace-record-names
            (list :buffer (buffer-name))))
  nil "The in-buffer popup computes its candidates here on every keystroke.")

(defun xl-perfetto--install-corfu ()
  "Advise `corfu--update' when corfu is loaded."
  (when (fboundp 'corfu--update)
    (advice-add 'corfu--update :around #'xl-perfetto--corfu-around)
    (push (lambda ()
            (advice-remove 'corfu--update #'xl-perfetto--corfu-around))
          xl-perfetto--installed)))

(defun xl-perfetto--install-vertico ()
  "Advise `vertico--update' when vertico is loaded."
  (when (fboundp 'vertico--update)
    (advice-add 'vertico--update :around #'xl-perfetto--vertico-around)
    (push (lambda ()
            (advice-remove 'vertico--update #'xl-perfetto--vertico-around))
          xl-perfetto--installed)))

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

(xl-perfetto-define-span-advice xl-perfetto--pyim-input-around
  pyim-input-method (&rest args) (apply original args)
  "input" 11 "pyim-input-method"
  (append (list :mode (symbol-name major-mode))
          (when xl-perfetto-trace-record-names
            (list :buffer (buffer-name))))
  nil "It covers a whole composition, from the first key to the commit.")

(xl-perfetto-define-span-advice xl-perfetto--pyim-run-around
  pyim-process-run (&rest args) (apply original args)
  "input" 11 "pyim-process-run"
  (list :candidates (if (boundp 'pyim-process--candidates)
                        (length pyim-process--candidates)
                      0))
  nil "The candidate count is the size of the list pyim just built.")

(xl-perfetto-define-span-advice xl-perfetto--pyim-refresh-around
  pyim-page--refresh (&rest args) (apply original args)
  "input" 11 "pyim-page--refresh"
  nil nil "It measures the popup redraw.")

(xl-perfetto-define-span-advice xl-perfetto--pyim-select-around
  pyim-select-word (&rest args) (apply original args)
  "input" 11 "pyim-select-word"
  nil nil "Its duration covers inserting the text and the after-change hooks.")

(defun xl-perfetto--install-input ()
  "Advise pyim's entry points.  Loads pyim when it is installed."
  (when (require 'pyim nil t)
    (dolist (pair '((pyim-input-method . xl-perfetto--pyim-input-around)
                    (pyim-process-run . xl-perfetto--pyim-run-around)
                    (pyim-page--refresh . xl-perfetto--pyim-refresh-around)
                    (pyim-select-word . xl-perfetto--pyim-select-around)))
      (let ((function (car pair))
            (advice (cdr pair)))
        (when (fboundp function)
          (advice-add function :around advice)
          (push (lambda () (advice-remove function advice))
                xl-perfetto--installed))))))

(defun xl-perfetto--install-async ()
  "Advise `xl-async-start' when xl-async is loaded."
  (when (fboundp 'xl-async-start)
    (advice-add 'xl-async-start :around #'xl-perfetto--async-around)
    (push (lambda ()
            (advice-remove 'xl-async-start #'xl-perfetto--async-around))
          xl-perfetto--installed)))


(xl-perfetto-define-probe xl-async 'xl-async #'xl-perfetto--install-async)
(xl-perfetto-define-probe vertico 'completion #'xl-perfetto--install-vertico)
(xl-perfetto-define-probe corfu 'completion #'xl-perfetto--install-corfu)
;; The installer loads pyim itself, as before, so a recording made before pyim
;; was first used still covers typing Chinese.
(xl-perfetto-define-probe emacs 'input #'xl-perfetto--install-input)

(provide 'xl-perfetto-probes)
;;; xl-perfetto-probes.el ends here
