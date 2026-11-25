;;; early-init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-
;; Loaded by the stub in ~/.emacs-profiles/crafted/early-init.el, which
;; defines `xl-dotfiles' and `xl-crafted-home'.

;; Slow-start recorder: the time each file finishes loading.  When init takes
;; more than `xl-slow-start-seconds', the longest stretches, the fresh .eln
;; files and our files loaded from source go to startup-slow.log, so a slow
;; start that cannot be reproduced on demand still leaves evidence.  Otherwise
;; the list is dropped.  Cost: one cons per loaded file.
(defvar xl-slow-start-seconds 3)
(defvar xl--start-loads nil "(TIME . FILE) for each finished load, newest first.")
(defun xl--note-load (file) (push (cons (float-time) file) xl--start-loads))
(add-hook 'after-load-functions #'xl--note-load)

(defun xl--slow-start-report (&optional log)
  "Write the startup record to LOG (startup-slow.log) if init was slow.
Returns the file written, or nil."
  (remove-hook 'after-load-functions #'xl--note-load)
  (let* ((init (float-time (time-subtract after-init-time before-init-time)))
         (log (or log (expand-file-name "startup-slow.log" user-emacs-directory)))
         (events (reverse xl--start-loads))
         (prev (float-time before-init-time))
         gaps)
    (setq xl--start-loads nil)
    (when (> init xl-slow-start-seconds)
      (ignore-errors
        (dolist (e events)
          (push (list (- (car e) prev) (- (car e) (float-time before-init-time)) (cdr e)) gaps)
          (setq prev (car e)))
        (with-temp-file log
          (insert (format "%s  init %.2f s, %d files loaded\n\n"
                          (format-time-string "%F %T") init (length events)))
          (insert "Longest stretches before a load finished (stretch, at, file):\n")
          (dolist (g (seq-take (sort gaps (lambda (a b) (> (car a) (car b)))) 15))
            (insert (format "  %6.3f s  %6.3f s  %s\n" (nth 0 g) (nth 1 g) (nth 2 g))))
          (when (boundp 'comp-loaded-comp-units-h)
            (insert "\nUser .eln files loaded, built in the last day (age):\n")
            (maphash (lambda (f _)
                       (when (string-match-p "eln-cache" f)
                         (let ((age (float-time (time-subtract
                                                 nil (file-attribute-modification-time
                                                      (file-attributes f))))))
                           (when (< age 86400)
                             (insert (format "  %7.0f s  %s\n" age (file-name-nondirectory f)))))))
                     comp-loaded-comp-units-h))
          (insert "\nOur files loaded from source:\n")
          (dolist (e events)
            (when (and (string-prefix-p (expand-file-name "emacs/" xl-dotfiles) (cdr e))
                       (string-suffix-p ".el" (cdr e)))
              (insert "  " (cdr e) "\n")))
          (insert "\nEvery load (at, file):\n")
          (dolist (e events)
            (insert (format "  %7.3f  %s\n" (- (car e) (float-time before-init-time)) (cdr e)))))
        (message "Slow start: %.1f s; details in %s" init log)
        log))))
(add-hook 'emacs-startup-hook #'xl--slow-start-report)

;; ffap works out its C and C++ include folders when it loads, by running
;; gcc, clang and g++ (`ffap--c-path').  Embark loads ffap during init, and on
;; macOS those commands are xcrun shims that sometimes hang: the slow-start
;; log showed 12.1 of 13.3 s spent there.  With the variables already set,
;; ffap's `defvar' skips that work; the folders matter only for following an
;; #include with ffap.
(defvar ffap-c-path '("/usr/include" "/usr/local/include"))
(defvar ffap-c++-path ffap-c-path)

;; Crafted writes custom.el and the selected-package list back on every start.
;; I set my options explicitly, so nothing needs saving at startup.
(setq crafted-init-auto-save-customized nil
      crafted-init-auto-save-selected-packages nil)

;; Startup speed: no GC during init, back to a moderate threshold afterwards.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 128 1024 1024))))

;; Disable window chrome before the first frame is drawn.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Start maximized: the first frame is created with this size, so it never
;; shows as a normal window first.  (Setting it in init.el came too late.)
(push '(fullscreen . maximized) default-frame-alist)

;; Packages run as byte code: no JIT (it compiles in the background on first
;; use and slows the whole machine) and no native compile on install.  Emacs's
;; own Lisp stays native (it ships compiled).  Advising a C primitive still
;; needs a "trampoline", compiled once on the main thread and cached in
;; eln-cache; `just emacs-check' builds them in batch, before a GUI start.
(setq load-prefer-newer t)
;; Kill switch for eln-opt (eln-opt.md, "应急开关"): with ELN_OPT=0, every .eln is
;; skipped and the .elc or .el files load instead.  Core Emacs stays native.
(when (equal (getenv "ELN_OPT") "0")
  (setq load-no-native t))   ; a stale .elc never shadows a newer .el (eln-opt tier 1)
(setq native-comp-jit-compilation nil
      package-native-compile nil
      native-comp-async-report-warnings-errors 'silent)

;; Compiled packages are not portable across Emacs versions.
(setq package-user-dir
      (expand-file-name (format "elpa-%d.%d" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Crafted would refresh package archives at startup when older than a day.
;; This config skips that: init.el refreshes only on the path that installs a
;; missing package, where a stale cache makes a dependency look unavailable;
;; otherwise refresh by hand with M-x package-refresh-contents.
(setq crafted-package-perform-stale-archive-check nil)

;; Defer package archive reads: `package-initialize' reads every archive
;; cache from disk on each start.  With this nil, Crafted skips it, and
;; init.el activates installed packages with `package-activate-all' and
;; reads the archives only when a selected package is missing.
(setq package-enable-at-startup nil)

;; Emacs 31 warns about every third-party file without a lexical-binding
;; cookie (e.g. spacemacs-theme); not actionable here.
(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

;; The tagged evil release (1.15.0 on NonGNU ELPA) breaks on Emacs 30+
;; (void-variable evil-mode-buffers); take the evil family from MELPA.
(setq package-pinned-packages '((evil . "melpa")
                                (evil-collection . "melpa")
                                (evil-org . "melpa")))

(load (expand-file-name "modules/crafted-early-init-config" xl-crafted-home))
