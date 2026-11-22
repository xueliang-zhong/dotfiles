;;; early-init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-
;; Loaded by the stub in ~/.emacs-profiles/crafted/early-init.el, which
;; defines `xl-dotfiles' and `xl-crafted-home'.

;; Crafted writes custom.el and the selected-package list back on every start.
;; I set my options explicitly, so nothing needs saving at startup.
(setq crafted-init-auto-save-customized nil
      crafted-init-auto-save-selected-packages nil)

;; Startup trace.  A white frame at startup means init is stuck before the
;; theme loads, and the hang is rare and hard to reproduce.  Each phase of
;; init appends one line to startup.log; after a white screen, the last line
;; shows where init stopped (startup.log.1 keeps the start before).  GUI and
;; terminal starts only: batch runs (tests, just recipes) leave it alone.
(defvar xl-startup-time (current-time) "When this Emacs began early-init.")
(defvar xl-startup-log (expand-file-name "startup.log" user-emacs-directory)
  "File that records how far each phase of startup got.")

(defun xl-startup-mark (phase)
  "Append PHASE, with seconds since early-init, to `xl-startup-log'."
  (unless noninteractive
    (let ((inhibit-message t) (message-log-max nil))
      (write-region (format "%6.2fs  %s\n"
                            (float-time (time-subtract nil xl-startup-time)) phase)
                    nil xl-startup-log t 'silent))))

(unless noninteractive
  (when (file-exists-p xl-startup-log)
    (rename-file xl-startup-log (concat xl-startup-log ".1") t))
  (xl-startup-mark (format "early-init (Emacs %s, pid %d)" emacs-version (emacs-pid)))
  ;; C-g while the frame is still white opens a backtrace of where init is
  ;; stuck; turned off again once startup is done.
  (setq debug-on-quit t)
  (add-hook 'emacs-startup-hook
            (lambda () (setq debug-on-quit nil) (xl-startup-mark "startup done"))
            90))

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
