;;; early-init.el --- Crafted Emacs profile -*- lexical-binding: t; -*-
;; Loaded by the stub in ~/.emacs-profiles/crafted/early-init.el, which
;; defines `xl-dotfiles' and `xl-crafted-home'.

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

;; No JIT native compilation: it compiles packages in the background on
;; first use and slows the whole machine. Packages run as byte code.
;; Emacs 31 has no job-count or threshold setting to limit it, so it stays off.
(setq native-comp-jit-compilation t
      inhibit-automatic-native-compilation t
      package-native-compile t
      native-comp-async-report-warnings-errors 'silent)

;; Compiled packages are not portable across Emacs versions.
(setq package-user-dir
      (expand-file-name (format "elpa-%d.%d" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Crafted refreshes package archives at startup when older than a day;
;; refresh by hand instead (M-x package-refresh-contents).
(setq crafted-package-perform-stale-archive-check nil)

;; Emacs 31 warns about every third-party file without a lexical-binding
;; cookie (e.g. spacemacs-theme); not actionable here.
(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

;; The tagged evil release (1.15.0 on NonGNU ELPA) breaks on Emacs 30+
;; (void-variable evil-mode-buffers); take the evil family from MELPA.
(setq package-pinned-packages '((evil . "melpa")
                                (evil-collection . "melpa")
                                (evil-org . "melpa")))

(load (expand-file-name "modules/crafted-early-init-config" xl-crafted-home))
