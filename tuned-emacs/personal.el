;;; personal.el --- your own Emacs settings -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; init.el loads this file last, after tuned-key-bindings, so anything
;; you set here wins over the defaults.  It starts empty on purpose:
;; the defaults come from Crafted and from tuned-emacs/lisp/, and you
;; add your own choices here.
;;
;; Where to start:
;; - example/xueliang.el is the author's full personal opinionated
;;   configuration: theme, fonts, completion, Org, Git and key
;;   bindings.  Copy the parts you want into this file, or copy the
;;   whole file to start from it.
;; - SPC f e d opens this file.
;; - Machine-specific paths (for example a second dotfiles folder)
;;   belong in ~/.emacs-profiles/local.el, which is loaded before the
;;   modules.
;;
;; Rules that keep this file safe to edit:
;; - Use only plain Emacs APIs and the modules in tuned-emacs/lisp/.
;;   Crafted functions and variables exist only when Crafted has
;;   loaded, which is before this file, but they can change between
;;   Crafted releases.
;; - Keep the no-byte-compile cookie on the first line.  This file is
;;   loaded from source, so a stale .elc can never be used.
;; - Keep the file small; move anything that grows into its own file
;;   under tuned-emacs/lisp/ and require it from here.

;;; Code:

;;; personal.el ends here
