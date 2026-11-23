;;; xl-package-upgrades.el --- Count upgradable packages in a child Emacs  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))
;; Keywords: tools, convenience

;;; Commentary:
;; After every refresh of the Package Menu, package.el finds the packages
;; that can be upgraded and says so in the echo area.  That search compares
;; each installed package with the merged archives, and takes about 0.26 s
;; on the main thread for ten thousand packages.
;;
;; This file runs the same search in a child Emacs, through `xl-async-run'.
;; The child merges the archives itself and receives only the installed
;; packages, as plain data.  Its answer is the list of names, which is all
;; the message needs.  The message is the same, and it appears a moment
;; later, when the child answers.
;;
;; Only that message moves.  Marking upgrades (`U') and executing the
;; transaction still use package.el's synchronous search, because they act
;; on the result at once.  If the child fails, the original search runs.

;;; Code:

(require 'package)
(require 'json)
(require 'xl-async)

(defvar xl-package-upgrades--process nil
  "The child Emacs counting upgrades, while one runs.")

(defvar xl-package-upgrades-count nil
  "How many packages can be upgraded, from the last child, or nil if unknown.
A refresh sets it to nil, and the next count fills it in.")

(defvar xl-package-upgrades--source
  (let ((lib (locate-library "xl-package-upgrades")))
    (and lib (concat (file-name-sans-extension lib) ".el")))
  "The source file the child Emacs loads to count the upgrades.")

(defun xl-package-upgrades--set-count (count)
  "Store COUNT and show it in the Package Menu's mode line, if one is open."
  (setq xl-package-upgrades-count count)
  (when-let* ((buffer (get-buffer "*Packages*")))
    (with-current-buffer buffer
      (package-menu--set-mode-line-format))))

(defun xl-package-upgrades-ensure-count ()
  "Count the upgrades in a child Emacs, unless known or on its way.
The Package Menu's mode line calls this, so it never searches on the main
thread."
  (when (and (null xl-package-upgrades-count)
             xl-package-upgrades--source
             (not (process-live-p xl-package-upgrades--process)))
    (xl-package-upgrades--spawn
     (lambda (names)
       (xl-package-upgrades--set-count
        (if (eq names 'failed)
            (length (package-menu--find-upgrades))
          (length names)))))))


;; ============================================================
;; Child side
;; ============================================================

(defun xl-package-upgrades-names (config)
  "Return the names of the packages that can be upgraded, as strings.
This runs in the child Emacs.  CONFIG is a plist with :user-dir,
:archives, :priorities and :pinned (the variables the merge reads), and
:installed, a list of (NAME VERSION-LIST KIND-STRING-OR-NIL) for each
installed package.  The search itself is `package--upgradeable-packages',
the one package.el runs."
  (let ((package-user-dir (plist-get config :user-dir))
        (package-archives (plist-get config :archives))
        (package-archive-priorities (plist-get config :priorities))
        (package-pinned-packages (plist-get config :pinned))
        ;; Stop package.el from initializing itself, which would read the
        ;; installed packages from disk and refresh the archives over the
        ;; network.  Both are already covered by the data below.
        (package--initialized t)
        (package-alist
         (mapcar (lambda (entry)
                   (let ((name (nth 0 entry)))
                     (list name
                           (package-desc-create
                            :name name
                            :version (nth 1 entry)
                            :kind (and (nth 2 entry) (intern (nth 2 entry)))))))
                 (plist-get config :installed)))
        package-archive-contents)
    (package-read-all-archive-contents)
    ;; An empty archive list would make package.el refresh over the network.
    (when package-archive-contents
      (mapcar #'symbol-name (package--upgradeable-packages)))))

;; ============================================================
;; Parent side
;; ============================================================

(defun xl-package-upgrades--installed ()
  "Return the installed packages as plain data for the child Emacs."
  (mapcar (lambda (elt)
            (let ((desc (cadr elt)))
              (list (car elt)
                    (package-desc-version desc)
                    (and (package-desc-kind desc)
                         (symbol-name (package-desc-kind desc))))))
          (package--alist)))

(defun xl-package-upgrades--parse (output)
  "Return the list of names in the child's OUTPUT, or `failed'.
The child prints its answer as JSON on the last line; anything before it
is messages from Emacs."
  (let ((line (car (last (split-string (string-trim output) "\n" t)))))
    (condition-case nil
        (let ((names (json-parse-string line :array-type 'list)))
          (if (and (listp names) (seq-every-p #'stringp names))
              names
            'failed))
      (error 'failed))))

(defun xl-package-upgrades--spawn (on-done)
  "Count the upgrades in a child Emacs and call ON-DONE with the result.
ON-DONE gets the list of names, or `failed' when the child did not answer."
  (let* ((config (list :user-dir package-user-dir
                       :archives package-archives
                       :priorities package-archive-priorities
                       :pinned package-pinned-packages
                       :installed (xl-package-upgrades--installed)))
         (form (format "(progn (terpri) (princ (json-serialize (vconcat (xl-package-upgrades-names '%s)))))"
                       (prin1-to-string config))))
    (setq xl-package-upgrades--process
          (xl-async-run
           :name "xl-package-upgrades"
           :owner nil
           :command (list (expand-file-name invocation-name invocation-directory)
                          "--batch" "-Q"
                          "-L" (file-name-directory xl-package-upgrades--source)
                          "-l" xl-package-upgrades--source
                          "--eval" form)
           :on-done (lambda (output code)
                      (funcall on-done
                               (if (zerop code)
                                   (xl-package-upgrades--parse output)
                                 'failed)))))))

(defun xl-package-upgrades--notify (orig)
  "Tell the user how many packages can be upgraded, counted in a child.
ORIG is `package-menu--find-and-notify-upgrades', which runs the same
search on the main thread; it is the fallback when the child fails."
  (if (not xl-package-upgrades--source)
      (funcall orig)
    (xl-package-upgrades--spawn
     (lambda (names)
       (cond
        ((eq names 'failed) (funcall orig))
        (names
         (xl-package-upgrades--set-count (length names))
         (message "Packages that can be upgraded: %d; type `%s' to mark for upgrading."
                  (length names)
                  (substitute-command-keys "\\[package-menu-mark-upgrades]"))))))))

(advice-add 'package-menu--find-and-notify-upgrades :around
            #'xl-package-upgrades--notify)

(provide 'xl-package-upgrades)
;;; xl-package-upgrades.el ends here
