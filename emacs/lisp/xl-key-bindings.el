;;; xl-key-bindings.el --- Spacemacs key habits on top of Crafted  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience

;;; Commentary:
;; Crafted has no leader-key system; this file recreates the Spacemacs
;; defaults my fingers use: the SPC tree, `M-m' in insert/emacs state, the
;; `,' / `SPC m' major-mode leader, `fd' escape, surround and window numbers.
;; Loaded by emacs/init.el before xueliang.el, so personal bindings
;; there win on conflicts (e.g. SPC SPC, SPC x, SPC g f).
;;
;; This is an adapter rather than a standalone package: it reads
;; `xl-dotfiles' and calls commands defined in xueliang.el.

;;; Code:

(require 'seq)
(autoload 'flymake-show-buffer-diagnostics "flymake" nil t) ; SPC e l before flymake loads

(defun xl-sk-bind (map &rest specs)
  "In MAP bind KEY LABEL DEF triples; a nil DEF only names a prefix."
  (while specs
    (let ((key (pop specs)) (label (pop specs)) (def (pop specs)))
      (if def
          (keymap-set map key (cons label def))
        (with-eval-after-load 'which-key
          (which-key-add-keymap-based-replacements map key label))))))

;;; Small commands Spacemacs provides and Emacs lacks
(defun xl-sk-copy-file-path () "Copy this buffer's file path." (interactive)
  (let ((path (or buffer-file-name default-directory)))
    (kill-new path) (message "Copied %s" path)))
(defun xl-sk-delete-this-file () "Delete this buffer's file and kill the buffer." (interactive)
  (when (and buffer-file-name (yes-or-no-p (format "Delete %s? " buffer-file-name)))
    (delete-file buffer-file-name t) (kill-buffer)))
(defun xl-sk-copy-whole-buffer () "Copy the whole buffer." (interactive)
  (kill-new (buffer-string)) (message "Buffer copied"))
(defun xl-sk-open-dotfile () "Open my personal Emacs config." (interactive)
  (find-file (expand-file-name "emacs/xueliang.el" xl-dotfiles)))
(defun xl-sk-relative-line-numbers () "Toggle relative line numbers." (interactive)
  (setq-local display-line-numbers
              (if (eq display-line-numbers 'relative) nil 'relative)))
(defvar xl-sk-themes '(spacemacs-dark doom-tokyo-night doom-spacegrey)
  "Themes cycled by SPC T n, like `dotspacemacs-themes'.")
(defun xl-sk-cycle-theme () "Switch to the next theme in `xl-sk-themes'." (interactive)
  (let* ((now (car custom-enabled-themes))
         (next (or (cadr (memq now xl-sk-themes)) (car xl-sk-themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next t) (message "Theme: %s" next)))

;;; SPC tree
(defvar-keymap xl-leader-map :doc "Keys under SPC (and M-m outside normal state).")
(evil-define-key '(normal visual motion) 'global (kbd "SPC") xl-leader-map)
(evil-define-key '(insert emacs) 'global (kbd "M-m") xl-leader-map)

(xl-sk-bind xl-leader-map
  "SPC" "switch buffer"  #'consult-buffer ; Spacemacs default is M-x
  "TAB" "last buffer"    #'mode-line-other-buffer
  "!"   "shell command"  #'shell-command
  "'"   "eshell"         #'eshell
  ";"   "comment"        #'evilnc-comment-operator
  "u"   "universal arg"  #'universal-argument
  "1" "window 1" #'winum-select-window-1   "2" "window 2" #'winum-select-window-2
  "3" "window 3" #'winum-select-window-3   "4" "window 4" #'winum-select-window-4
  "5" "window 5" #'winum-select-window-5   "6" "window 6" #'winum-select-window-6
  "7" "window 7" #'winum-select-window-7   "8" "window 8" #'winum-select-window-8
  "9" "window 9" #'winum-select-window-9

  "a" "apps" nil
  "a c" "calc" #'calc  "a d" "dired" #'dired  "a s e" "eshell" #'eshell
  "a o a" "org agenda" #'org-agenda  "a o c" "org capture" #'org-capture

  "b" "buffers" nil
  "b b" "switch buffer" #'consult-buffer
  "b h" "home"          #'xl-sk-home-buffer
  "b d" "kill buffer"   #'kill-current-buffer
  "b x" "kill buffer & window" #'kill-buffer-and-window
  "b n" "next buffer"   #'next-buffer
  "b p" "previous buffer" #'previous-buffer
  "b R" "revert"        #'revert-buffer-quick
  "b m" "messages"      #'view-echo-area-messages
  "b I" "ibuffer"       #'ibuffer
  "b Y" "copy buffer"   #'xl-sk-copy-whole-buffer

  "c" "comments/compile" nil
  "c l" "comment lines"     #'evilnc-comment-or-uncomment-lines
  "c p" "comment paragraph" #'evilnc-comment-or-uncomment-paragraphs
  "c y" "copy & comment"    #'evilnc-copy-and-comment-lines
  "c C" "compile"           #'compile

  "e" "errors" nil
  "e n" "next error" #'next-error  "e p" "previous error" #'previous-error
  "e l" "list errors" #'flymake-show-buffer-diagnostics

  "f" "files" nil
  "f f" "find file"     #'find-file
  "f r" "recent files"  #'consult-recent-file
  "f s" "save"          #'save-buffer
  "f S" "save all"      #'save-some-buffers
  "f R" "rename file"   #'rename-visited-file
  "f D" "delete file"   #'xl-sk-delete-this-file
  "f c" "copy file"     #'copy-file
  "f j" "dired jump"    #'dired-jump
  "f y" "yank" nil
  "f y y" "copy path"   #'xl-sk-copy-file-path
  "f e" "emacs config" nil
  "f e d" "open config" #'xl-sk-open-dotfile

  "g" "git" nil
  "g s" "status" #'magit-status  "g b" "blame" #'magit-blame
  "g l" "file log" #'magit-log-buffer-file  "g d" "file diff" #'magit-diff-buffer-file
  "g m" "dispatch" #'magit-dispatch

  "h" "help" nil
  "h i" "info" #'info  "h m" "man" #'man
  "h d" "describe" nil
  "h d f" "function" #'describe-function  "h d v" "variable" #'describe-variable
  "h d k" "key" #'describe-key  "h d m" "mode" #'describe-mode
  "h d b" "bindings" #'describe-bindings  "h d p" "package" #'describe-package
  "h d a" "symbol" #'describe-symbol

  "i" "insert" nil
  "i s" "snippet" #'xueliang-insert-snippet
  "i S" "new snippet" #'xueliang-new-snippet

  "j" "jump" nil
  "j j" "to chars" #'avy-goto-char-timer  "j l" "to line" #'avy-goto-line
  "j w" "to word" #'avy-goto-word-1  "j i" "imenu" #'imenu
  "j d" "dired jump" #'dired-jump  "j D" "dired jump other" #'dired-jump-other-window

  "n" "narrow" nil
  "n r" "region" #'narrow-to-region  "n f" "defun" #'narrow-to-defun
  "n s" "org subtree" #'org-narrow-to-subtree  "n w" "widen" #'widen

  "p" "project" nil
  "p f" "find file" #'project-find-file  "p p" "switch project" #'project-switch-project
  "p b" "project buffer" #'project-switch-to-buffer  "p d" "project dired" #'project-dired
  "p k" "kill buffers" #'project-kill-buffers  "p c" "compile" #'project-compile
  "p '" "eshell" #'project-eshell
  "p s" "search" #'consult-ripgrep

  "q" "quit" nil
  "q q" "quit" #'save-buffers-kill-terminal  "q Q" "kill emacs" #'kill-emacs
  "q r" "restart" #'restart-emacs  "q z" "close frame" #'delete-frame

  "r" "resume/registers" nil
  "r l" "resume list" #'vertico-repeat
  "r y" "kill ring" #'consult-yank-pop

  "s" "search" nil
  "s s" "this buffer" #'consult-line
  "s p" "project" #'consult-ripgrep
  "s j" "imenu" #'imenu
  "s c" "clear highlight" #'evil-ex-nohighlight

  "t" "toggles" nil
  "t n" "line numbers" #'display-line-numbers-mode
  "t r" "relative numbers" #'xl-sk-relative-line-numbers
  "t l" "truncate lines" #'toggle-truncate-lines
  "t w" "whitespace" #'whitespace-mode
  "t f" "fill column" #'display-fill-column-indicator-mode
  "t s" "syntax check" #'flymake-mode
  "t S" "spelling" #'flyspell-mode
  "t h" "highlight" nil
  "t h h" "current line" #'hl-line-mode

  "T" "UI" nil
  "T n" "next theme" #'xl-sk-cycle-theme  "T s" "choose theme" #'load-theme
  "T F" "fullscreen" #'toggle-frame-fullscreen  "T M" "maximize" #'toggle-frame-maximized

  "w" "windows" nil
  "w h" "left" #'evil-window-left  "w j" "down" #'evil-window-down
  "w k" "up" #'evil-window-up  "w l" "right" #'evil-window-right
  "w H" "move far left" #'evil-window-move-far-left  "w J" "move to bottom" #'evil-window-move-very-bottom
  "w K" "move to top" #'evil-window-move-very-top  "w L" "move far right" #'evil-window-move-far-right
  "w /" "split right" #'xueliang-split-right  "w -" "split below" #'xueliang-split-below
  "w v" "split right" #'xueliang-split-right  "w s" "split below" #'xueliang-split-below
  "w d" "close" #'delete-window  "w m" "maximize" #'delete-other-windows
  "w =" "balance" #'balance-windows  "w w" "other window" #'other-window
  "w u" "undo layout" #'winner-undo  "w U" "redo layout" #'winner-redo)

;;; Major-mode leader: `,' in normal state and `SPC m' resolve per mode.
;;  A mode without a map keeps evil's `,' (repeat f/t backwards).
(defvar xl-major-leader-maps nil "Alist of (MODE . KEYMAP) for `,' and SPC m.")
(defun xl-sk--major-map (&rest _)
  (cdr (seq-find (lambda (entry) (derived-mode-p (car entry))) xl-major-leader-maps)))
(defun xl-sk--comma (&rest _)
  (or (xl-sk--major-map) #'evil-repeat-find-char-reverse))
(evil-define-key '(normal visual motion) 'global (kbd ",")
  '(menu-item "major mode" nil :filter xl-sk--comma))
(keymap-set xl-leader-map "m" '(menu-item "major mode" nil :filter xl-sk--major-map))

(defvar-keymap xl-org-leader-map :doc "Spacemacs org layer keys.")
(xl-sk-bind xl-org-leader-map
  "," "C-c C-c" #'org-ctrl-c-ctrl-c  "'" "edit block" #'org-edit-special
  "*" "toggle heading" #'org-ctrl-c-star  "-" "toggle item" #'org-ctrl-c-minus
  "RET" "C-c RET" #'org-ctrl-c-ret  "a" "agenda" #'org-agenda  "c" "capture" #'org-capture
  "l" "open link" #'org-open-at-point  "p" "priority" #'org-priority
  "A" "archive subtree" #'org-archive-subtree
  "b" "babel" nil
  "b b" "execute buffer" #'org-babel-execute-buffer  "b e" "execute block" #'org-babel-execute-src-block
  "C" "clock" nil
  "C i" "clock in" #'org-clock-in  "C o" "clock out" #'org-clock-out
  "C g" "goto clock" #'org-clock-goto
  "d" "dates" nil
  "d d" "deadline" #'org-deadline  "d s" "schedule" #'org-schedule
  "d t" "timestamp" #'org-time-stamp  "d T" "inactive timestamp" #'org-time-stamp-inactive
  "e" "export" nil
  "e e" "export" #'org-export-dispatch
  "i" "insert" nil
  "i l" "link" #'org-insert-link  "i h" "heading" #'org-insert-heading
  "i s" "subheading" #'org-insert-subheading  "i t" "tags" #'org-set-tags-command
  "i p" "property" #'org-set-property
  "s" "subtrees" nil
  "s r" "refile" #'org-refile  "s n" "narrow" #'org-narrow-to-subtree  "s N" "widen" #'widen
  "s s" "sparse tree" #'org-sparse-tree  "s h" "promote" #'org-promote-subtree
  "s l" "demote" #'org-demote-subtree  "s j" "move down" #'org-move-subtree-down
  "s k" "move up" #'org-move-subtree-up  "s S" "sort" #'org-sort
  "t" "tables" nil
  "t a" "align" #'org-table-align  "t c" "convert region" #'org-table-convert-region
  "t r" "recalculate" #'org-table-recalculate  "t s" "sort" #'org-table-sort-lines
  "T" "toggles" nil
  "T c" "checkbox" #'org-toggle-checkbox  "T i" "inline images" #'org-toggle-inline-images
  "T l" "link display" #'org-toggle-link-display  "T t" "todo" #'org-todo)
(push (cons 'org-mode xl-org-leader-map) xl-major-leader-maps)

(defvar-keymap xl-elisp-leader-map :doc "Spacemacs emacs-lisp layer keys.")
(xl-sk-bind xl-elisp-leader-map
  "e" "eval" nil
  "e e" "last sexp" #'eval-last-sexp  "e f" "defun" #'eval-defun
  "e b" "buffer" #'eval-buffer  "e r" "region" #'eval-region
  "g" "goto" nil
  "g g" "definition" #'xref-find-definitions  "g G" "references" #'xref-find-references
  "h" "help" nil
  "h h" "describe symbol" #'describe-symbol)
(push (cons 'emacs-lisp-mode xl-elisp-leader-map) xl-major-leader-maps)

;;; Home buffer: SPC b h, recent files 1-9, startup time at the bottom
(defun xl-sk-home-init-time ()
  "Insert Emacs' startup time, to two decimals, three blank lines below the links."
  (fancy-splash-insert
   :face '(variable-pitch font-lock-comment-face italic)
   (format "\n\n\nEmacs loaded in %.2f seconds\n"
           (float-time (time-subtract after-init-time before-init-time)))))

(defvar-local xl-sk--home-files nil "Recent files listed on the home buffer.")
(defun xl-sk-home-recentf ()
  "Insert up to 9 recent files, each with the number that opens it."
  ;; One spelling per file: /Users/me/x and ~/x are the same entry
  (setq recentf-list (seq-uniq (mapcar #'abbreviate-file-name recentf-list)))
  (setq xl-sk--home-files (seq-take recentf-list 9))
  (when xl-sk--home-files
    (fancy-splash-insert :face '(variable-pitch font-lock-string-face italic)
                         "Recent Files:\n")
    (seq-do-indexed
     (lambda (file i)
       (fancy-splash-insert :face 'default (format "[%d]  " (1+ i))
                            :link (list (abbreviate-file-name file)
                                        (lambda (_button) (find-file file)))
                            "\n"))
     xl-sk--home-files)))
(setq crafted-startup-module-list '(xl-sk-home-recentf))

(defun xl-sk-home-tail (&optional _concise)
  "Bottom of the home buffer: quick links, then the startup time."
  (fancy-splash-insert
   :face 'variable-pitch
   "\nTo start...     "
   :link `("Open a File" ,(lambda (_button) (call-interactively #'find-file)))
   "     "
   :link `("Open Home Directory" ,(lambda (_button) (dired "~")))
   "     "
   :link `("Open Dotfiles" ,(lambda (_button) (dired xl-dotfiles)))
   "\n")
  (xl-sk-home-init-time))
(advice-add 'crafted-startup-tail :override #'xl-sk-home-tail)

(defun xl-sk--home-keys (&rest _)
  "In the home buffer, digits 1-9 open the listed recent files."
  (with-current-buffer "*Crafted Emacs*"
    (kill-local-variable 'browse-url-browser-function) ; Crafted sets eww here
    (dotimes (i 9)
      (let ((n i))
        (dolist (state '(normal motion))
          (evil-local-set-key state (kbd (number-to-string (1+ n)))
                              (lambda () (interactive)
                                (when-let* ((file (nth n xl-sk--home-files)))
                                  (find-file file)))))))))
(advice-add 'crafted-startup--display-startup-screen :after #'xl-sk--home-keys)

(defun xl-sk-home-buffer () "Redraw and show the Crafted home buffer." (interactive)
  (crafted-startup-screen))

;;; Leader keys that sit on Spacemacs' own positions, with the commands I use
(xl-sk-bind xl-leader-map
  "RET" "switch buffer"  #'consult-buffer
  "?"   "all keys"       #'which-key-show-top-level
  "*"   "project symbol" #'xueliang-search-project-at-point
  "/"   "search buffer"  #'consult-line
  "x"   "M-x"            #'execute-extended-command
  "b w" "read-only"      #'read-only-mode
  "f T" "imenu"          #'imenu
  "g f" "git files"      #'project-find-file
  "s i" "imenu"          #'imenu
  "w c" "close window"   #'delete-window
  "w g" "golden ratio"   #'golden-ratio)

;;; Vim extras that ship with Spacemacs
(setq evil-escape-key-sequence "fd" evil-escape-delay 0.1)
(when (fboundp 'evil-escape-mode) (evil-escape-mode 1))
(when (fboundp 'global-evil-surround-mode) (global-evil-surround-mode 1))
(setq winum-auto-setup-mode-line t)
(when (fboundp 'winum-mode) (winum-mode 1))

(provide 'xl-key-bindings)
;;; xl-key-bindings.el ends here
