# References:
- http://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
- https://www.youtube.com/watch?v=SzA2YODtgK4
- https://www.youtube.com/watch?v=5FQwQ0QWBTU

# Moving & Editing
## vim+tmux
- vim editing
## emacs
- evil-mode

# Multi-Window
## vim+tmux
- :sp, :vs
- With plugins: C-j/k/l/h between vim windows & tmux panes.
## emacs
- same as vim in evil-mode, for example :sp, :vs.


# Shell
## vim+tmux
- vim+tmux provides shell and vim in the same window.
## emacs
- M-x shell


# Session
## vim+tmux
- attach to a session: tmux attach
- detach from a session: '<prefix> d'
## emacs
-
-


# Completion
## vim+tmux
- basic completion: C-N, C-P
## emacs
- in evil mode, basic completion: C-N


# Buffer Exploration
## vim+tmux
- vim :buffers, :BufExplorer
## emacs
- in evil mode, :buffers


# Escape from command mode
## vim+tmux
- ESC
## emacs
- C-g

# Execute a function
## vim+tmux
-
## emacs
- M-x (func), e.g. M-x (move-begninning-of-line)


# tags
## vim+tmux
- ctags, file tags.
## emacs
- etags, file TAGS.


# Package/Plugin mangement
## vim+tmux
- Vundle
## emacs
  (require 'package)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)


# Key binding/mapping
## vim+tmux
- map
## emacs
- (global-set-key): https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html


# grep
## vim+tmux
- :grep, :vimgrep, :Ag
## emacs
- M-x grep RET


# Auto Complete
## vim+tmux
- C-n, C-p
## emacs
- M-/
- in evil-mode: C-n, C-p
- (hippie-expand)
- (helm-complete-file-name-at-point)
- (helm-complex-command-history)
- company-mode package

-----------------------------------------------------------

# [title]
## vim+tmux
## emacs

