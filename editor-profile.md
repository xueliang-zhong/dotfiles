# References:
- http://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
- https://www.youtube.com/watch?v=SzA2YODtgK4
- https://www.youtube.com/watch?v=5FQwQ0QWBTU
- http://ergoemacs.org/emacs/elisp_examples.html
- http://ergoemacs.org/emacs/command-frequency.html
- https://github.com/emacs-helm/helm
- https://github.com/emacs-tw/awesome-emacs
- https://github.com/caiorss/Emacs-Elisp-Programming





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
- C-x-0 to kill current window (the emacs way).


# Shell
## vim+tmux
- vim+tmux provides shell and vim in the same window.
## emacs
- M-x shell

# Run Shell command
## vim+tmux
- :!
## emacs
- in evil mode, :!CMD as well.
- M-| on selected text.
- M-1 M-!


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
- helm-do-grep-ag


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
- company-complete and C-g to quit completion.


# Tag Search/Jumps
## vim+tmux
- C-], C-t
## emacs
- Same as vim in evil-mode.
- https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Tag.html
- xref-find-definitions
- xref-find-references


# Dynamic Evaluation of Expression
## vim+tmux
- (none)
## emacs
- M-x pp-eval-last-sexp in buffer.

# High-light current line
## vim+tmux
- :set cursorline
## emacs
- M-x hl-line-mode

# Line number
## vim+tmux
- :set nu
## emacs
- M-x linum-mode

# Source Function Browsing
## vim+tmux
- Tagbar
## emacs
- M-x semantic-mode
- M-x helm-semantic-or-imenu


# Diffs
## vim+tmux
- vimdiff
## emacs

# High light certain column (for code review)
## vim+tmux
- set colorcolumn=100
## emacs
- whitespace/column-marker

# git integration
## vim+tmux
- git plugs: vim-fugitive, vim-gitgutter
## emacs
- magit

# spell
## vim+tmux
- :set spell
## emacs
- M-x flyspell-mode
- C-M-i correct word in flyspell-mode.
- or use "z =", ispell-word


# Find file
## vim+tmux
- Nerdtree
## emacs
- helm-find-files


# Font Size
## vim+tmux
## emacs
- C-x C-= / C-x C-- / C-x C-0


;; Org mode
## emacs
- move items up and down: Alt+Up/Down
- change items indent: Alt+Left/Right
- shift+Left/Right: change items' bullets.
- TAB to fold/collapse items.
- Shift-TAB to fold/collapse whole doc.



;; Emacs Functions
execute-extended-command
keyboard-quit
command-log-mode


----------------------------------------------------------

# [title]
## vim+tmux
## emacs
