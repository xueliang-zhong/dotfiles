#!/bin/bash
# Copy vim/nvim/tmux configs and ~/bin scripts into $HOME.
# Emacs is managed separately: `just emacs-mac-setup` or
# `bash emacs/install/emacs-setup.sh setup-macos` (same script).
set -e

# Define a function for cp to ensure -f flag is always used
CP_CMD() {
    cp -f "$@"
    echo copying "$@"
}

# vim (Goal: <= 250 LOC)
rm -f ~/.vimrc
CP_CMD ./vimrc ~/.vimrc

# nvim (Goal: <= 300 LOC)
mkdir -p ~/.config/nvim/
rm -f ~/.config/nvim/init.lua
CP_CMD ./init.lua ~/.config/nvim/init.lua

# tmux
rm -f ~/.tmux.conf
CP_CMD ./tmux.conf ~/.tmux.conf

# ~/bin
mkdir -p ~/bin/
rm -rf ~/bin/links
CP_CMD ./links ~/bin/

rm -rf ~/bin/org-table-to-dotty.sh
CP_CMD ./org-table-to-dotty.sh ~/bin/
