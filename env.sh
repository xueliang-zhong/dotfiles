#!/bin/bash
# source install.sh

# Define a function for cp to ensure -f flag is always used
CP_CMD() {
    cp -f "$@"
    echo copying "$@"
}

# vim (Goal: <= 250 LOC)
rm -f ~/.vimrc
CP_CMD ./vimrc ~/.vimrc

# nvim (Goal: <= 200 LOC)
mkdir -p ~/.config/nvim/
rm -f ~/.config/nvim/init.lua
CP_CMD ./init.lua ~/.config/nvim/init.lua

# emacs (Goal: <= 200 LOC)
rm -f ~/.spacemacs
CP_CMD ./spacemacs.el  ~/.spacemacs

# tmux
rm -f ~/.tmux.conf
CP_CMD ./tmux.conf ~/.tmux.conf

# ~/bin
mkdir -p ~/bin/
rm -rf ~/bin/links
CP_CMD ./links ~/bin/
