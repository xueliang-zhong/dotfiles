#!/bin/bash
# source install.sh

# Define a function for cp to ensure -f flag is always used
CP_CMD() {
    cp -f "$@"
    echo copying "$@"
}

# emacs
rm -f ~/.spacemacs
CP_CMD ./spacemacs.el  ~/.spacemacs

# vim
rm -f ~/.vimrc
CP_CMD ./vimrc ~/.vimrc
# CP_CMD ./gvimrc ~/.vimrc

# nvim
mkdir -p ~/.config/nvim/
rm -f ~/.config/nvim/init.lua
CP_CMD ./init.lua ~/.config/nvim/init.lua

# doom-emacs
mkdir -p ~/.doom.d/
rm -f ~/.doom.d/init.el
rm -f ~/.doom.d/config.el
rm -f ~/.doom.d/packages.el
CP_CMD ./doom-init.el     ~/.doom.d/init.el
CP_CMD ./doom-config.el   ~/.doom.d/config.el
CP_CMD ./doom-packages.el ~/.doom.d/packages.el

# tmux
rm -f ~/.tmux.conf
CP_CMD ./tmux.conf ~/.tmux.conf

# ~/bin
mkdir -p ~/bin/
rm -rf ~/bin/links
CP_CMD ./links ~/bin/
