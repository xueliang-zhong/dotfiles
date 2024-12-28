#!/bin/bash
# source install.sh

# Define a function for cp to ensure -f flag is always used
CP_CMD() {
    cp -f "$@"
}

# vim
rm -f ~/.vimrc
CP_CMD ./vimrc ~/.vimrc

# nvim
mkdir -p ~/.config/nvim/
rm -f ~/.config/nvim/init.lua
CP_CMD ./init.lua ~/.config/nvim/init.lua

# doom-emacs
mkdir -p ~/.doom.d/
rm -f ~/.doom.d/init.el
rm -f ~/.doom.d/config.el
CP_CMD ./doom-init.el   ~/.doom.d/init.el
CP_CMD ./doom-config.el ~/.doom.d/config.el

# tmux
rm -f ~/.tmux.conf
CP_CMD ./tmux.conf ~/.tmux.conf

# alacrity
rm -f ~/.config/alacritty/alacritty.toml
CP_CMD ./alacritty.toml ~/.config/alacritty/alacritty.toml

# ~/bin
mkdir -p ~/bin/
rm -rf ~/bin/links
CP_CMD ./links ~/bin/

