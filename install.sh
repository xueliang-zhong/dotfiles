#!/bin/bash

# vim
rm -f ~/.vimrc
ln -sf $PWD/vimrc ~/.vimrc

# nvim
mkdir -p ~/.config/nvim
ln -sf $PWD/init.lua ~/.config/nvim/init.lua

# emacs
rm -f ~/.doom.d/init.el
rm -f ~/.doom.d/config.el
ln -sf $PWD/doom-init.el ~/.doom.d/init.el
ln -sf $PWD/doom-config.el ~/.doom.d/config.el

# tmux
rm -f ~/.tmux.conf
ln -sf $PWD/tmux.conf ~/.tmux.conf

# ~/bin
mkdir -p ~/bin/
cp -f $PWD/links ~/bin/

# DONE
echo "Done copying configs and scripts"
echo "Remember to run: ~/.config/emacs/bin/doom sync"
