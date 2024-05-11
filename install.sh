#!/bin/bash

# ~/bin
mkdir -p ~/bin/

# vim
rm -f ~/.vimrc
ln -sf ${PWD}/vimrc ~/.vimrc

# emacs
rm -f ~/.doom.d/config.el
ln -sf ${PWD}/doom-config.el ~/.doom.d/config.el

# tmux
rm -f ~/.tmux.conf
ln -sf ${PWD}/tmux.conf ~/.tmux.conf

# DONE
echo DONE
