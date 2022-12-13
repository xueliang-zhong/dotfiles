#!/bin/bash

# ~/bin
mkdir -p ~/bin/

# vim
rm -f ~/.vimrc
ln -sf `pwd`/vimrc ~/.vimrc

# emacs
rm -f ~/.doom.d/config.el
ln -sf `pwd`/doom-config.el ~/.doom.d/config.el

# DONE
echo DONE
