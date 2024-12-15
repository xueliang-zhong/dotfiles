#!/bin/bash
# vim
rm -f ~/.vimrc
ln -sf $PWD/vimrc ~/.vimrc

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
echo DONE
