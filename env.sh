#!/bin/bash
# source install.sh

export NVIM_HOME=~/.config/nvim

# vim
rm -f ~/.vimrc
ln -sf $PWD/vimrc ~/.vimrc

# nvim
rm -rf $NVIM_HOME
mkdir -p $NVIM_HOME
ln -sf $PWD/init.lua $NVIM_HOME/init.lua

# emacs
mkdir -p ~/.doom.d/
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

# Git Config
git config --global user.name "Xueliang Zhong"
git config --global user.email "xueliang.zhong@gmail.com"

# Oh My Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

#
# My fzf based quick commands
#
alias d='echo $(dirs | sed "s/ /\n/g"| fzf)'
alias f='vim  $(fzf --preview "cat {}")'
alias ff=f
alias r='eval $(fc -ln 10000 | fzf --no-sort --reverse --height 40%)'
alias h=r
alias x=r

# DONE
echo "Done copying configs and scripts"
