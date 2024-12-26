#!/bin/bash
# source install.sh

export NVIM_HOME=~/.config/nvim
DOT_FILE_PATH=$(realpath env.sh | xargs dirname)
echo "DOT_FILE_PATH = " $DOT_FILE_PATH

# vim
rm -f ~/.vimrc
ln -sf $DOT_FILE_PATH/vimrc ~/.vimrc

# nvim
rm -rf $NVIM_HOME
mkdir -p $NVIM_HOME
ln -sf $DOT_FILE_PATH/init.lua $NVIM_HOME/init.lua

# emacs
mkdir -p ~/.doom.d/
rm -f ~/.doom.d/init.el
rm -f ~/.doom.d/config.el
ln -sf $DOT_FILE_PATH/doom-init.el ~/.doom.d/init.el
ln -sf $DOT_FILE_PATH/doom-config.el ~/.doom.d/config.el

# tmux
rm -f ~/.tmux.conf
ln -sf $DOT_FILE_PATH/tmux.conf ~/.tmux.conf

# ~/bin
mkdir -p ~/bin/
cp -f $DOT_FILE_PATH/links ~/bin/

# Git Config
git config --global user.name "Xueliang Zhong"
git config --global user.email "xueliang.zhong@gmail.com"

# Fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

# Oh My Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

#
# My fzf based quick commands
#
alias d='echo $(dirs | sed "s/ /\n/g"| fzf)'
# alias f='vim  $(fzf --preview "cat {}")'
alias f='vim  $(fzf --preview "bat {} --style=plain --color=always")'
alias ff=f
alias r='eval $(fc -ln 10000 | fzf --no-sort --reverse --height 40%)'
alias h=r
alias x=r

# DONE
echo "Done copying configs and scripts"
