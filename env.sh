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

# ~/bin
mkdir -p ~/bin/
rm -rf ~/bin/links
CP_CMD ./links ~/bin/

#
# My fzf based quick commands
#
alias d='eval $(dirs | sed "s/ /\n/g"| fzf --reverse --height 30%)'
alias f='vim  $(fzf --preview "cat {}")'
alias ff=f
alias r='eval $(fc -ln 10000 | fzf --no-sort --height 60%)'
alias h='alias | grep "^[a-z]=" | fzf --height 40%'
alias x=r

# Other useful alias
alias python=python3
alias p=python
