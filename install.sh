# ~/bin
mkdir -p ~/bin/

# vim
ln -sf `pwd`/vimrc ~/.vimrc
ln -sf `pwd`/open_link_arg2.sh ~/bin/open_link_arg2.sh

# emacs
vimdiff `pwd`/doom-config.el ~/.doom.d/config.el

# DONE
echo DONE
