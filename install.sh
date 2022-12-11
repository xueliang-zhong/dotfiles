# ~/bin
mkdir -f ~/bin/

# vim
ln -sf `pwd`/vimrc ~/.vimrc
ln -sf `pwd`/open_link_arg2.sh ~/bin/open_link_arg2.sh

# emacs
cp -f `pwd`/doom-config.el ~/.doom.d/config.el

# emacs eshell alias
rm -f ~/.emacs.d/eshell/alias
ln -sf `pwd`/alias ~/.emacs.d/eshell/alias

# dropbox
ln -sf ~/Dropbox ~/workspace/dropbox

# DONE
echo DONE
