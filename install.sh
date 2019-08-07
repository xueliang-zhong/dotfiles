# ~/bin
mkdir -f ~/bin/

# vim
rm -f ~/.vimrc ~/.config/nvim/init.vim
ln -sf `pwd`/vimrc ~/.vimrc
ln -sf `pwd`/vimrc ~/.config/nvim/init.vim
ln -sf `pwd`/open_link_arg2.sh ~/bin/open_link_arg2.sh
ln -sf `pwd`/daily-work.sh ~/bin/daily-work.sh

# emacs
rm -f ~/.emacs
rm -f ~/.spacemacs
ln -sf `pwd`/spacemacs.el ~/.spacemacs

# emacs eshell alias
rm -f ~/.emacs.d/eshell/alias
ln -sf `pwd`/alias ~/.emacs.d/eshell/alias

# desktop env
ln -s /usr/bin/xfce4-terminal  ~/bin/terminal

# dropbox
ln -s ~/Dropbox ~/workspace/dropbox

# DONE
echo DONE

