# vim
rm -f ~/.vimrc ~/.config/nvim/init.vim
ln -sf `pwd`/vimrc ~/.vimrc
ln -sf `pwd`/vimrc ~/.config/nvim/init.vim

# tmux
rm -f ~/.tmux.conf
rm -f ~/.window.tmux
ln -sf `pwd`/tmux.conf ~/.tmux.conf
ln -sf `pwd`/window.tmux ~/.window.tmux

# ctags settings
rm -f ~/.ctags ~/bin/ctag
ln -sf `pwd`/ctags.conf ~/.ctags
ln -sf `pwd`/ctag ~/bin/ctag

# m command
rm -f ~/bin/m
ln -sf `pwd`/m ~/bin/m

# emacs
rm -f ~/.emacs
ln -sf `pwd`/emacs.el ~/.emacs

# emacs.d
rm -rf ~/.emacs.d/xueliang/
mkdir  ~/.emacs.d/xueliang/

# my-editor
rm -f ~/bin/git-rebase-head
ln -sf `pwd`/git-rebase-head ~/bin/git-rebase-head

# emacs eshell alias
rm -f ~/.emacs.d/eshell/alias
ln -sf `pwd`/alias ~/.emacs.d/eshell/alias

# so that emacs can start chrome using 'browse-url-chromium'
rm -f ~/bin/chromium
ln -s /usr/bin/google-chrome ~/bin/chromium

# Android aosp/linaro development
rm -f ~/bin/cpplint.py
rm -f ~/bin/adb
ln -s ~/workspace/aosp/art/tools/cpplint.py  ~/bin/cpplint.py
ln -s ~/workspace/aosp/out/host/linux-x86/bin/adb  ~/bin/adb

# gnome environment
rm -f ~/bin/files
rm -f ~/bin/terminal
ln -s /usr/bin/nautilus ~/bin/files
ln -s /usr/bin/gnome-terminal ~/bin/terminal

# dropbox
ln -s ~/Dropbox ~/workspace/dropbox

# use ipython instead of python
ln -s /usr/bin/ipython ~/bin/python

# DONE
echo DONE

