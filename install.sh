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
rm -f ~/.spacemacs
ln -sf `pwd`/spacemacs.el ~/.spacemacs

# emacs.d
rm -rf ~/.emacs.d/xueliang/
mkdir  ~/.emacs.d/xueliang/

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
ln -s /usr/bin/nemo ~/bin/files
ln -s /usr/bin/gnome-terminal ~/bin/terminal

# dropbox
ln -s ~/Dropbox ~/workspace/dropbox

# gdb
rm -f ~/.gdbinit
ln -s `pwd`/gdbinit ~/.gdbinit

# DONE
echo DONE

