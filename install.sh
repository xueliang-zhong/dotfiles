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
rm -f ~/.ctags
ln -sf `pwd`/ctags ~/.ctags

# m command
rm -f ~/bin/m
ln -sf `pwd`/m ~/bin/m

