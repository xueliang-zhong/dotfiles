rm -f ~/.vimrc
rm -f ~/.tmux.conf
rm -f ~/.ctags

###########################################

ln -sf `pwd`/vimrc ~/.vimrc
ln -sf `pwd`/vimrc ~/.config/nvim/init.vim

ln -sf `pwd`/tmux.conf ~/.tmux.conf

ln -sf `pwd`/m ~/bin/m

ln -sf `pwd`/ctags ~/.ctags
