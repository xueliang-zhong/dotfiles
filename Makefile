all: install
	@ls -l ~/.config/nvim/init.lua
	@ls -l ~/.vimrc
	@ls -l ~/.doom.d/*.el
	@echo "Scripts Installed"

install:
	bash env.sh

