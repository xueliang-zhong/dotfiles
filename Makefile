###########################
# emacs & nvim config files
###########################

all: install
	@ls -l ~/.config/nvim/init.lua
	@ls -l ~/.vimrc
	@ls -l ~/.doom.d/*.el
	@echo "Scripts Installed"

install:
	bash env.sh

###########################
# Docker Environment
###########################

IMAGE = ubuntu-24.10
CONTAINER = $(IMAGE)-container

docker-build:
	docker build -t $(IMAGE) .

docker-run:
	docker run -it --name $(CONTAINER) -v ~/workspace:/home/xuezho01/workspace $(IMAGE)

docker-attach:
	docker exec -it $(CONTAINER) zsh

docker-clean:
	docker stop $(CONTAINER)
	docker rm $(CONTAINER)
