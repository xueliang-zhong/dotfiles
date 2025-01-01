###########################
# emacs & nvim config files
###########################

all: just-list install # docker-build
make: just-list install

just-list:
    @echo just get familiar with justfile for this project
    just --list

install:
    @ls -l ~/.config/nvim/init.lua
    @ls -l ~/.vimrc
    @bash env.sh
    @echo "Scripts Installed"

###########################
# Docker Environment
###########################

IMAGE := "ubuntu-24.10"
CONTAINER := "ubuntu-24.10-container"

docker-build:
    docker build -t {{IMAGE}} .

docker-run:
    docker run -it --name {{CONTAINER}} -v ~/workspace:/home/xuezho01/workspace {{IMAGE}}

docker-attach:
    docker exec -it {{CONTAINER}} zsh

docker-clean:
    docker stop {{CONTAINER}}
    docker rm   {{CONTAINER}}
