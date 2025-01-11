###########################
# emacs & nvim config files
###########################

all: just-list install maintenance-check # docker-build
make: all

just-list:
    @echo just get familiar with justfile for this project
    just --list

install:
    @ls -l ~/.config/nvim/init.lua
    @ls -l ~/.vimrc
    @bash env.sh
    @echo "Scripts Installed"

maintenance-check:
  @echo "maintenance check of vimrc/init.lua/spacemacs"
  @echo "vimrc (goal <= 250 LOC):" && wc -l vimrc
  @echo "init.lua (goal <= 250 LOC):" && wc -l init.lua
  @echo "doom-emacs (goal <= 250 LOC):" && grep -v "^[ ]*;" doom-config-2025.el | wc -l

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
