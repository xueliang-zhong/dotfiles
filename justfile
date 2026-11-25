###########################
# emacs & nvim config files
###########################

all: install maintenance-check # docker-build
make: all

list:
    @echo just get familiar with justfile for this project
    just --list

install:
    @bash env.sh
    @echo "Scripts Installed"

maintenance-check:
  @echo "maintenance check of vimrc/init.lua/spacemacs"
  @echo "vimrc (goal <= 250 LOC):" && wc -l vimrc
  @echo "init.lua (goal <= 250 LOC):" && wc -l init.lua
  @echo "emacs config (goal <= 200 LOC):"
  @grep -v "^[ ]*;" spacemacs.el | grep -v "^[ ]*$" | grep -A 1000 "defun.*dotspacemacs/user-config" | wc -l

emacs-dotfiles-clean:
  # make sure no other emacs files under $HOME
  rm -rf ~/.emacs.d
  rm -rf ~/.emacs*
  rm -rf ~/.spacemacs*
  rm -rf ~/.config/emacs

doom-emacs-install:
  # standard installation
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
  ~/.config/emacs/bin/doom install
  # a must step after installation
  ~/.config/emacs/bin/doom sync

spacemacs-install:
  # to be safe, not depending on emacs-dotfiles-clean
  git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

###########################
# Emacs profiles: try before buy
# Each profile is its own init directory under ~/.emacs-profiles and is
# started with --init-directory, so it never touches ~/.emacs.d (Spacemacs).
#
# The recipes below only wrap tuned-emacs/install/emacs-setup.sh, so the same setup
# works on a machine without `just`:
#   bash tuned-emacs/install/emacs-setup.sh setup-macos
# The script holds the checks (platform, Emacs 30+, profile), the Crafted
# clone, the profile stubs and the trampoline cache; the recipes pass
# EMACS/PROFILE/PROFILE_DIR/DOTFILES through.
#
# Overrides: EMACS=/path/to/emacs just emacs-try
#            just PROFILE=other emacs-check   (or PROFILE_DIR=/some/dir)
###########################

# The `emacs` on PATH (Emacs 31; on macOS /Applications/Emacs.app).
EMACS := env_var_or_default("EMACS", "emacs")
# Forward slashes everywhere: on Windows HOME may be C:\Users\me, and a
# backslash is an escape both in sh command lines and in Elisp strings.
HOME_DIR := replace(env_var_or_default("HOME", env_var_or_default("USERPROFILE", "")), "\\", "/")
PROFILES := HOME_DIR / ".emacs-profiles"
DOTFILES := replace(justfile_directory(), "\\", "/")
PROFILE := "crafted"
PROFILE_DIR := PROFILES / PROFILE
# What the script reads from the environment, quoted for sh.
XL_ENV := "EMACS='" + EMACS + "' PROFILE='" + PROFILE + "' PROFILES='" + PROFILES + "' PROFILE_DIR='" + PROFILE_DIR + "' DOTFILES='" + DOTFILES + "'"

# clone Crafted Emacs at the locked commit and generate the profile stubs in DIR
emacs-crafted-install dir=PROFILE_DIR:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh crafted-install '{{dir}}'

# start the profile next to the default Emacs
emacs-try:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh try

# The first run also installs the packages.
# load the profile in batch mode; fails on any init error
emacs-smoke:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh smoke

# run the ERT suite against the loaded profile: keys, hooks, files, behaviour
emacs-test:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh test

# everything a config change must pass: smoke + ERT suite
emacs-check:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh check

# build the subr trampolines in batch, so a GUI start never compiles them
emacs-trampolines-cache-setup:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh trampolines

# macOS/Linux: make Crafted the default Emacs (~/.emacs.d -> the profile)
emacs-make-default:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh make-default

# undo emacs-make-default: put Spacemacs back as ~/.emacs.d
emacs-restore-spacemacs:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh restore-spacemacs

# Windows (run in Git Bash): fresh clone to working Emacs in one command
emacs-windows-setup:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh setup-windows

###########################
# macOS: one command does all
###########################

# macOS (Terminal): dotfiles + Crafted Emacs in one command
emacs-mac-setup:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh setup-macos

###########################
# Linux: one command does all
###########################

# Linux: dotfiles + Crafted Emacs in one command
emacs-linux-setup:
    @{{XL_ENV}} bash tuned-emacs/install/emacs-setup.sh setup-linux

###########################
# Docker Environment
###########################

IMAGE := "ubuntu:24.04"
CONTAINER := "ubuntu-24.04-container"

docker-build:
    docker build -t {{IMAGE}} .

docker-run:
    docker run -it --name {{CONTAINER}} -v ~/workspace:/home/xuezho01/workspace {{IMAGE}}

docker-attach:
    docker exec -it {{CONTAINER}} zsh

docker-clean:
    docker stop {{CONTAINER}}
    docker rm   {{CONTAINER}}
