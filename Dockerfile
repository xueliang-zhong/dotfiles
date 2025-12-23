FROM ubuntu:24.04

# Install necessary packages (AOSP & Yocto build inspired)
RUN apt-get update && apt-get install -y \
    bat binutils bison build-essential chrpath cmake cpio curl debianutils diffstat  \
    file flex fontconfig gawk gcc gettext git git-core gnupg htop iputils-ping  \
    libacl1 libgl1-mesa-dev libxml2-utils \
    libvulkan-dev vulkan-tools \
    locales make just nano neovim golang-go ninja-build python3 python3-git python3-jinja2 \
    python3-pexpect python3-pip python3-subunit ripgrep socat sudo texinfo tree  \
    universal-ctags unzip wget xsltproc xz-utils zip zlib1g-dev zsh zstd \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Add user
ARG USERNAME=xuezho01
ARG PASSWORD=asdfjkl;
RUN useradd -m -s /usr/bin/zsh ${USERNAME} && \
    echo "${USERNAME}:${PASSWORD}" | chpasswd && \
    usermod -aG sudo ${USERNAME}

# Set up workspace
RUN mkdir -p /home/${USERNAME}/workspace && \
    chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}/workspace

# Clone dotfiles repository and run setup script
USER ${USERNAME}
WORKDIR /home/${USERNAME}/workspace
RUN git clone https://github.com/xueliang-zhong/dotfiles && \
    cd dotfiles && bash ubuntu_env.sh

# Set default working directory
WORKDIR /home/${USERNAME}/workspace/dotfiles

