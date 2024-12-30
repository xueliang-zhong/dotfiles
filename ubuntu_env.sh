#
# Heavier installation
#

source env.sh

# Git Config
git config --global user.name "Xueliang Zhong"
git config --global user.email "xueliang.zhong@arm.com"
export EDITOR='vim'

#
# installation order matters
#

# Oh My Zsh (generate basic .zshrc first)
if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi

# fzf (add fzf on top of .zshrc)
if [ ! -d "$HOME/.fzf" ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install --all
fi

if [ ! -d "~/bin/lazygit" ]; then
    echo "Installing lazygit ..."
    git clone https://github.com/jesseduffield/lazygit.git &> /dev/null
    cd lazygit
    go install
    cd -
fi
cp -f ~/go/bin/lazygit ~/bin/
echo "export PATH=$PATH:$HOME/bin/" >> ~/.zshrc

# Set ZSH_THEME "essembeh"
sed -i 's/^ZSH_THEME=.*/ZSH_THEME="essembeh"/' ~/.zshrc

# Append following lines to ~/.zshrc using EOF technique
if ! grep -q "# My fzf based quick commands" ~/.zshrc; then
  cat << 'EOF' >> ~/.zshrc
#
# My fzf based quick commands
#
alias d='eval $(dirs | sed "s/ /\n/g" | fzf --reverse --height 30%)'
alias f='vim $(fzf --preview "batcat --style=numbers --color=always --line-range=:500 {}")'
alias ff=f
alias r='eval $(fc -ln | fzf --no-sort --height 60%)'
alias h='alias | grep "[a-z]=" | fzf --height 40%'
alias x=r

# Other useful alias
alias python=python3
alias p=python
alias ag=rg
# NOTE: keep make as it is (avoid alias make=just), just in case some other project requires make


# just command completion for shell
just --completions zsh > ~/.just-completions.zsh
echo >> ~/.zshrc
echo "[ -f ~/.just-completions.zsh ] && source ~/.just-completions.zsh" >> ~/.zshrc

EOF
  echo "Awesome changes made to ~/.zshrc."
fi
