# AGENTS.md - Dotfiles Configuration

## Project Overview

This is a dotfiles repository containing configuration files for:
- **Neovim** (`init.lua`) - Lua-based configuration
- **Vim** (`vimrc`) - Vimscript configuration
- **Emacs/Spacemacs** (`spacemacs.el`, `doom-config-2025.el`)
- **Tmux** (`tmux.conf`)
- **Shell scripts** - Various utility scripts

## Build/Lint/Test Commands

Uses [`just`](https://github.com/casey/just) as the task runner.

```bash
# Install all configurations to home directory
just install

# Run maintenance checks (line counts)
just maintenance-check

# Docker development environment
just docker-build && just docker-run

# Emacs-specific installation
just emacs-dotfiles-clean
just doom-emacs-install
just spacemacs-install
```

Manual commands:
```bash
bash env.sh
wc -l vimrc init.lua spacemacs.el doom-config-2025.el
```

## Code Style Guidelines

### General Principles

- **Keep files small**: vimrc <= 250 LOC, init.lua <= 300 LOC, spacemacs user-config <= 200 LOC
- **Minimal dependencies**: Prefer built-in features over plugins
- **Cross-platform**: Ensure configurations work on macOS and Linux
- **Performance-first**: Avoid heavy plugins; prioritize speed

### Neovim (init.lua)

- **Formatting**: 2 spaces, snake_case for variables, PascalCase for modules
- Group related code with `local function ___section_name__()` markers
- **Imports**: Use `require()` for plugins, `vim.cmd()` for Vimscript
- **Error Handling**: Use `pcall` for potentially failing operations

```lua
vim.opt.clipboard = "unnamedplus"

local function ___basic_settings__() end
vim.opt.scrolloff = 3
```

### Vim (vimrc)

- **Formatting**: 2 spaces, `expandtab`, `shiftwidth=2`, `tabstop=2`
- Double quotes for comments, single quotes for strings
- Group settings with `" => Section"` headers

```vim
" => Global Settings
set nocompatible
set clipboard=unnamed,unnamedplus

" => Key mappings
let mapleader=" "
nnoremap <leader>ff :find **<C-d>
```

- **Naming**: CamelCase for functions (e.g., `XueliangIndexOrToday`), `g:` prefix for globals

### Emacs Lisp (spacemacs.el, doom-config-2025.el)

- **Formatting**: 2 spaces, `defcustom` for configurable vars, `setq-default` for buffer-local
- **Naming**: kebab-case, prefix with `dotspacemacs-` for Spacemacs functions

```elisp
(defun dotspacemacs/user-config ()
  "User configuration."
  (setq-default
   dotspacemacs-show-translation-package-names nil
   dotspacemacs-use-ido t))
```

### Tmux (tmux.conf)

- Consistent indentation, `#` for comments, group settings with `# Section`

```tmux
# General settings
set -g mouse on
set -g default-terminal "screen-256color"

# Key bindings
bind-key -n C-t new-window
```

### Shell Scripts

- Use bash (`#!/bin/bash`), 4 spaces indentation, `set -e` for error handling

```bash
#!/bin/bash
set -e

CP_CMD() {
    cp -f "$@"
    echo "copying $@"
}
```

## Editor-Specific Notes

**Neovim**: Source changes with `:luafile %`, check Lua errors with `:lua print(...)`

**Vim**: Source changes with `:source ~/.vimrc`

**Spacemacs/Emacs**: Evaluate with `C-x C-e`, reload with `SPC q r`

## File Structure

```
.
├── init.lua           # Neovim config (primary)
├── vimrc              # Vim config (fallback for nvim)
├── spacemacs.el       # Spacemacs config
├── doom-config-2025.el # Doom Emacs config
├── tmux.conf          # Tmux config
├── env.sh             # Installation script
├── justfile           # Task runner
└── bin/               # User scripts (installed to ~/bin/)
```

## Working with Git

```bash
git diff
git add <file>
git commit -m "description"
```

## Best Practices

1. **Test before committing**: Source config files in respective editors
2. **Keep backups**: Ensure old configs are backed up before overwriting
3. **Document new features**: Add comments for non-obvious configurations
4. **Cross-platform**: Test on both macOS and Linux when possible
5. **Minimalism**: Add new features only when necessary; prefer built-in solutions
