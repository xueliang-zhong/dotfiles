-- Basic Settings
local function ___basic_settings__() end

vim.opt.number = true          -- Show line numbers
vim.opt.relativenumber = false -- Relative line numbers
vim.opt.cursorline = true      -- highlight current line
vim.opt.tabstop = 4            -- Number of spaces a <Tab> counts for
vim.opt.shiftwidth = 4         -- Number of spaces for indentation
vim.opt.expandtab = true       -- Use spaces instead of tabs
vim.opt.smartindent = true     -- Auto-indent new lines
vim.opt.wrap = false           -- Don't wrap lines
vim.opt.ignorecase = true      -- Case-insensitive search
vim.opt.smartcase = true       -- Smart case sensitivity
vim.opt.splitright = true      -- Open vertical splits to the right
vim.opt.splitbelow = true      -- Open horizontal splits below
vim.opt.autochdir = true       -- Automatically changes the current working directory of current file.
vim.g.loaded_netrw = 1         -- Required for neo-tree
vim.g.loaded_netrwPlugin = 1   -- Required for neo-tree
vim.opt.clipboard = "unnamedplus" -- System clipboard for MacOS
vim.opt.tags = ""
vim.opt.guicursor = ""
vim.opt.timeoutlen = 100       -- Makes leader key more responsive in INSERT mode

-- Plugin Management with lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    "folke/which-key.nvim",
    "tpope/vim-commentary",            -- commenting plugin
    "nvim-lualine/lualine.nvim",       -- nice status line

    -- IDE
    "nvim-tree/nvim-tree.lua",         -- nerdtree for neovim
    "liuchengxu/vista.vim",
    "nvim-tree/nvim-web-devicons",     -- needed by IDE Plugins

    "nvim-telescope/telescope.nvim",   -- telescope fuzzy finder
    "nvim-lua/plenary.nvim",           -- dependency for telescope

    "nvim-treesitter/nvim-treesitter", -- main plugin for tree-sitter

    -- Git
    "tpope/vim-fugitive",              -- Gread, Gwrite
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",         -- required
            "sindrets/diffview.nvim",        -- optional - Diff integration
        },
        config = true
    },

    -- LSP support
    -- NOTE: don't have to enable it, just keep nvim light weight & simple

    -- themes --
    "catppuccin/nvim",

    -- doesn't work across the board on different neovim version I use
    -- "nvim-treesitter/nvim-treesitter-context",
})

-- Colorscheme
require("catppuccin").setup({})
vim.opt.termguicolors = true
vim.cmd("colorscheme catppuccin-macchiato") -- Use the default colorscheme (you can change this later)

-- Jump to the last position when reopening a file
vim.cmd([[autocmd BufReadPost * normal! g'"]])

--
-- My Awesome Autocomplete
--

local function ___auto_complete_support__() end

local local_auto_complete_keys = {
  'a', 'e', 'i', 'o', 'u',
  'A', 'E', 'I', 'O', 'U',
  's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
  'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '_',
}

local function enable_auto_complete()
  for _, key in ipairs(local_auto_complete_keys) do
    vim.api.nvim_set_keymap('i', key, key .. '<C-n><C-p>', { noremap = true, silent = true })
  end
end

local function disable_auto_complete()
  for _, key in ipairs(local_auto_complete_keys) do
    vim.api.nvim_del_keymap('i', key)
  end
end

-- Enable auto-completion globally
enable_auto_complete()

-- Improve <Tab> key behavior in auto-completion
vim.api.nvim_set_keymap('i', '<Tab>', 'pumvisible() ? "<C-n>" : "<Tab>"', { noremap = true, expr = true, silent = true })

-- Automatically disable auto-completion in Telescope windows
vim.api.nvim_create_autocmd("FileType", {
  pattern = "TelescopePrompt",
  callback = function() disable_auto_complete() end,
})

-- Re-enable auto-completion for other windows when leaving Telescope
vim.api.nvim_create_autocmd("BufLeave", {
  pattern = "*",
  callback = function()
    if vim.bo.filetype == "TelescopePrompt" then
      enable_auto_complete()
    end
  end,
})

--
-- Plugins
--
local function ___plug_in_configs___() end

-- Telescope Settings
local telescope = require("telescope")
telescope.setup({
    defaults = {
        prompt_prefix = "? ",
        border = true,
        layout_config = {
            width = 0.84,
            height = 0.84,
            preview_width = 0.618,
        },
    },
})

-- IDE (tagbar & nvim-tree) setup

require("nvim-tree").setup({
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})

-- which-key
require("which-key").setup({})

-- TSModuleInfo
require("nvim-treesitter.configs").setup({
    ensure_installed = {
        "commonlisp", -- emacs
        "cpp",
        "lua",
        "python",
        "starlark",   -- support tensorflow BUILD file (Bazel build)
    },
    highlight = { enable = true }, -- Enable syntax highlighting
    indent = { enable = true },    -- Enables Tree-sitter's indentation logic
})

-- lualine
require("lualine").setup({
    options = {
        theme = "onedark",       -- Choose a theme (e.g., gruvbox, onedark, dracula)
        section_separators = "", -- Remove separators for a cleaner look
        component_separators = "",
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff" },
        lualine_c = { { "filename", path = 1 }, "diagnostics" },
        lualine_x = { "diagnostics", "encoding", "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" },
    },
})

-- Neogit
local neogit = require("neogit")

neogit.setup {
    -- Hides the hints at the top of the status buffer
    disable_hint = false,
}

--
-- Keys (Leader Key and Function Keys)
--
local function ___keys_leader_Fn_remap__() end

-- Awesome remap keys
-- vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv") -- move selected region down
-- vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv") -- move selected region up

vim.keymap.set("n", "J", "mzJ`z") -- better J behaviour

-- better window movements
vim.keymap.set({"n","i"}, "<C-h>", "<C-w><C-h>", { noremap = true, silent = true })  -- emacs style
vim.keymap.set({"n","i"}, "<C-j>", "<C-w><C-j>", { noremap = true, silent = true })  -- emacs style
vim.keymap.set({"n","i"}, "<C-k>", "<C-w><C-k>", { noremap = true, silent = true })  -- emacs style
vim.keymap.set({"n","i"}, "<C-l>", "<C-w><C-l>", { noremap = true, silent = true })  -- emacs style

-- Leader Keys
-- Note : TODO: space as leader key makes typing <space> sluggish
vim.g.mapleader = " "

vim.keymap.set("n", "<leader><leader>", ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>/",       ":Telescope current_buffer_fuzzy_find<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>?",       ":Telescope keymaps<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>*",       ":Telescope grep_string<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ff",      ":Telescope find_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fp",      ":Telescope find_files cwd=~/workspace/dotfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fr",      ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader><CR>",    ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>\\",      ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gf",      ":Telescope git_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gg",      ":Neogit kind=split<CR><C-w>L", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bs",      ":vs<CR>:enew<CR>", { noremap = true, silent = true, desc = "Open scratch buffer" })
vim.keymap.set({"n","i"}, "<leader>cc", ":make -f nvim.makefile<CR>:copen<CR>", { noremap = true, silent = true })

vim.keymap.set("n", "<leader>sj",      ":Telescope jumplist<CR>", { noremap = true, silent = true })

-- Other Useful Keys Based On Telescope
vim.keymap.set("n", "z=",          ":Telescope spell_suggest<CR>", { noremap = true, silent = true })

vim.keymap.set({"n","i"}, "<C-g>", "<ESC><ESC>", { noremap = true, silent = true })  -- emacs style

-- Function Keys
vim.keymap.set({"n","i"}, "<f3>", "<ESC>:NvimTreeToggle<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f4>", "<ESC>:q<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f6>", "<ESC>:Telescope registers<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f7>", "<ESC>:make<CR>:copen<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f8>", "<ESC>:Vista!!<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f9>", "<ESC>:Telescope fd<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f10>", "<ESC>:Telescope<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f11>", "<ESC>:on<CR>:Vista!!<CR>:NvimTreeToggle<CR><C-w><C-w>", { noremap = true, silent = true })
-- <f5> and <f12>: used by tmux
