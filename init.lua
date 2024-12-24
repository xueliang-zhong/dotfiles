-- Basic Settings
vim.opt.number = true          -- Show line numbers
vim.opt.relativenumber = false -- Relative line numbers
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
    "majutsushi/tagbar",             -- Tagbar
    "neovim/nvim-lspconfig",         -- LSP support
    "nvim-lua/plenary.nvim",         -- Dependency for telescope
    "nvim-telescope/telescope.nvim", -- Fuzzy finder
    "nvim-tree/nvim-tree.lua",       -- nerdtree for neovim
    "tpope/vim-commentary",          -- Commenting plugin
    "tpope/vim-fugitive",            -- Git integration
    "nvim-lualine/lualine.nvim",     -- Nice status line
    "nvim-treesitter/nvim-treesitter", -- Main plugin for Tree-sitter

    -- themes --
    "catppuccin/nvim"
})

-- Colorscheme
require("catppuccin").setup({})
vim.opt.termguicolors = true
vim.cmd("colorscheme catppuccin-mocha") -- Use the default colorscheme (you can change this later)

-- Jump to the last position when reopening a file
vim.cmd([[autocmd BufReadPost * normal! g'"]])

--
-- Autocomplete
--
local local_auto_complete_keys = {
    'a', 'e', 'i', 'o', 'u',
    'A', 'E', 'I', 'O', 'U',
    's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
    'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '_',
}

-- Function to enable mappings
local function enable_auto_complete()
    for _, key in ipairs(local_auto_complete_keys) do
        vim.api.nvim_set_keymap('i', key, key .. '<C-n><C-p>', { noremap = true, silent = true })
    end
end

-- Function to disable mappings
local function disable_auto_complete()
    for _, key in ipairs(local_auto_complete_keys) do
        vim.api.nvim_del_keymap('i', key)
    end
end

-- Enable auto-completion globally
enable_auto_complete()

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

-- Improve <Tab> key behavior in auto-completion
vim.api.nvim_set_keymap('i', '<Tab>', 'pumvisible() ? "<C-n>" : "<Tab>"', { noremap = true, expr = true, silent = true })

-- Telescope Settings
local telescope = require("telescope")
telescope.setup({
    defaults = {
        prompt_prefix = "? ",
        layout_config = {
            horizontal = { preview_width = 0.618 },
        },
    },
})

-- nvim-tree setup using defaults
require("nvim-tree").setup()

-- treesitter
-- TSModuleInfo
require("nvim-treesitter.configs").setup({
    ensure_installed = { "lua", "python", "cpp", "commonlisp" }, -- Languages
    highlight = {
        enable = true, -- Enable syntax highlighting
    },
    indent = {
        enable = true, -- Enables Tree-sitter's indentation logic
    },
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

-- Leader Key
vim.keymap.set("n", "<Space>", "", { noremap = true, silent = true }) -- Space as leader key
vim.g.mapleader = " " -- Leader key (try to stay the same as emacs)
vim.keymap.set("n", "<leader><Space>", ":Telescope buffers<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>/",       ":Telescope current_buffer_fuzzy_find<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>?",       ":Telescope keymaps<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>*",       ":Telescope grep_string<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ff",      ":Telescope find_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader><CR>",    ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gg",      ":Telescope git_status<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bs",      ":vs<CR>:enew<CR>", { noremap = true, silent = true, desc = "Open scratch buffer" })
vim.keymap.set("n", "<leader>sj",      ":Telescope jumplist<CR>", { noremap = true, silent = true })

-- Telescope Keys
vim.keymap.set("n", "z=",              ":Telescope spell_suggest<CR>", { noremap = true, silent = true })

-- Function Keys
vim.keymap.set({"n","i"}, "<f3>", "<ESC>:NvimTreeToggle<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f4>", "<ESC>:q<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f6>", "<ESC>:Telescope registers<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f8>", "<ESC>:TagbarToggle<CR>", { noremap = true, silent = true })
-- <f7> : my make function
vim.keymap.set({"n","i"}, "<f9>", "<ESC>:Telescope fd<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f10>", "<ESC>:Telescope<CR>", { noremap = true, silent = true })
