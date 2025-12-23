-- load vimrc first
-- then apply init.lua on top
vim.cmd('source ~/.vimrc')

-- Basic Settings
-- NOTE: this is on top of basic settings in vimrc
local function ___basic_settings__() end

vim.g.loaded_netrw = 1         -- Required for neo-tree
vim.g.loaded_netrwPlugin = 1   -- Required for neo-tree
vim.opt.clipboard = "unnamedplus" -- System clipboard for MacOS
vim.opt.tags = ""
vim.opt.guicursor = ""
vim.opt.timeoutlen = 100       -- Makes leader key more responsive in INSERT mode
vim.o.makeprg = "just"         -- invoke 'just' when typing :make<CR>

--
-- Plugin Management with lazy.nvim
--
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

local function ___lazy_plugin_management__() end

require("lazy").setup({
    "tpope/vim-commentary",            -- commenting plugin
    "nvim-lualine/lualine.nvim",       -- nice status line

    -- Fuzzy finder
    {
      "nvim-telescope/telescope.nvim",   -- telescope fuzzy finder
      version = 'v0.1.9',                -- for nvim 0.9
    },
    "nvim-lua/plenary.nvim",           -- dependency for telescope

    -- justfile support
    'NoahTheDuke/vim-just',

    -- IDE
    "nvim-tree/nvim-tree.lua",         -- nerdtree for neovim
    "preservim/tagbar",                -- tagbar suits my style better (than vista.vim or other ones)
    "nvim-tree/nvim-web-devicons",     -- needed by IDE Plugins

    -- Autocomplete
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
            -- "hrsh7th/cmp-cmdline" -- not enabled as it can be overly distracting
        },
    },

    -- Git
    "lewis6991/gitsigns.nvim",         -- Git Gutter
    "tpope/vim-fugitive",              -- Gread, Gwrite
    {
        "NeogitOrg/neogit",
        version = 'v0.0.1',            -- for nvim 0.9
        dependencies = {
            "sindrets/diffview.nvim",  -- optional - Diff integration
        },
        config = true
    },

    -- Which-key
    {
        "folke/which-key.nvim",
        preset = "helix", -- "modern" can be too distracting
        event = "VeryLazy",
        opts = {
            delay = 500, -- ms (?)
        },
    },

    -- themes --
    "catppuccin/nvim",
    "folke/tokyonight.nvim",

    -- LSP & treesitter support
    -- NOTE: don't have to enable LSP, just keep nvim light weight & simple
    -- NOTE: also avoid treesitter for now: prioritising lightweight, cross-platform, and speed
    -- "nvim-treesitter/nvim-treesitter", -- main plugin for tree-sitter
    -- "nvim-treesitter/nvim-treesitter-context",
})

-- Colorscheme
vim.opt.termguicolors = true
vim.cmd("colorscheme catppuccin-mocha") -- Good ones: catppuccin-mocha, tokyonight-night
require("catppuccin").setup({})

--
-- Autocomplete
--
local function ___auto_complete_support__() end

local cmp = require("cmp")
cmp.setup({
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item.

        -- Improve Tab behavior in autocomplete
        ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            else
                fallback()
            end
        end, { "i", "s" }),

        ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            else
                fallback()
            end
        end, { "i", "s" }),
    }),
    -- only following basic sources are enabled
    sources = cmp.config.sources({
        { name = 'buffer' },
        { name = 'path' },
    })
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
        mappings = {
            i = {
                -- make <C-a> and <C-k> mappings more like emacs in Telescope search
                ["<C-a>"] = function()
                    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Home>", true, false, true), "n", true)
                end,
                ["<C-k>"] = function()
                    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<ESC>lDD", true, false, true), "n", true)
                end,
            },
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

-- Git (Neogit, Gitsigns)
require('gitsigns').setup()
-- Need to have this <ESC> map to make sure preview_hunk is closed by ESC very quickly
vim.keymap.set("n", "<ESC>", "<ESC>jk<ESC>", { noremap = true, silent = true })

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

--
-- Auto commands
--

-- justfile support
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
    pattern = "justfile",
    callback = function()
        -- invoke 'just' when typing :make
        vim.bo.makeprg = "just"
    end,
})

--
-- Keys (Leader Key and Function Keys)
--
local function ___keys_leader_Fn_remap__() end

-- Efficient quickfix navigation; make sure to it with Telescope and <C-q>
vim.keymap.set("n", "<C-n>", ":cnext<CR>")
vim.keymap.set("n", "<C-p>", ":cprev<CR>")

-- some emacs key bindings that work well for me
vim.keymap.set({"n","i"}, "<C-g>", "<ESC><ESC>", { noremap = true, silent = true })  -- emacs style
-- emacs's <C-a> and <C-k> in :ex mode and / search
vim.cmd("cmap <C-a> <Home>")
vim.cmd("cmap <C-k> <C-\\>e strpart(getcmdline(), 0, getcmdpos() - 1)<CR>")

-- Gitsigns
vim.keymap.set("n", "gh", "<ESC>:Gitsigns next_hunk<CR><ESC>:Gitsigns preview_hunk<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "gs", "<ESC>:Gitsigns stage_hunk<CR>", { noremap = true, silent = true })

--
-- My Commands
--

local function ___My_Commands__() end

vim.api.nvim_create_user_command('Glog', function() vim.cmd("Telescope git_commits") end, { desc = "git log" })
vim.api.nvim_create_user_command('Gcommit', function() vim.cmd('!git commit -m "update %"') end, { desc = "Git commit current file" })
vim.api.nvim_create_user_command('Gblame', function() vim.cmd('Git blame') end, { desc = "Git blame" })
vim.api.nvim_create_user_command('Gshow', function() vim.cmd('Git show') end, { desc = "Git show" })
vim.api.nvim_create_user_command('Gdiff', function()
    vim.cmd('Git diff')
    vim.cmd('wincmd L')
end, { desc = "Git diff" })

-- Leader Keys
-- Note : NOTE: space as leader key makes typing <space> sluggish in nvim
vim.g.mapleader = " "

-- Telescope related leader keys (NOTE: this is different from vim)
vim.keymap.set("n", "<leader><leader>", ":Telescope buffers<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>/",        ":Telescope current_buffer_fuzzy_find<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>?",        ":Telescope keymaps<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>*",        ":Telescope grep_string<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ff",       ":Telescope find_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fp",       ":Telescope find_files cwd=~/workspace/dotfiles<CR>", { noremap = true, silent = true, desc = "find file in private config" })
vim.keymap.set("n", "<leader>fr",       ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>\\",       ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader><CR>",     ":Telescope oldfiles<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>sj",       ":Telescope jumplist<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "z=",               ":Telescope spell_suggest<CR>", { noremap = true, silent = true })

-- git related leader keys
vim.keymap.set("n", "<leader>gf",      ":Telescope git_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gg",      "<ESC>:Neogit kind=vsplit<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gh",      "<ESC>:Gitsigns preview_hunk<CR>", { noremap = true, silent = true })

-- Misc leader keys
vim.keymap.set({"n","i"}, "<leader>cc", ":make<CR>:copen<CR>", { noremap = true, silent = true })

-- Function Keys
vim.keymap.set({"n","i"}, "<f3>", "<ESC>:NvimTreeFindFileToggle<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f6>", "<ESC>:Telescope registers<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f7>", "<ESC>:make<CR>:copen<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f8>", "<ESC>:TagbarToggle<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f9>", "<ESC>:Telescope fd<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f10>", "<ESC>:Telescope<CR>", { noremap = true, silent = true })
-- <f5> and <f12>: used by tmux
