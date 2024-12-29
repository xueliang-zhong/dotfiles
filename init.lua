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
    "nvim-telescope/telescope.nvim",   -- telescope fuzzy finder
    "nvim-lua/plenary.nvim",           -- dependency for telescope, neogit

    -- justfile support
    'NoahTheDuke/vim-just',

    -- IDE
    "nvim-tree/nvim-tree.lua",         -- nerdtree for neovim
    "preservim/tagbar",                -- tagbar suits my style better (than vista.vim or other ones)
    "nvim-tree/nvim-web-devicons",     -- needed by IDE Plugins

    -- avoid treesitter for now: prioritising lightweight, cross-platform, and speed
    -- "nvim-treesitter/nvim-treesitter", -- main plugin for tree-sitter
    -- "nvim-treesitter/nvim-treesitter-context",

    -- LSP support
    -- NOTE: don't have to enable it, just keep nvim light weight & simple

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
})

-- Colorscheme
vim.opt.termguicolors = true
-- To quickly select theme:
-- https://vimcolorschemes.com/i/trending
-- map <F2> :Telescope colorscheme<CR>
vim.cmd("colorscheme catppuccin-mocha") -- Good ones: catppuccin-mocha, tokyonight-night
require("catppuccin").setup({})

-- Jump to the last position when reopening a file
vim.cmd([[autocmd BufReadPost * normal! g'"]])

--
-- My Awesome Autocomplete
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
local neogit = require("neogit")

neogit.setup ({
    -- Hides the hints at the top of the status buffer
    disable_hint = false,
    popup = {
        -- Make sure hjkl keys are only for vim style moving
        ["l"] = "false",
    },
})

-- Make sure hjkl keys are only for vim style moving NeogitStatus
vim.api.nvim_create_autocmd({"TextChanged", "CursorMoved"}, {
    pattern = "NeogitStatus",
    callback = function()
        vim.api.nvim_buf_set_keymap(0, "n", "l", "<Right>", { noremap = true, silent = true })
    end,
})

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
-- autocommands
--

-- high tail whitespaces
vim.cmd [[highlight ExtraWhitespace ctermbg=red guibg=red]]

-- Match trailing whitespace
vim.api.nvim_create_autocmd({"BufWinEnter", "InsertLeave"}, {
    pattern = "*",
    command = "match ExtraWhitespace /\\s\\+$/"
})
vim.api.nvim_create_autocmd("InsertEnter", {
    pattern = "*",
    command = "match none"
})

-- justfile support
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
    pattern = "justfile",
    callback = function()
        -- invoke 'just' when typing :make
        vim.bo.makeprg = "just"
    end,
})

-- Define the function to generate and load tags
local function generate_and_load_tags()
    local original_dir = vim.fn.getcwd()

    -- Try to get the Git root directory
    local git_root = vim.fn.system("git rev-parse --show-toplevel")
    local use_current_dir = false

    -- Check if the command succeeded
    if vim.v.shell_error ~= 0 then
        use_current_dir = true
        git_root = original_dir -- Use the current directory if not in a Git repo
    else
        git_root = vim.fn.trim(git_root) -- Trim trailing newline from the result
    end

    -- change to the determined directory & generate the tags
    local ctags_command = "ctags -R --exclude=.git --exclude=build --exclude=python."
    vim.fn.chdir(git_root)
    vim.fn.system(ctags_command)

    if vim.v.shell_error ~= 0 then
        print("Error: Failed to generate tags!")
        vim.fn.chdir(original_dir)
        return
    end

    vim.opt.tags:append(git_root .. "/tags")
    vim.fn.chdir(original_dir)

    -- Inform the user about the directory used
    if use_current_dir then
        print("Tags generated and loaded from the current directory: " .. git_root)
    else
        print("Tags generated and loaded from the Git root: " .. git_root)
    end
end

--
-- Keys (Leader Key and Function Keys)
--
local function ___keys_leader_Fn_remap__() end

-- Awesome remap keys
-- Move selected region up or down, works great with 'vip'
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z") -- better J behaviour

vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Efficient quickfix navigation; make sure to it with Telescope and <C-q>
vim.keymap.set("n", "<C-n>", ":cnext<CR>")
vim.keymap.set("n", "<C-p>", ":cprev<CR>")

-- some emacs key bindings that work well for me
vim.keymap.set({"n", "i"}, "<C-x><C-o>", function()
  local current_line = vim.api.nvim_get_current_line()
  while current_line:match("^%s*$") do
    vim.api.nvim_command("normal! dd")
    current_line = vim.api.nvim_get_current_line()
  end
end, { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<C-g>", "<ESC><ESC>", { noremap = true, silent = true })  -- emacs style

-- emacs's <C-a> and <C-k> in :ex mode and / search
vim.cmd("cmap <C-a> <Home>")
vim.cmd("cmap <C-k> <C-\\>e strpart(getcmdline(), 0, getcmdpos() - 1)<CR>")

-- better window movements
vim.keymap.set({"n","i"}, "<C-h>", "<C-w><C-h>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<C-j>", "<C-w><C-j>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<C-k>", "<C-w><C-k>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<C-l>", "<C-w><C-l>", { noremap = true, silent = true })

-- Gitsigns
vim.keymap.set("n", "gh", "<ESC>:Gitsigns preview_hunk<CR>", { noremap = true, silent = true })

--
-- My Commands
--

local function ___My_Commands__() end

-- justfile to Makefile format
local function justfile_to_makefile()
    vim.cmd("%s/^[ ]\\+/\\t/g")   -- Replace leading spaces with tabs
    vim.cmd("%s/[ ]*:=[ ]*/=/g")  -- Change variable definitions to Makefile style
    vim.cmd("%s/{{/$(/g")         -- Replace variable opening braces with $(
    vim.cmd("%s/}}/)/g")          -- Replace variable closing braces with )
    vim.cmd("w! Makefile")
end

vim.api.nvim_create_user_command(
    'JustfileToMakefileFormat',
    justfile_to_makefile,
    { desc = "Convert Justfile syntax to Makefile syntax" })

-- SmallWindow Function
vim.api.nvim_create_user_command(
    'SmallWindow',
    function()
        vim.cmd("set nonu")
        vim.cmd("set nocursorline")
        -- Hide Gitsigns
        require('gitsigns').toggle_signs(false)     -- Disable signs in the gutter
        require('gitsigns').toggle_linehl(false)    -- Disable line highlights
        require('gitsigns').toggle_numhl(false)     -- Disable number column highlights
        require('gitsigns').toggle_word_diff(false) -- Disable word diffs
    end, { desc = "make nvim suiable for small window or tmux copy" })

vim.api.nvim_create_user_command(
    'BigWindow',
    function()
        vim.cmd("set nu")
        vim.cmd("set cursorline")
        -- Show Gitsigns
        require('gitsigns').toggle_signs(true)     -- Enable signs in the gutter
        require('gitsigns').toggle_linehl(true)    -- Enable line highlights
        require('gitsigns').toggle_numhl(true)     -- Enable number column highlights
        require('gitsigns').toggle_word_diff(true) -- Enable word diffs
    end, { desc = "make nvim suiable for big window and development" })

-- Leader Keys
-- Note : TODO: space as leader key makes typing <space> sluggish
vim.g.mapleader = " "

-- Telescope related leader kesy
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
vim.keymap.set("n", "<leader>gg",      "<ESC>:on<CR><ESC>:Neogit kind=vsplit<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gh",      "<ESC>:Gitsigns preview_hunk<CR>", { noremap = true, silent = true })

-- Misc leader keys
vim.keymap.set("n", "<leader>bs",       ":vs<CR>:enew<CR>", { noremap = true, silent = true, desc = "Open scratch buffer" })
vim.keymap.set({"n","i"}, "<leader>cc", ":make<CR>:copen<CR>", { noremap = true, silent = true })

vim.keymap.set("n", "<leader>tt", function()
    print ("Re-creating a new tags file now ...")
    generate_and_load_tags()
end, { noremap = true, silent = true, desc = "Regnerate a new tags file" })

vim.keymap.set("n", "<leader>]", function()
    local function load_tags_from_dir()
        local git_root = vim.fn.system("git rev-parse --show-toplevel")

        if vim.v.shell_error ~= 0 then
            git_root = vim.fn.getcwd() -- Use the current directory if not in a Git repo
        else
            git_root = vim.fn.trim(git_root) -- Trim trailing newline
        end

        if vim.fn.filereadable(git_root .. "/tags") == 0 then
            print("Tags file not found in " .. git_root .. ", creating a new one now...")
            generate_and_load_tags()
        end

        vim.opt.tags:append(git_root .. "/tags")
        return git_root
    end

    -- Load tags from the appropriate directory
    local tags_dir = load_tags_from_dir()

    -- Get the word under the cursor
    local cword = vim.fn.expand("<cword>")

    -- Use Telescope to preview tags
    require('telescope.builtin').tags({ default_text = cword })
end, { noremap = true, silent = true, desc = "ctags preview current <cword>" })

-- Function Keys
vim.keymap.set({"n","i"}, "<f3>", "<ESC>:NvimTreeFindFileToggle<CR>", { noremap = true, silent = true })
-- NOTE: :fc<CR> to close floating window (not supported in older nvim)
vim.keymap.set({"n","i"}, "<f4>", "<ESC>:q<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f6>", "<ESC>:Telescope registers<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f7>", "<ESC>:make<CR>:copen<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f8>", "<ESC>:TagbarToggle<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f9>", "<ESC>:Telescope fd<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f10>", "<ESC>:Telescope<CR>", { noremap = true, silent = true })

vim.keymap.set({"n", "i"}, "<F11>", function()
    local original_win = vim.fn.win_getid()  -- Remember the current window ID
    vim.cmd('stopinsert')           -- Ensure exiting insert mode if in it
    vim.cmd('only')                 -- Close all other windows
    vim.cmd('TagbarOpen')           -- Tagbar works best here
    vim.cmd('NvimTreeFindFile')     -- Toggle NvimTree
    vim.fn.win_gotoid(original_win) -- Doesn't work all the time, but good enough
end, { noremap = true, silent = true })

-- <f5> and <f12>: used by tmux
