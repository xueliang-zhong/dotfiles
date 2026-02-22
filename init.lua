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
vim.opt.scrolloff = 0
vim.opt.sidescrolloff = 5
vim.opt.softtabstop = 2
vim.opt.updatetime = 300       -- Faster diagnostics/git signs refresh cadence
vim.opt.signcolumn = "yes"     -- Prevent text jitter when signs appear/disappear
vim.opt.inccommand = "split"   -- Preview substitutions incrementally
vim.opt.splitkeep = "screen"   -- Keep viewport stable when splitting/resizing
vim.opt.shada = "!,'100,<50,s10,h" -- Better command/search/register persistence
vim.opt.sessionoptions:append({ "globals", "localoptions", "tabpages" })

vim.opt.undofile = true
local undo_dir = vim.fn.stdpath("state") .. "/undo"
vim.fn.mkdir(undo_dir, "p")
vim.opt.undodir = undo_dir

--
-- Plugin Management with lazy.nvim
--
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
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
    { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} }, -- Indentation guides

    -- Seamless navigation between nvim splits and tmux panes
    {
        "christoomey/vim-tmux-navigator",
        lazy = false,
        config = function()
            -- Disable default mappings (they use C-h/j/k/l)
            vim.g.tmux_navigator_no_mappings = 1
            -- Auto-save modified buffers when crossing vim/tmux panes
            vim.g.tmux_navigator_save_on_switch = 2
            -- Set up C-w prefixed mappings for directional navigation
            vim.keymap.set('n', '<C-w>h', ':TmuxNavigateLeft<cr>', { silent = true })
            vim.keymap.set('n', '<C-w>j', ':TmuxNavigateDown<cr>', { silent = true })
            vim.keymap.set('n', '<C-w>k', ':TmuxNavigateUp<cr>', { silent = true })
            vim.keymap.set('n', '<C-w>l', ':TmuxNavigateRight<cr>', { silent = true })
            -- Also support C-w C-h/j/k/l for easier key pressing
            vim.keymap.set('n', '<C-w><C-h>', ':TmuxNavigateLeft<cr>', { silent = true })
            vim.keymap.set('n', '<C-w><C-j>', ':TmuxNavigateDown<cr>', { silent = true })
            vim.keymap.set('n', '<C-w><C-k>', ':TmuxNavigateUp<cr>', { silent = true })
            vim.keymap.set('n', '<C-w><C-l>', ':TmuxNavigateRight<cr>', { silent = true })
            -- C-w C-w cycles through windows and can navigate to tmux
            vim.keymap.set('n', '<C-w><C-w>', function()
                local wins = vim.api.nvim_tabpage_list_wins(0)
                if #wins == 1 then
                    -- Only one window, try to go to tmux
                    vim.cmd('TmuxNavigatePrevious')
                else
                    -- Cycle to next window
                    vim.cmd('wincmd w')
                end
            end, { silent = true })
        end,
    },

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
    "tpope/vim-fugitive",              -- Gread
    {
        "NeogitOrg/neogit",
        version = 'v0.0.1',            -- for nvim 0.9
        dependencies = {
            "sindrets/diffview.nvim",  -- optional - Diff integration
        },
        config = function()
            require("neogit").setup({
                auto_refresh = true,
                filewatcher = {
                    enabled = true,
                    interval = 1000,
                },
                mappings = {
                    popup = {
                        ["l"] = false,  -- Avoid default LogPopup on 'l'
                    },
                    status = {
                        ["h"] = false,
                        ["l"] = false,
                    },
                },
            })
            -- Simple left/right movement like arrow keys
            vim.api.nvim_create_autocmd("FileType", {
                pattern = "NeogitStatus",
                callback = function()
                    vim.keymap.set("n", "h", "h", { buffer = true, remap = false, silent = true })
                    vim.keymap.set("n", "l", "l", { buffer = true, remap = false, silent = true })
                end,
            })
        end,
    },

    -- Which-key
    {
        "folke/which-key.nvim",
        preset = "helix", -- "modern" can be too distracting
        event = "VeryLazy",
        opts = {
            delay = 1000, -- increased to reduce typing interference
            filter = function(mapping)
                return mapping.desc and mapping.desc ~= ""
            end,
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
require("catppuccin").setup({})
vim.cmd("colorscheme catppuccin-mocha") -- Good ones: catppuccin-mocha, tokyonight-night

--
-- Autocomplete
--
local function ___auto_complete_support__() end

local cmp = require("cmp")
cmp.setup({
    performance = {
        debounce = 60,
        throttle = 30,
        max_view_entries = 7,
    },
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
                -- Allow space in search (like fzf/ivy)
                ["<space>"] = function(prompt_bufnr)
                    local action_state = require("telescope.actions.state")
                    local current_picker = action_state.get_current_picker(prompt_bufnr)
                    local text = current_picker:_get_prompt() .. " "
                    current_picker:set_prompt(text)
                end,
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
    pickers = {
        oldfiles = {
            path_display = function(_, path)
                local filename = vim.fn.fnamemodify(path, ":t")
                local parent = vim.fn.fnamemodify(path, ":~:h")
                if parent == "." then
                    return filename
                end
                return string.format("%s  (%s)", filename, vim.fn.pathshorten(parent))
            end,
        },
    },
})

-- Load fzf extension for better fzf-like behavior (optional, requires telescope-fzf-native.nvim)
-- telescope.load_extension('fzf')

-- IDE (tagbar & nvim-tree) setup
require("nvim-tree").setup({
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})
-- Tagbar: use org-style fold toggle key
vim.g.tagbar_map_togglefold = { "<Tab>", "o", "za" }

-- Git (Neogit, Gitsigns)
require('gitsigns').setup()
-- Clear search highlight and close any floating windows on ESC
vim.keymap.set("n", "<ESC>", "<ESC>:noh<CR>", { noremap = true, silent = true })

-- lualine
require("lualine").setup({
    options = {
        theme = "catppuccin",    -- Match the main colorscheme
        section_separators = "", -- Remove separators for a cleaner look
        component_separators = "",
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff" },
        lualine_c = { { "filename", path = 1 }, "diagnostics" },
        lualine_x = { "encoding", "fileformat", "filetype" },
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

-- Keep buffer content in sync when files change outside Neovim
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
    callback = function()
        if vim.fn.mode() ~= "c" then
            vim.cmd("checktime")
        end
    end,
})

-- Highlight trailing whitespace
vim.api.nvim_create_autocmd("BufWinEnter", {
    callback = function()
        vim.fn.matchadd("TrailingWhitespace", "\\s\\+$", 10)
    end,
})
vim.api.nvim_set_hl(0, "TrailingWhitespace", { bg = "#f7768e" })

-- Clean trailing whitespace on save
vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function()
        local save_cursor = vim.fn.getpos(".")
        vim.cmd([[%s/\s\+$//e]])
        vim.fn.setpos(".", save_cursor)
    end,
})

-- Flash yanked text for visual feedback
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
        vim.highlight.on_yank({ higroup = "IncSearch", timeout = 150 })
    end,
})

-- Auto-save with debounce (only for normal files, not special buffers)
vim.api.nvim_create_autocmd({ "InsertLeave", "TextChanged" }, {
    callback = function()
        if vim.bo.buftype == "" and vim.bo.modifiable and vim.bo.modified then
            vim.defer_fn(function()
                if vim.bo.modified then
                    vim.cmd("silent! write")
                end
            end, 1000) -- 1 second debounce
        end
    end,
})

-- Create missing parent directories automatically on save
vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function(args)
        if args.file == "" then
            return
        end
        local parent = vim.fn.fnamemodify(args.file, ":p:h")
        if vim.fn.isdirectory(parent) == 0 then
            vim.fn.mkdir(parent, "p")
        end
    end,
})

-- Open quickfix/location list only when there are entries
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
    pattern = "[^l]*",
    callback = function()
        local qf = vim.fn.getqflist({ size = 0 })
        if qf.size > 0 then
            vim.cmd("copen")
        else
            vim.cmd("cclose")
        end
    end,
})

vim.api.nvim_create_autocmd("QuickFixCmdPost", {
    pattern = "l*",
    callback = function()
        local loc = vim.fn.getloclist(0, { size = 0 })
        if loc.size > 0 then
            vim.cmd("lwindow")
        else
            vim.cmd("lclose")
        end
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
vim.keymap.set("i", "<C-v>", "<C-r>+", { noremap = true, silent = true })
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

local function xueliang_stop_insert_if_needed()
    local mode = vim.api.nvim_get_mode().mode
    if mode:match("^[it]") then
        vim.cmd("stopinsert")
    end
end

local function xueliang_open_scratch_buffer_window()
    vim.cmd("rightbelow vsplit")
    vim.cmd("enew")
    vim.bo.buftype = "nofile"
    vim.bo.bufhidden = "wipe"
    vim.bo.swapfile = false
end

local function xueliang_toggle_readonly()
    vim.cmd("setlocal readonly!")
end

local function xueliang_close_window()
    xueliang_stop_insert_if_needed()
    if vim.fn.winnr("$") > 1 then
        vim.cmd("close")
    else
        vim.cmd("bdelete")
    end
end

local function xueliang_kill_buffer_and_window()
    xueliang_stop_insert_if_needed()
    vim.cmd("bdelete")
    if vim.fn.winnr("$") > 1 then
        pcall(vim.cmd, "close")
    end
end

local function xueliang_terminal_popup()
    xueliang_stop_insert_if_needed()
    vim.cmd("botright split")
    vim.cmd("resize 12")
    vim.cmd("terminal")
    vim.cmd("startinsert")
end

local function xueliang_imenu_or_org_today()
    local ext = vim.fn.expand("%:e")
    if ext == "org" or ext == "md" then
        local date = os.date("%Y-%m-%d")
        local pattern = ext == "org" and ("<" .. date) or date
        local found = vim.fn.search("\\V" .. pattern, "w")
        if found == 0 then
            vim.notify("No entry found for " .. date)
        else
            vim.cmd("normal! zz")
            vim.cmd("nohlsearch")
        end
        return
    end
    vim.cmd("TagbarToggle")
end

local function xueliang_dired_sidebar()
    -- Toggle nvim-tree sidebar
    vim.cmd("NvimTreeToggle")
end

local function xueliang_telescope_counsel()
    vim.cmd("Telescope builtin")
end

vim.api.nvim_create_user_command('Glog', function() vim.cmd("Telescope git_commits") end, { desc = "git log" })
vim.api.nvim_create_user_command('Gcommit', function() vim.cmd('!git commit -m "update %:t"') end, { desc = "Git commit current file" })
vim.api.nvim_create_user_command('Gblame', function() vim.cmd('Git blame') end, { desc = "Git blame" })
vim.api.nvim_create_user_command('Gshow', function() vim.cmd('Git show') end, { desc = "Git show" })
vim.api.nvim_create_user_command('Gdiff', function()
    vim.cmd('Git diff')
    vim.cmd('wincmd L')
end, { desc = "Git diff" })

vim.api.nvim_create_user_command('Gwrite', function()
    -- Override vim-fugitive's Gwrite to avoid index.lock race condition
    -- See: https://github.com/tpope/vim-fugitive/issues/1624
    vim.cmd('silent !git add %')
    -- Clear the "Press ENTER" prompt
    vim.cmd('redraw!')
end, { desc = "Git add current file" })

-- Leader Keys
vim.g.mapleader = " "
vim.g.maplocalleader = " "

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
vim.keymap.set("n", "<leader>si",       xueliang_imenu_or_org_today, { noremap = true, silent = true, desc = "symbols or today's note" })
vim.keymap.set("n", "<leader>x",        ":Telescope commands<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "z=",               ":Telescope spell_suggest<CR>", { noremap = true, silent = true })

-- git related leader keys
vim.keymap.set("n", "<leader>gf",      ":Telescope git_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gg",      "<ESC>:Neogit kind=vsplit<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gh",      "<ESC>:Gitsigns preview_hunk<CR>", { noremap = true, silent = true })

-- Misc leader keys
-- NOTE: Only map in normal mode to prevent space key delay in insert mode
vim.keymap.set("n", "<leader>bs", xueliang_open_scratch_buffer_window, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bw", xueliang_toggle_readonly, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>cc", ":make<CR>:copen<CR>", { noremap = true, silent = true, desc = "Run make and open quickfix if needed" })
vim.keymap.set("n", "<leader>wc", xueliang_close_window, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>wg", "<C-w>=", { noremap = true, silent = true })

-- Which-key registration
local wk = require("which-key")
wk.add({
  { "<leader>b", group = "Buffer" },
  { "<leader>f", group = "Find" },
  { "<leader>g", group = "Git" },
  { "<leader>s", group = "Search" },
  { "<leader>w", group = "Window" },
})

-- Open SVG mindmap files from mindmap folder
local function xueliang_open_external_file(filepath)
    -- Prefer Neovim's native opener when available (cross-platform).
    if vim.ui and type(vim.ui.open) == "function" then
        local ok = pcall(vim.ui.open, filepath)
        if ok then
            return
        end
    end

    -- Fallback for older Neovim versions without vim.ui.open.
    local cmd = nil
    if vim.fn.has("macunix") == 1 then
        cmd = { "open", filepath }
    elseif vim.fn.has("win32") == 1 then
        cmd = { "cmd.exe", "/c", "start", "", filepath }
    elseif vim.fn.executable("xdg-open") == 1 then
        cmd = { "xdg-open", filepath }
    end

    if not cmd then
        vim.notify("No external opener available for: " .. filepath, vim.log.levels.WARN)
        return
    end

    local job_id = vim.fn.jobstart(cmd, { detach = true })
    if job_id <= 0 then
        vim.notify("Failed to open file: " .. filepath, vim.log.levels.ERROR)
    end
end

local function xueliang_open_mindmap()
    local mindmap_dir = vim.fn.expand("~/workspace/mindmap/")
    if vim.fn.isdirectory(mindmap_dir) == 0 then
        vim.notify("Directory not found: " .. mindmap_dir)
        return
    end
    require("telescope.builtin").find_files({
        prompt_title = "Open Mindmap",
        cwd = mindmap_dir,
        find_command = { "find", ".", "-maxdepth", "1", "-name", "*.svg", "-type", "f" },
        attach_mappings = function(prompt_bufnr, _)
            local actions = require("telescope.actions")
            local action_state = require("telescope.actions.state")
            actions.select_default:replace(function()
                local selection = action_state.get_selected_entry()
                actions.close(prompt_bufnr)
                if selection then
                    local filepath = mindmap_dir .. selection.value
                    xueliang_open_external_file(filepath)
                end
            end)
            return true
        end,
    })
end

-- Function Keys (aligned with spacemacs.el)
vim.keymap.set({"n","i"}, "<f2>", "<ESC>:TagbarToggle<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f3>", xueliang_open_mindmap, { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f4>", "<ESC>:x<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i","t"}, "<f5>", xueliang_terminal_popup, { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f6>", "<ESC>:Telescope registers<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f7>", ":make<CR>:copen<CR>", { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f8>", xueliang_imenu_or_org_today, { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f9>", xueliang_dired_sidebar, { noremap = true, silent = true })
vim.keymap.set({"n","i"}, "<f10>", xueliang_telescope_counsel, { noremap = true, silent = true })
vim.keymap.set({"n","i","t"}, "<c-f4>", xueliang_kill_buffer_and_window, { noremap = true, silent = true })
-- Note: F1 is used by tmux
