-- nvim functions/features that requires too much effort for me to maintain

-- tags generation/loading/jumping
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
    load_tags_from_dir()
    local cword = vim.fn.expand("<cword>")

    -- Use Telescope to preview tags
    require('telescope.builtin').tags({ default_text = cword })
end, { noremap = true, silent = true, desc = "ctags preview current <cword>" })
