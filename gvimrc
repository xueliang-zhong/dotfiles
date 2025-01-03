"
" Presumably vimrc has done most of the config
" This is extra configs for gvim
"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Windows GVIM Specific
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    " GUI SETTINGS
    set guifont=JetBrains\ Mono\ NL:h12
    set guioptions-=T
    " start with full screen
    autocmd GUIEnter * simalt ~x

    " Grep support
    set grepprg=findstr\ /n\ /s
    set grepformat=%f:%l:%m

    " SHELL : set shell to Git Bash
    set shell=C:/Program\ Files/Git/bin/bash.exe
    set shellcmdflag=-c
    set shellxquote=

    " KEYS
    nnoremap <F8>  <ESC>:grep "^\*" %<CR>:copen<CR>
    nnoremap <F11> <ESC>:call OpenUrlWithExplorer()<CR>
    nnoremap <F12> <ESC>:terminal<CR>

    " org-mode: Alt-Enter to start a new task (only works on Windows)
    nnoremap <A-CR> <ESC>o-<ESC>a  <ESC>i
    inoremap <A-CR> <ESC>o-<ESC>a  <ESC>i

    " AUTOCMD & COMMANDS
    autocmd BufEnter xzhong-links.txt nnoremap <CR> :call OpenUrlWithExplorer()<CR>
    command! MyWorkSpace call MyWorkSpace()
endif

if has("mac")
    command! MyWorkSpace call MyWorkSpaceMac()
endif

command! InsertDate execute "normal! i" . strftime("%Y-%m-%d %H:%M")
nnoremap <C-c>. <ESC>o<ESC>:InsertDate<CR>
nnoremap <C-c><C-.> <ESC>o<ESC>:InsertDate<CR>

nnoremap <CR> :call OpenUrlAtPoint()<CR>
function! OpenUrlAtPoint()
  let word = expand('<cWORD>')
  let url_pattern = 'https\?://[^\s]*'
  if word =~# url_pattern
    echom "URL Found!"
    if has("mac")
      execute '!open ' . word
    endif
    if has("gui_running")
      execute '!explorer.exe ' . word
    endif
  else
    normal! j
  endif
endfunction

function! OpenUrlWithExplorer()
  " Get the current line
  let line = getline('.')
  " Extract URL using a regex
  let match = matchstr(line, 'https\?://[^\s]*')
  " If a URL is found, open it in the default browser
  if !empty(match)
    execute 'terminal explorer.exe ' . shellescape(match)
  else
    echo "No URL found on this line."
  endif
endfunction

function! MyWorkSpaceMac()
  execute 'only'
  " Just have these files in the background
  execute 'vi ~/Dropbox/life-notes/daily_life_2024.org'
  execute 'vi ~/Dropbox/life-notes/daily_life_2025.org'
  execute 'vi ~/workspace/dotfiles/init.lua'
  " back to the first note
  execute 'vi ~/Dropbox/life-notes/vim-note-2025.md'
  execute 'sp ~/workspace/dotfiles/gvimrc'
  " openning neotree anyway
  execute 'vs ~/Dropbox/life-notes'
endfunction

function! MyWorkSpace()
  execute 'only'
  " Open a vertical split with the specified file
  execute 'vs ~\OneDrive - Arm\work-notes'
  execute 'sp ~\workspace\dotfiles\gvimrc'
  " Move to the first window
  execute 'wincmd w'
  execute 'vi ~/workspace/org-notes/vim-work-2025.org'
  execute 'vi ~\OneDrive - Arm\work-notes\vim-work-2025.md'
  " just to help autocompletion
  execute 'tabnew ~\OneDrive - Arm\work-notes\daily_work_2024.org'
  execute 'tabnew ~\OneDrive - Arm\work-notes\daily_work_2025.org'
  " back to vim-work-2025.md
  execute 'tabfirst'
  execute 'tabonly'
  execute 'set filetype=diff'
endfunction

function! MyDailyWebsite()
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/owa/'
  execute 'terminal explorer.exe ' . 'https://mail.google.com/mail/u/0/#inbox'
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/owa/'
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/calendar/'
  execute 'terminal explorer.exe ' . 'https://arm-ce.slack.com/messages/'
  execute 'terminal explorer.exe ' . 'https://confluence.arm.com/display/~xuezho01/GPU+COMPUTE+TRASH+PANDA'
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/calendar/group/arm.com/unitedstatesofcompute/view/workweek'
  execute 'terminal explorer.exe ' . 'https://talent.arm.com'
  execute 'terminal explorer.exe ' . 'https://confluence.arm.com/plugins/inlinetasks/mytasks.action'
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => My Org Mode
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <ESC> :noh<CR><ESC>
inoremap <ESC> <ESC>:noh<CR><ESC>
autocmd BufRead,BufEnter *.org set filetype=diff
" treat obsidian markdown as diff type
autocmd BufRead,BufEnter *.md set filetype=diff

nnoremap <S-Left>  <ESC>:call ToggleDiffLine()<CR>
nnoremap <S-Right> <ESC>:call ToggleDiffLine()<CR>
inoremap <S-Left>  <ESC>l<ESC>:call ToggleDiffLine()<CR>
inoremap <S-Right> <ESC>l<ESC>:call ToggleDiffLine()<CR>

" Fold
" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
" - create fold: <tab> in visual mode (vip zf)
" - delete fold: zd
" - toggle fold: <Tab> or <Shift-Tab> (za)
vnoremap <Tab> :fold<CR>
nnoremap <S-Tab> :let &foldlevel = (&foldlevel == 0 ? 99 : 0)<CR>

" Define the function to toggle the first character
function! ToggleDiffLine()
  let line = getline(".")
  " If the first character is '-' or '+', toggle it
  if line[0] == '-' || line[0] == '+'
    let newchar = line[0] == '-' ? '+' : '-'
    call setline('.', newchar . line[1:])
  " If the first character is '*', prepend another '*'
  elseif line[0] == '*'
    call setline('.', '*' . line)
  endif
endfunction

nnoremap <C-c><C-c> :call MarkdownTaskDone()<CR>
function! MarkdownTaskDone()
  let line = getline(".")
  " If the first character is '-' or '+'
    " Replace '- DONE' with '-'
  if line =~ '. \[x\] DONE:'
    call setline('.', '-' . line[11:])
  elseif line[0] == '-' || line[0] == '+'
    " Mark task as done (+/-/* are all standard markdown list)
    call setline('.', '* [x] DONE:' . line[1:])
  endif
endfunction

nnoremap <Tab> :call ToggleOrgTreeFold()<CR>
function! ToggleOrgTreeFold()
  let line = getline(".")
  if line =~ '^\*\*\* ' || line =~ '\*\*\* DONE ' || line =~ '^\# ' || line =~ '^\#\# ' || line =~ '^\#\#\# '
    echom 'Tree Found'
    " Check if the current line is in a fold
    if foldclosed('.') != -1
      " Toggle the fold
      normal! za
    else
      " Create a new fold
      normal! vip
      normal! zf
    endif
  else
    " If no match, move forward like 'w'
    normal! w
  endif
endfunction

" Move to the head of the org-tree
nnoremap <C-c><C-u> <ESC>vipo

" My Org Sort Function
" Or just simply: vip:sort
command! Sort execute "normal! vip:sort\<CR>"
command! OrgSort execute "normal! vip:sort\<CR>"
command! XueliangOrgSort execute "normal! vip:sort\<CR>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => My Snippets
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NOTE:
" , as prefix for all my snippets
" ,m stands for markdown snipets
nnoremap ,mo   <ESC>o<ESC>:read ~/workspace/dotfiles/org-snippets.txt<CR>w<ESC>0w
nnoremap ,mc   <ESC>o<ESC>:read ~/workspace/dotfiles/code-snippets.txt<CR>0w
nnoremap ,mt   <ESC>:read ~/workspace/dotfiles/ob-daily-note-snippets.txt<CR>gg0dd

" I like quick key bindings
nnoremap ,o   <ESC>o<ESC>:read ~/workspace/dotfiles/org-snippets.txt<CR>w<ESC>0w
nnoremap ,c   <ESC>o<ESC>:read ~/workspace/dotfiles/code-snippets.txt<CR>0w
nnoremap ,T   <ESC>:read ~/workspace/dotfiles/ob-daily-note-snippets.txt<CR>gg0dd
nnoremap ,-    <ESC>o-<ESC>a  <ESC>i
nnoremap ,<CR> <ESC>o+<ESC>a  <ESC>i
nnoremap ,t <ESC>o<ESC>:InsertDate<CR> " t stands for time, timestamp
