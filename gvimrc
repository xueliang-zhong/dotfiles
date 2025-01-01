"
" No plugin config of vimrc
"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on

set autochdir
set autowrite
set autoread " detect when a file is changed
set clipboard=unnamedplus
set clipboard+=unnamed " works on Mac OS
set cursorline
set history=1000
set laststatus=2
set modeline
set mouse=a
set novb belloff=all
set nowrap
set nocompatible
set ruler
set showcmd
set encoding=utf8
set makeprg=just

" avoid traditional vi stuff: EX mode, compatible with vi, ...
nnoremap Q <esc>
set nocompatible

" line numbers
set norelativenumber number

" search
set hls
set ignorecase
set incsearch
set smartcase
set magic

" indent
set autoindent
set smartindent

" tab settings
set tabstop=2
set expandtab
set shiftwidth=2
set smarttab

" make backspace behave in a sane manner
set backspace=indent,eol,start

" color/scheme settings
" good options: evening, elflord, desert, delek, koehler, lunaperche, pablo
colorscheme lunaperche
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
set colorcolumn=100  " useful in code review

" Swap files are no good to me
set noswapfile

" Life Chaning menu"
set wildmenu
set wildmode=full

" folding
set foldenable
set foldlevel=1

" makes ':find ' really fuzzy
set path+=**

" Always use vertical diffs: for example Gdiff, diffsplit, etc
set diffopt=vertical

" Open new split panes to right and bottom, which feels more natural
set splitbelow splitright

" Spell, use British English.
set spell spelllang=en_gb
set nospell

" keep visual selection when indenting/outdenting
vmap < <gv
vmap > >gv

set ttyfast " faster redrawing

" Jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Git commands
command! Gwrite  :terminal git add %
command! Gdiff   :vertical terminal git diff HEAD
command! Gblame  :terminal git blame %
command! Gcommit :terminal git commit -m "update %s"
command! Glog    :terminal git log
command! Gshow   :terminal git show

command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu norelativenumber laststatus=2   cursorline ruler   colorcolumn=100

" command! XueliangCdGitRoot cd `git rev-parse --show-toplevel`
command! XueliangTABTrailingSpaces retab | %s/\s\+$//e | noh
command! DailyWebsite call MyDailyWebsite()

" Get some nice syntax highlighting
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
autocmd BufRead *.def set filetype=c
autocmd BufRead *.cl  set filetype=c
autocmd BufRead *.sc  set filetype=python
autocmd BufRead justfile set filetype=bash

" In vimrc/.vim files, the K looks for vim help instead of man command.
autocmd BufRead {vimrc,.vimrc,*.vim} nmap K <ESC>:exe "help ".expand("<cword>")<CR>

" Sometimes, as an alternative to setting autochdir, the following command gives better results:
autocmd BufEnter * silent! lcd %:p:h

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Auto complete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" My awesome auto complete feature. NOTE: For simplicity, I chose my own auto
" complete over coc plugin.
"
" A small issue with this approach is such auto complete is always triggered
" when copying text into vim. Need to call XueliangAutoCompleteOFF command.
"
" Tune down the sensibility a bit: don't add too many keys to trigger
" autocomplete too quickly - could become a bit distracting.
"
let g:xueliang_auto_complete_keys = [
    \ 'a', 'e', 'i', 'o', 'u',
    \ 'A', 'E', 'I', 'O', 'U',
    \ 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
    \ 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L',
    \ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    \ '_'
    \ ]

function! XueliangAutoComplete_ON()
  for key in g:xueliang_auto_complete_keys
    execute 'inoremap ' . key . ' ' . key . '<C-n><C-p>'
  endfor
endfunction

function! XueliangAutoComplete_OFF()
  for key in g:xueliang_auto_complete_keys
    execute 'iunmap ' . key
  endfor
endfunction

command! AutoCompleteOFF call XueliangAutoComplete_OFF()

" Improve <Enter> key's behaviour in autocomplete.
inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"
" Improve <Tab> key's behaviour in autocomplete, like a clever tab.
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

" Autocomplete with dictionary words when :set spell
set complete+=kspell
" For better performance in auto complete.
set complete-=i
set pumheight=9
set completeopt=menuone

filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-g> <ESC><ESC>

" Terminal keys
tnoremap <F1> <C-W>N
tnoremap § <C-W>N
tnoremap <F4> <C-W>N<ESC>:q!<CR>
set notimeout ttimeout timeoutlen=100

" Better window movements in normal mode
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>

" Better window movements in insert mode
inoremap <C-h> <C-\><C-n><C-w><C-h>
inoremap <C-j> <C-\><C-n><C-w><C-j>
inoremap <C-k> <C-\><C-n><C-w><C-k>
inoremap <C-l> <C-\><C-n><C-w><C-l>

" My fuzzy search in /
cnoremap <expr> <Space> getcmdtype() == '/' ? '.*' : ' '

" <leader> key mappings
let mapleader=" "
nnoremap <leader><leader> <ESC>:buffers<CR>:buffer<Space>
" this should be consistent with <f9>
nnoremap <leader>ff :find **<C-d><Delete><Delete>
nnoremap <leader>gg <ESC>:terminal git status<CR>
nnoremap <leader>bs <ESC><C-W><C-N>
nnoremap <leader>fy <ESC>:let @+=expand('%:p')<CR>:echom "File path coped"<CR>
nnoremap <leader>th <ESC>:set cursorline<CR>
nnoremap <leader>? :map<CR> " Show key bindings
nnoremap <leader>* <ESC>:on<CR>ma:grep <cword> %<CR>:copen<CR><C-w><C-w>`a

command! LeaderRecentFiles execute 'browse oldfiles' | execute 'let v:oldfiles = v:oldfiles[0:15]'
nnoremap <leader><CR> <ESC>:LeaderRecentFiles<CR>

vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Keybindings for moving lines like moving tasks in org-mode
vnoremap <S-Down> :m '>+1<CR>gv=gv
vnoremap <S-Up>   :m '<-2<CR>gv=gv
nnoremap <S-Down> V:m '>+1<CR>gv=gv
nnoremap <S-Up>   V:m '<-2<CR>gv=gv

nnoremap <F3>  <ESC>:leftabove vs .<CR>:vertical resize 30<CR>
nnoremap <F4>  <ESC>:x<CR>
nnoremap <F6>  <ESC>:registers<CR>
nnoremap <F5>  <ESC>:terminal<CR>
nnoremap <F7>  <ESC>:make<CR>:copen<CR>

" both approaches are cool
" nnoremap <F9>  <ESC>:vi .<CR>
nnoremap <F9> :find **<C-d><Delete><Delete>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Windows GVIM Specific
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    " GUI SETTINGS
    set grepformat=%f:%l:%m
    set grepprg=findstr\ /n\ /s
    set guifont=JetBrains\ Mono\ NL:h12
    set guioptions-=T
    " start with full screen
    autocmd GUIEnter * simalt ~x
    " only turn my autocompletion on in gvim, to avoid clash with nvim's
    autocmd BufRead * call XueliangAutoComplete_ON()

    " SHELL : set shell to Git Bash
    set shell=C:/Program\ Files/Git/bin/bash.exe
    set shellcmdflag=-c
    set shellxquote=

    " KEYS
    nnoremap <F8>  <ESC>:grep "^\*" %<CR>:copen<CR>
    nnoremap <F11> <ESC>:call OpenUrlWithExplorer()<CR>
    nnoremap <F12> <ESC>:terminal<CR>
    " org-mode: Alt-Enter to start a new task
    nnoremap <A-CR> <ESC>o-<ESC>a  <ESC>i
    inoremap <A-CR> <ESC>o-<ESC>a  <ESC>i

    " AUTOCMD & COMMANDS
    autocmd BufEnter xzhong-links.txt nnoremap <CR> :call OpenUrlWithExplorer()<CR>
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
  execute 'vs ~/Dropbox/life-notes/vim-TODO.md'
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

" Ctrl-s to save my selected org tree to obsidian
vnoremap <C-s> :<C-u>call SendToObsidian()<CR>
function! SendToObsidian()
    " Get the selected text
    let l:save_cursor = getpos(".")
    let l:save_visual = getpos("'<")
    normal! gv"xy
    call setpos('.', l:save_cursor)
    call setpos("'<", l:save_visual)

    " Get the current <date>.md file used by obsidian
    let l:date = strftime("%Y-%m-%d")
    let l:filename = l:date . '.md'

    " Append the selected text to the file
    " NOTE: don't add new line here, as it is different on Windows/Mac
    call writefile(getreg('x', 1, 1), l:filename, 'a')

    " Notify me
    echom "Notes sent to obsidian."
endfunction

nnoremap <C-x><C-o> <ESC>:call DeleteBlankLines()<CR>
inoremap <C-x><C-o> <ESC>:call DeleteBlankLines()<CR>
function! DeleteBlankLines()
  " Start a loop to delete all consecutive blank lines
  while getline('.') =~ '^\s*$'
    execute "normal! dd"
    " Stop if we've reached the last line
    if line('.') > line('$')
      break
    endif
  endwhile
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

nnoremap ,-    <ESC>o-<ESC>a  <ESC>i
nnoremap ,<CR> <ESC>o+<ESC>a  <ESC>i
" t stands for time, timestamp
nnoremap ,t <ESC>o<ESC>:InsertDate<CR>
