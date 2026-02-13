"
" my simple & no plugin config of vimrc
"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on
filetype plugin indent on

set autochdir
set autowrite
set autoread " detect when a file is changed
set clipboard=unnamed,unnamedplus " works on both Linux and Mac OS
" set cursorline  " Disabled for performance
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
" Autocomplete with dictionary words when :set spell
set complete+=kspell
" For better performance in auto complete.
set complete-=i
set pumheight=9
set completeopt=menuone
" line numbers
set norelativenumber number
" search
set hls ignorecase incsearch smartcase magic
set autoindent smartindent
" tab settings
set tabstop=2 expandtab shiftwidth=2 smarttab
" make backspace behave in a sane manner
set backspace=indent,eol,start

" color/scheme settings
" good options: evening, elflord, desert, delek, koehler, lunaperche, pablo
colorscheme lunaperche
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
set colorcolumn=100  " useful in code review
set ttyfast " Faster redrawing

" avoid traditional vi stuff: EX mode, compatible with vi, ...
nnoremap Q <esc>
set noswapfile

" folding
set foldenable foldlevel=1

" makes ':find ' really fuzzy
set path+=**
set wildmenu wildmode=full

" Make split panes more natural. Always use vertical diffs
set diffopt=vertical
set splitbelow splitright

" Spell, use British English.
set spell spelllang=en_gb
set nospell

" Jump to the last position when reopening a file
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Get some nice syntax highlighting
autocmd BufRead *.log,*.txt,*.sc set filetype=asm
autocmd BufRead *.org,*.md set filetype=diff
autocmd BufRead *.def,*.cl set filetype=c
autocmd BufRead justfile,BUILD set filetype=bash

" Highlight trailing whitespaces
highlight ExtraWhitespace ctermbg=red guibg=red

" Match trailing whitespaces - only on BufRead to reduce lag
autocmd BufRead * match ExtraWhitespace /\s\+$/

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Git commands
command! Gblame  :terminal git blame %
command! Gcommit :terminal git commit -m "update %"
command! Gdiff   :vertical terminal git diff HEAD
command! Glog    :terminal git log
command! Gshow   :terminal git show
command! Gwrite  :terminal git add %

command! Sort execute "normal! vip:sort\<CR>"
command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu norelativenumber laststatus=2   cursorline ruler   colorcolumn=100
command! RecentFiles execute 'browse oldfiles' | execute 'let v:oldfiles = v:oldfiles[0:15]'
command! GitRoot cd `git rev-parse --show-toplevel`
command! XueliangTABTrailingSpaces retab | %s/\s\+$//e | noh
command! Scratch rightbelow vsplit | enew | setlocal buftype=nofile bufhidden=wipe noswapfile
command! TerminalPopup botright split | resize 12 | terminal
command! ToggleReadonly setlocal readonly!

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Auto complete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
    " Create the map again to avoid the "no such mapping" error when calling
    " this function twice.
    execute 'inoremap ' . key . ' ' . key . '<C-n><C-p>'
    " Then properly unmap the key.
    execute 'iunmap ' . key
  endfor
endfunction

" Enable my autocomplete globaly, however only for vim (nvim's v:verion == 801)
" DISABLED for Neovim since nvim-cmp handles autocomplete
if v:version >= 900 && !has('nvim')
    autocmd BufRead * call XueliangAutoComplete_ON()
endif
command! AutoCompleteOFF call XueliangAutoComplete_OFF()

" Improve <Enter> key's behaviour in autocomplete.
inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"
" Improve <Tab> key's behaviour in autocomplete, like a clever tab.
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-g> <ESC><ESC>

" Terminal keys
tnoremap <F1> <C-W>N
tnoremap § <C-W>N
tnoremap <F4> <C-\><C-n>:close<CR>
tnoremap <C-F4> <C-\><C-n>:bdelete<CR>
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

" Fold
" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
" - create fold: <tab> in visual mode (vip zf)
" - delete fold: zd
" - toggle fold: <Tab> or <Shift-Tab> (za)
vnoremap <Tab> :fold<CR>
nnoremap <S-Tab> :let &foldlevel = (&foldlevel == 0 ? 99 : 0)<CR>
nnoremap <Tab> za

" Some other cool key mappings
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
" Keybindings for moving lines like moving tasks in org-mode
vnoremap <S-Down> :m '>+1<CR>gv=gv
vnoremap <S-Up>   :m '<-2<CR>gv=gv
nnoremap <S-Down> V:m '>+1<CR>gv=gv
nnoremap <S-Up>   V:m '<-2<CR>gv=gv

" Better J behaviour in joining lines
nnoremap J mzJ`z
" Keep visual selection when indenting/outdenting
vmap < <gv
vmap > >gv

" <leader> key mappings
let mapleader=" "
nnoremap <leader><leader> <ESC>:buffers<CR>:buffer<Space>
nnoremap <leader>/ /
nnoremap <leader>ff :find **<C-d><Delete><Delete>
nnoremap <leader>fp :find ~/workspace/dotfiles/**<C-d><Delete><Delete>
nnoremap <leader>fr :RecentFiles<CR>
nnoremap <leader>\ :RecentFiles<CR>
nnoremap <leader>gg <ESC>:terminal git status<CR>
nnoremap <leader>gf <ESC>:GitRoot<CR>:find **<C-d><Delete><Delete>
nnoremap <leader>gh <ESC>:Gdiff<CR>
nnoremap <leader>bs :Scratch<CR>
nnoremap <leader>bw :ToggleReadonly<CR>
nnoremap <leader>cc :make<CR>:copen<CR>
nnoremap <leader>wc :close<CR>
nnoremap <leader>wg <C-w>=
nnoremap <leader>sj :jumps<CR>
nnoremap <leader>si :call XueliangIndexOrToday()<CR>
nnoremap <leader>x :command<CR>
nnoremap <leader>fy <ESC>:let @+=expand('%:p')<CR>:echom "File path coped"<CR>
nnoremap <leader>th :if &cursorline == 1 \| set nocursorline \| else \| set cursorline \| endif<CR>
nnoremap <leader>? :map<CR> " Show key bindings
nnoremap <leader>* <ESC>:on<CR>ma:grep <cword> %<CR>:copen<CR><C-w><C-w>`a
nnoremap <leader><CR> <ESC>:RecentFiles<CR>

" simple tags support
" support both fast (./tags) & slow (GIT_ROOT/tags) tags generation & loading
nnoremap <leader>tt <ESC>:!ctags -R --exclude=.git --exclude=build<CR><C-\><C-n>:set tags+=tags<CR>
nnoremap <leader>tT <ESC>:GitRoot<CR>:!ctags -R --exclude=.git --exclude=build<CR><C-\><C-n>:set tags+=tags<CR>
nnoremap <leader>] <ESC>g]

" function keys
nnoremap <F3>  <ESC>:leftabove vs .<CR>:vertical resize 30<CR>
inoremap <F3>  <ESC>:leftabove vs .<CR>:vertical resize 30<CR>
nnoremap <F4>  <ESC>:close<CR>
inoremap <F4>  <ESC>:close<CR>
nnoremap <C-F4> <ESC>:bdelete<CR>
inoremap <C-F4> <ESC>:bdelete<CR>
nnoremap <F6>  <ESC>:registers<CR>
inoremap <F6>  <ESC>:registers<CR>
nnoremap <F5>  :TerminalPopup<CR>
inoremap <F5>  <ESC>:TerminalPopup<CR>
nnoremap <F7>  <ESC>:make<CR>:copen<CR>
inoremap <F7>  <ESC>:make<CR>:copen<CR>
nnoremap <F9> :find **<C-d><Delete><Delete>
inoremap <F9> <ESC>:find **<C-d><Delete><Delete>
nnoremap <F10> <ESC>:command<CR>
inoremap <F10> <ESC>:command<CR>

function! XueliangIndexOrToday()
  if expand('%:e') ==# 'org' || expand('%:e') ==# 'md'
    execute '/' . strftime('%Y-%m-%d')
    normal! zz
    noh
  else
    silent! vimgrep /\v^\s*(def|class|function|local function|defun)\>/j %
    copen
  endif
endfunction

nnoremap <F8> :call XueliangIndexOrToday()<CR>
inoremap <F8> <ESC>:call XueliangIndexOrToday()<CR>

" emacs's DeleteBlankLines approach is so good
nnoremap <C-x><C-o> <ESC>:call DeleteBlankLines()<CR>
inoremap <C-x><C-o> <ESC>:call DeleteBlankLines()<CR>
function! DeleteBlankLines()
  " Start a loop to delete all consecutive blank lines
  while getline('.') =~ '^\s*$'
    execute "normal! dd"
    if line('.') > line('$')
      break
    endif
  endwhile
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Windows GUI Support
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    " GUI SETTINGS
    set grepformat=%f:%l:%m
    set grepprg=findstr\ /n\ /s
    set guifont=JetBrains\ Mono\ NL:h12
    set guioptions-=T
    " start with full screen
    autocmd GUIEnter * simalt ~x

    " SHELL : set shell to Git Bash
    set shell=C:/Program\ Files/Git/bin/bash.exe
    set shellcmdflag=-c
    set shellxquote=
endif
