""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin List
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')
Plug 'kien/ctrlp.vim'                   " fuzzy find files (Ctrl-P).
Plug 'vim-scripts/ctrlp-funky'          " improve CtrlP to fuzzy find functions
Plug 'scrooloose/nerdtree'              " file drawer, open with :NERDTreeToggle
Plug 'tpope/vim-fugitive'               " the ultimate git helper: Gdiff, Glog, Gstatus ...
Plug 'airblade/vim-gitgutter'           " show modifications to the file.
Plug 'tpope/vim-commentary'             " comment/uncomment lines with gcc or gc in visual mode
Plug 'majutsushi/tagbar'                " Tagbar
Plug 'vim-airline/vim-airline'          " Better status bar
Plug 'vim-airline/vim-airline-themes'
Plug 'yggdroot/indentline'              " Indentlines with chars like '|', useful in coding.
Plug 'ntpeters/vim-better-whitespace'   " Shows trailing whitespace, etc.
Plug 'octol/vim-cpp-enhanced-highlight' " Better c++11/14 highlighting.
Plug 'nanotech/jellybeans.vim'          " A very nice colorscheme
Plug 'https://github.com/jnurmine/Zenburn' " zenburn theme.
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " Fuzzy find Plugins.
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()
filetype plugin indent on

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
colorscheme zenburn " good options: evening, elflord, desert, delek, koehler, pablo, jellybeans, zenburn
if &diff
    colorscheme desert
endif
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
set colorcolumn=100  " useful in code review

if has('gui_running')
  set guioptions-=T
  set guifont=Dejavu\ Sans\ Mono\ 14
endif

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

let mapleader="\<Space>"

" Tags
let android_src = expand("~/workspace/linaro")
exe "set tags+=".expand(android_src)."/art/tags"

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
command! Nonu set nonu norelativenumber
command! Noline set laststatus=0
command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Simplewindow set nonu norelativenumber laststatus=2 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu   relativenumber laststatus=2   cursorline   ruler
command! Richwindow   set   nu   relativenumber laststatus=2   cursorline   ruler

command! XueliangCdGitRoot cd `git rev-parse --show-toplevel`
command! XueliangTABTrailingSpaces retab | %s/\s\+$//e | noh
command! XueliangDailyWebSites !~/bin/daily-websites.sh <CR>

" commands using fzf framework
command! XueliangOpenlink terminal ++close links
command! XueliangProjects call fzf#run({'source': 'cat ~/Dropbox/vim/xzhong-projects.txt',  'sink': 'e', 'down' : '51%'})

" works better than Gdiffsplit
command! Gdiff Gblame | /not.*commit.*

" TStation
command! XueliangTStation source ~/workspace/T/T-vim-T-station.vim

" Get some nice syntax highlighting
autocmd BufRead *.def set filetype=c
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
autocmd BufRead *.sc  set filetype=python
autocmd BufRead *.sc  nmap <buffer> <F8> <ESC>:BLines ###<CR>
autocmd BufRead *.org set filetype=asm
autocmd BufRead *.org nmap <buffer> <F8> <ESC>:BLines \*\*\* =><CR>
" autocmd BufRead *.org %s/\s\+$//e

" In vimrc/.vim files, the K looks for vim help instead of man command.
autocmd BufRead {vimrc,.vimrc,*.vim} nmap K <ESC>:exe "help ".expand("<cword>")<CR>

" Sometimes, as an alternative to setting autochdir, the following command gives better results:
autocmd BufEnter * silent! lcd %:p:h

" FZF
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" Files command with preview window
command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir GFiles
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! XueliangDailyT_Func()
  :cd ~/workspace/T/
  :make
  ":!cat T.sh | grep "echo.*:.*quick command" | fzf +s -e | sed "s/echo//" | sed "s/\"//g" | awk '{print $1}' | xargs bash "T.sh"
endfunction

function! XueliangHandleWebURL()
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
  echo s:uri
  if s:uri != ""
    silent exec "!google-chrome '".s:uri."'"
  else
    echo "No URI found in line."
  endif
endfunction

" Toggle NERDTree
function! ToggleNerdTree()
  if @% != "" && (!exists("g:NERDTree") || (g:NERDTree.ExistsForTab() && !g:NERDTree.IsOpen()))
    :NERDTreeFind
  else
    :NERDTreeToggle
  endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-airline
let g:airline_theme='zenburn'  " papercolor, zenburn, jellybeans

" Use Ag over Grep
set grepprg=ag\ --nogroup\ --nocolor

" Tagbar iconds
let g:tagbar_iconchars = ['+', '▼']
let g:tagbar_sort = 0  " avoid to preserve the oringal programe order.

" Fzf
" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
"imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})

" Command for creating Peekaboo window
let g:peekaboo_window = 'split bo 24new'

" gx to open link
let g:netrw_browsex_viewer = "google-chrome"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Auto complete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
" Useful for openning file in terminal.
nnoremap gf <C-W><C-V>gf

" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
nnoremap <Tab> za

" Emacs ivy style search
nnoremap / <ESC>:BLines<CR>

" <leader> key mappings
map <leader>* <ESC>:XueliangCdGitRoot<CR><ESC>:exe "Ag! " . expand("<cword>")<CR>
map <leader><leader> <ESC>:noh<CR><ESC>:History<CR>
map <leader>/ <ESC>:BLines<CR>
map <leader>? <ESC>:Maps<CR>
map <leader>: <ESC>:History:<CR>
map <leader>F <ESC>:Files!<CR>
map <leader>X <ESC>:Commands<CR>
map <leader>bs <ESC><C-W><C-N>
map <leader>wg <ESC><C-W>10+
map <leader>ff <ESC>:XueliangCdGitRoot<CR><ESC>:GFiles<CR>
map <leader>gg <ESC>:Gstatus<CR>
map <leader>pp <ESC>:XueliangProjects<CR><ESC>:GFiles<CR>
map <leader>x <ESC>:History:<CR>
map <leader>o <ESC>:call XueliangHandleWebURL()<CR><C-l><C-l>

" Show key bindings
nnoremap <Leader>? :Maps<CR>

" tag jumping
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>

map <F4>      <ESC>:x<CR>
map <F5>      <ESC>:terminal<CR>

nnoremap <F8>         :TagbarToggle<CR>
nnoremap <leader><F8> :BTags<CR>
nnoremap <C-F8>       :BTags<CR>
nnoremap <F9>         :Files<CR>
nnoremap <C-F9>       :call ToggleNerdTree()<CR>
nnoremap <F12>        :!~/bin/links<CR><CR>
nnoremap <S-F12>      :call XueliangDailyT_Func()<CR><CR>
nnoremap <C-F12>      :exe "!~/Dropbox/vim/open-sc-view.sh " . expand("<cword>")<CR><CR>
