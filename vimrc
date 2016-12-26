""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vundle settings
" https://github.com/VundleVim/Vundle.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" let vundle manage vundle
Plugin 'gmarik/vundle'

" PluginInstall/List/Update
Plugin 'kien/ctrlp.vim'           " fuzzy find files (Ctrl-P).
Plugin 'scrooloose/nerdtree'      " file drawer, open with :NERDTreeToggle
Plugin 'benmills/vimux'
Plugin 'tpope/vim-fugitive'       " the ultimate git helper: Gdiff, Glog, Gstatus ...
Plugin 'airblade/vim-gitgutter'   " show modifications to the file.
Plugin 'tpope/vim-commentary'     " comment/uncomment lines with gcc or gc in visual mode
Plugin 'taglist.vim'              " TList
Plugin 'a.vim'                    " Switch h/{cc,c} files
Plugin 'jlanzarotta/bufexplorer'  " BufExplorer
Plugin 'Valloric/YouCompleteMe'   " Auto-completion
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

call vundle#end()
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Xueliang
" These are my VIM flavor
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on

set autochdir
set autowrite
set autoread " detect when a file is changed
set clipboard=unnamed
set cursorline
set history=1000
set laststatus=2
set modeline
set mouse=a
set nocp
set novb
set nowrap
set ruler
set showcmd
set encoding=utf8

" line numbers
set nu
set relativenumber

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
colorscheme desert " other options: evening, elflord
set encoding=utf8
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
set colorcolumn=100  " useful in code review
hi ColorColumn ctermbg=lightblue guibg=lightgrey

" Swap files are no good to me
set noswapfile

" Life Chaning menu"
set wildmenu

" folding
set foldenable
set foldlevel=1

" set a map leader for more key combos
let mapleader = ','

au BufRead *.def set filetype=c
au BufRead *.log set filetype=asm
au BufRead *.txt set filetype=asm

let android_src = expand("/home/xueliang/workspace/Android-src/")

" Tags
exe "set tags+=".expand(android_src)."/art/tags"
exe "set tags+=".expand(android_src)."/framework/base/tags"
exe "set tags+=".expand(android_src)."/external/vixl/src/tags"
exe "set tags+=".expand(android_src)."/bionic/libc/tags"

" CScope
set cst  " include cscope tags
exe "cs add ".expand(android_src)."/art/cscope.out"

" Show CScope find result in quickfix window - life changing
set cscopequickfix=s-,c-,d-,i-,t-,e-

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions
" """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-h> :call WinMove('h')<cr>
map <C-j> :call WinMove('j')<cr>
map <C-k> :call WinMove('k')<cr>
map <C-l> :call WinMove('l')<cr>

" Window movement shortcuts
" move to the window in the direction shown
function! WinMove(key)
  let t:curwin = winnr()
  exec "wincmd ".a:key
  if (t:curwin == winnr())
    if (match(a:key,'[jk]'))
      "wincmd v
    else
      "wincmd s
    endif
  endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Taglist
let Tlist_Show_One_File = 1
let Tlist_Compact_Format = 1
let Tlist_Use_Right_Window = 1

" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'

" vim-airline
let g:airline_theme='papercolor'
let g:airline#extensions#ycm#enabled = 1
let g:airline#extensions#ycm#error_symbol   = 'E:' " set error count prefix
let g:airline#extensions#ycm#warning_symbol = 'W:' " set warning count prefix

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map k gk
map j gj

" Now the following old C-J/K maps conflicts with WinMove()
" Use C-e and C-y instead.
" map <C-J>     <C-E>g<Down>
" map <C-K>     <C-Y>g<Up>

map <F1>      <ESC>q:

map <F3>      <ESC>:execute "!c++filt " . expand("<cword>")<CR>

map <F4>      <ESC>*
            \ <ESC>:exe "cd " . expand(android_src)."/art/"<CR>
            \ <ESC>:cs f 0 <cword><CR>
            \ <ESC>:copen<CR>

map <F5>      <ESC>:grep "^Index:" %<CR>
            \ <ESC><ESC>:copen<CR>\
            \ <ESC><ESC><C-W>L

map <F6>      <ESC>:Tlist<CR>:TlistUpdate<CR><C-L>

map <F7>      <ESC>:!clear<CR><ESC>:make -j20<CR>

map <F9>      <ESC>:NERDTreeToggle<CR>

map <F10>     <ESC>:BufExplorer<CR>
map <F12>     <ESC>*
            \ <ESC>:exec "grep " . expand("<cword>") .  " * -rn --exclude 'cscope.*' --exclude 'tags'"<CR>
            \ <ESC>:copen<CR>

map <C-N>     <ESC>:cn<CR>zz

map ]]        <ESC>:exe "pta " . expand("<cword>")<CR>
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>

map <leader>b <ESC>:BufExplorer<CR>
map <leader>n <ESC>:NERDTreeToggle<CR>
map <leader>t <ESC>:Tlist<CR>:TlistUpdate<CR>
