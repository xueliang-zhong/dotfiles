""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vundle settings
" https://github.com/VundleVim/Vundle.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" let vundle manage vundle
Plugin 'gmarik/vundle'

" PluginInstall/List/Update
Plugin 'kien/ctrlp.vim'           " fuzzy find files
Plugin 'scrooloose/nerdtree'      " file drawer, open with :NERDTreeToggle
Plugin 'benmills/vimux'           
Plugin 'tpope/vim-fugitive'       " the ultimate git helper
Plugin 'tpope/vim-commentary'     " comment/uncomment lines with gcc or gc in visual mode
Plugin 'taglist.vim'              " TList
Plugin 'a.vim'                    " Switch h/{cc,c} files
Plugin 'jlanzarotta/bufexplorer'  " BufExplorer
Plugin 'Valloric/YouCompleteMe'   " Auto-completion

call vundle#end()
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Xueliang
" These are my VIM flavor
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set ai
set autochdir
set autowrite
set autoread " detect when a file is changed
set background=dark
set backspace=2
"set clipboard=unamed
set cst
set cursorline
set expandtab
set foldenable
set foldlevel=1
set history=1000
set hls
set ignorecase
set smartcase 
set smartindent
set incsearch
set laststatus=2
set magic
set modeline
set mouse=a
set nocp
set novb
set nowrap
set nu
set ruler
set shiftwidth=2
set showcmd
set tabstop=2
set encoding=utf8
colorscheme evening

" For 100-char width limit, useful in code review
set colorcolumn=100
hi ColorColumn ctermbg=lightblue guibg=lightgrey

"Swap files are no good to me"
set noswapfile

"Life Chaning menu"
set wildmenu

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
exe "cs add ".expand(android_src)."/art/cscope.out"

" Show CScope find result in quickfix window - life changing
set cscopequickfix=s-,c-,d-,i-,t-,e-

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Taglist
let Tlist_Show_One_File = 1
let Tlist_Use_Right_Window = 1

" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map k gk
map j gj
map <C-J>     <C-E>g<Down>
map <C-K>     <C-Y>g<Up>

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

map <F7>      <ESC>:!clear<CR><ESC>:make -j2<CR>

map <F9>      <ESC>:NERDTreeToggle<CR>

map <F10>     <ESC>:BufExplorer<CR>
map <F12>     <ESC>*
            \ <ESC>:exec "grep " . expand("<cword>") .  " * -rn --exclude 'cscope.*' --exclude 'tags'"<CR>
            \ <ESC>:copen<CR>

map <C-N>     <ESC>:cn<CR>zz
map <C-P>     <ESC>:cp<CR>zz

map ]]        <ESC>:exe "pta " . expand("<cword>")<CR>
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>

