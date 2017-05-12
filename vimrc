""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vundle settings
" https://github.com/VundleVim/Vundle.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" let vundle manage vundle
Plugin 'gmarik/vundle'

" PluginInstall/List/Update
Plugin 'kien/ctrlp.vim'                   " fuzzy find files (Ctrl-P).
Plugin 'vim-scripts/ctrlp-funky'          " improve CtrlP to fuzzy find functions
Plugin 'scrooloose/nerdtree'              " file drawer, open with :NERDTreeToggle
Plugin 'benmills/vimux'
Plugin 'tpope/vim-fugitive'               " the ultimate git helper: Gdiff, Glog, Gstatus ...
Plugin 'airblade/vim-gitgutter'           " show modifications to the file.
Plugin 'tpope/vim-commentary'             " comment/uncomment lines with gcc or gc in visual mode
Plugin 'majutsushi/tagbar'                " Tagbar
Plugin 'a.vim'                            " Switch h/{cc,c} files
Plugin 'jlanzarotta/bufexplorer'          " BufExplorer
"Plugin 'Valloric/YouCompleteMe'           " Auto-completion
Plugin 'rking/ag.vim'                     " Ag (silver searcher)
Plugin 'christoomey/vim-tmux-navigator'   " Tmuxleft
Plugin 'vim-airline/vim-airline'          " Better status bar
Plugin 'vim-airline/vim-airline-themes'
Plugin 'yggdroot/indentline'              " Indentlines with chars like '|', useful in coding.
Plugin 'ntpeters/vim-better-whitespace'   " Shows trailing whitespace, etc.
Plugin 'octol/vim-cpp-enhanced-highlight' " Better c++11/14 highlighting.
Plugin 'nanotech/jellybeans.vim'          " A very nice colorscheme

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
set novb belloff=all
set nowrap
set ruler
set showcmd
set encoding=utf8

" avoid traditional vi stuff: EX mode, compatible with vi, ...
nnoremap Q <esc>
set nocompatible

" line numbers
set relativenumber number

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
colorscheme elflord " good options: evening, elflord, desert, delek, koehler, pablo, jellybeans
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
set colorcolumn=100  " useful in code review
hi ColorColumn ctermbg=lightblue guibg=lightgrey

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

" Autocomplete with dictionary words when spell check is on
set complete+=kspell

" Always use vertical diffs: for example Gdiff, diffsplit, etc
set diffopt+=vertical

" Open new split panes to right and bottom, which feels more natural
set splitbelow splitright

" set a map leader for more key combos
let mapleader="\<Space>"

let android_src = expand("~/workspace/linaro")

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

" Hard to type things
iabbrev --> →
iabbrev <-- ←
iabbrev ^^  ↑
iabbrev VV  ↓

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! Nonu set nonu norelativenumber
command! Noline set laststatus=0
command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Simplewindow set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu   relativenumber laststatus=2   cursorline   ruler
command! Richwindow   set   nu   relativenumber laststatus=2   cursorline   ruler

autocmd BufRead todo Smallwindow   " my todo file is usually opened in a small window.
autocmd BufRead .vimrc Smallwindow

" Useful in automatic code review; requires ~/bin/cpplint.py
autocmd BufRead *.{h,cc} command! Cpplint !cpplint.py --filter=-whitespace/line_length,-build/include %

" Get some nice syntax highlighting
autocmd BufRead *.def set filetype=c
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm

" In vimrc/.vim files, the K looks for vim help instead of man command.
autocmd BufRead {vimrc,.vimrc,*.vim} nmap K <ESC>:exe "help ".expand("<cword>")<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! CleverTab()
  if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
    return "\<Tab>"
  else
    return "\<C-N>"
  endif
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_min_num_of_chars_for_completion = 1

" vim-airline
let g:airline_theme='papercolor'
let g:airline#extensions#ycm#enabled = 1
let g:airline#extensions#ycm#error_symbol   = 'E:' " set error count prefix
let g:airline#extensions#ycm#warning_symbol = 'W:' " set warning count prefix

" CtrlP funky highlighting
let g:ctrlp_funky_syntax_highlight = 1

" Use Ag over Grep
set grepprg=ag\ --nogroup\ --nocolor

" Tagbar iconds
let g:tagbar_iconchars = ['+', '▼']

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ESC also helps removes the search high-lighting.
map <ESC>     :noh<CR>

" <leader> key mappings
map <leader>i <ESC>:TagbarToggle<CR>
map <Leader>f <ESC>:CtrlP<CR>

" BufExplorer
map <leader><leader> <ESC>:BufExplorer<CR>
map <leader>b <ESC>:BufExplorer<CR>

" NERDTree
map <leader>nf <ESC>:NERDTreeFind<CR>
map <leader>nt <ESC>:NERDTreeToggle<CR>

" search for current word or any word
map <leader>a <ESC>:Ag<CR>

" Cscope search in my projects
map <leader>cs <ESC>*
             \ <ESC>:exe "cd " . expand(android_src)."/art/"<CR>
             \ <ESC>:cs f 0 <cword><CR>
             \ <ESC>:copen<CR>

" Quicker window and tmux pane movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" next in quick fix window
map <C-N>     :cn<CR>zz

" tag jumping
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>
map ]]        <ESC>:exe "pta " . expand("<cword>")<CR>

map <F3>      <ESC>:execute "!c++filt " . expand("<cword>")<CR>

map <F7>      <ESC>:!clear<CR><ESC>:make -j33<CR>

"
" The following key maps are obsolete.
"

" No need for g[jk] any more because most of the width limit.
" map k gk
" map j gj

" Now the following old C-J/K maps conflicts with WinMove()
" Use C-e and C-y instead.
" map <C-J>     <C-E>g<Down>
" map <C-K>     <C-Y>g<Up>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Usage Suggestions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" - For Plugins, http://vimawesome.com/ looks like a good place to go to.
" - :grep uses Ag now, which is better than :vimgrep
" - Command-line window: q: q/ q? in normal mode, or C-F in command-line mode.
" - Use C-j and C-k to navigate CtrlP's result window.
" - Use C-space to trigger completion from YCM; and use C-X C-P to trigger form vim.
" - Set Chrome's secure shell to open in a seperate window, so that C-T/C-N/C-P all work in vim.
" - :map to browse current key mappings.
" - :command to browse current commands.
" - Use gcc in normal mode to quickly comment code.
"
" - vim's built in complete:
"  |i_CTRL-X_CTRL-L| Whole lines
"  |i_CTRL-X_CTRL-N| keywords in the current file
"  |i_CTRL-X_CTRL-K| keywords in 'dictionary'
"  |i_CTRL-X_CTRL-T| keywords in 'thesaurus', thesaurus-style
"  |i_CTRL-X_CTRL-I| keywords in the current and included files
"  |i_CTRL-X_CTRL-]| tags
"  |i_CTRL-X_CTRL-F| file names
"  |i_CTRL-X_CTRL-D| definitions or macros
"  |i_CTRL-X_CTRL-V| Vim command-line
"  |i_CTRL-X_CTRL-U| User defined completion
"  |i_CTRL-X_CTRL-O| omni completion
"  |i_CTRL-X_s|      Spelling suggestions
"  |i_CTRL-N|        keywords in 'complete'
"  and use C-E to exit the completion.
