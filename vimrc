""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vundle settings
" https://github.com/VundleVim/Vundle.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.vim/plugged')
Plug 'kien/ctrlp.vim'                   " fuzzy find files (Ctrl-P).
Plug 'vim-scripts/ctrlp-funky'          " improve CtrlP to fuzzy find functions
Plug 'scrooloose/nerdtree'              " file drawer, open with :NERDTreeToggle
Plug 'tpope/vim-fugitive'               " the ultimate git helper: Gdiff, Glog, Gstatus ...
Plug 'airblade/vim-gitgutter'           " show modifications to the file.
Plug 'tpope/vim-commentary'             " comment/uncomment lines with gcc or gc in visual mode
Plug 'majutsushi/tagbar'                " Tagbar
Plug 'jlanzarotta/bufexplorer'          " BufExplorer
Plug 'christoomey/vim-tmux-navigator'   " Tmuxleft
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
call plug#end()
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on

set autochdir
set autowrite
set autoread " detect when a file is changed
set clipboard=unnamedplus
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
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
set colorcolumn=100  " useful in code review
hi ColorColumn ctermbg=lightblue guibg=lightgrey

if has('gui_running')
  set guioptions-=T
  set guifont=Ubuntu\ Mono\ 15
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
set diffopt+=vertical

" Open new split panes to right and bottom, which feels more natural
set splitbelow splitright

let mapleader="\<Space>"

" Tags
let android_src = expand("~/workspace/linaro")
exe "set tags+=".expand(android_src)."/art/tags"
exe "set tags+=".expand(android_src)."/framework/base/tags"
exe "set tags+=".expand(android_src)."/external/vixl/src/tags"
exe "set tags+=".expand(android_src)."/bionic/libc/tags"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! Nonu set nonu norelativenumber
command! Noline set laststatus=0
command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Simplewindow set nonu norelativenumber laststatus=2 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu   relativenumber laststatus=2   cursorline   ruler
command! Richwindow   set   nu   relativenumber laststatus=2   cursorline   ruler

" commands using fzf framework
command! XueliangOpenlink call fzf#run({'source': 'cat ~/Dropbox/vim/xzhong-links.txt', 'sink': '!~/bin/open_link_arg2.sh', 'down' : '51%'})
command! XueliangProjects call fzf#run({'source': 'cat ~/Dropbox/vim/xzhong-projects.txt',  'sink': 'e', 'down' : '51%'})

" Useful in automatic code review; requires ~/bin/cpplint.py
autocmd BufRead *.{h,cc} command! Cpplint !cpplint.py --filter=-whitespace/line_length,-build/include %

" Get some nice syntax highlighting
autocmd BufRead *.def set filetype=c
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
autocmd BufRead *.org set filetype=asm

" In vimrc/.vim files, the K looks for vim help instead of man command.
autocmd BufRead {vimrc,.vimrc,*.vim} nmap K <ESC>:exe "help ".expand("<cword>")<CR>

" Sometimes, as an alternative to setting autochdir, the following command gives better results:
autocmd BufEnter * silent! lcd %:p:h

" Automatically save and load views.
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

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
" vim-airline
let g:airline_theme='zenburn'  " papercolor, zenburn, jellybeans
let g:airline#extensions#ycm#enabled = 1
let g:airline#extensions#ycm#error_symbol   = 'E:' " set error count prefix
let g:airline#extensions#ycm#warning_symbol = 'W:' " set warning count prefix

" Use Ag over Grep
set grepprg=ag\ --nogroup\ --nocolor

" Tagbar iconds
let g:tagbar_iconchars = ['+', '▼']

" Fzf
" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
"imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Auto complete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" This is quite awesome already.
inoremap a a<c-n><c-p>
inoremap e e<c-n><c-p>
inoremap i i<c-n><c-p>
inoremap o o<c-n><c-p>
inoremap u u<c-n><c-p>
inoremap A A<c-n><c-p>
inoremap E E<c-n><c-p>
inoremap I I<c-n><c-p>
inoremap O O<c-n><c-p>
inoremap U U<c-n><c-p>
inoremap _ _<c-n><c-p>
inoremap y y<c-n><c-p>
inoremap Y Y<c-n><c-p>
inoremap j j<c-n><c-p>
inoremap J J<c-n><c-p>
inoremap h h<c-n><c-p>
inoremap H H<c-n><c-p>
inoremap l l<c-n><c-p>

" Autocomplete with dictionary words when :set spell
set complete+=kspell
" For better performance in auto complete.
set complete-=i
set pumheight=12

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-g> <ESC><ESC>w

" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
nnoremap <Tab> za

" <leader> key mappings
map <leader>* <ESC>:exe "Ag! " . expand("<cword>")<CR>
map <leader><leader> <ESC>:History<CR>
map <leader>/ <ESC>:BLines<CR>
map <leader>f <ESC>:GFiles<CR>
map <leader>F <ESC>:Files<CR>
map <leader>g <ESC>:Gstatus<CR>
map <leader>p <ESC>:XueliangProjects<CR>
map <leader>: <ESC>:History:<CR>
map <leader>x <ESC>:History:<CR>
map <leader>X <ESC>:Commands<CR>

" Show key bindings
nnoremap <Leader>? :Maps<CR>

" tag jumping
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>
map ]]        <ESC>:exe "Tags " . expand("<cword>")<CR>

map <F4>      <ESC>:x<CR>
map <F5>      <ESC>:terminal<CR>

nnoremap <F8>   :BTags<CR>
nnoremap <C-F8> :TagbarToggle<CR>
nnoremap <F9>   :NERDTreeToggle %<CR>
nnoremap <C-F9> :NERDTreeFind<CR>
nnoremap <F12>  :XueliangOpenlink<CR>
