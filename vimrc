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

" Spell, use British English.
set spell spelllang=en_gb
set nospell

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
inoremap b b<c-n><c-p>
inoremap c c<c-n><c-p>
inoremap d d<c-n><c-p>
inoremap e e<c-n><c-p>
inoremap f f<c-n><c-p>
inoremap g g<c-n><c-p>
inoremap h h<c-n><c-p>
inoremap i i<c-n><c-p>
inoremap j j<c-n><c-p>
inoremap k k<c-n><c-p>
inoremap l l<c-n><c-p>
inoremap m m<c-n><c-p>
inoremap n n<c-n><c-p>
inoremap o o<c-n><c-p>
inoremap p p<c-n><c-p>
inoremap q q<c-n><c-p>
inoremap r r<c-n><c-p>
inoremap s s<c-n><c-p>
inoremap t t<c-n><c-p>
inoremap u u<c-n><c-p>
inoremap v v<c-n><c-p>
inoremap w w<c-n><c-p>
inoremap x x<c-n><c-p>
inoremap y y<c-n><c-p>
inoremap z z<c-n><c-p>
inoremap A A<c-n><c-p>
inoremap B B<c-n><c-p>
inoremap C C<c-n><c-p>
inoremap D D<c-n><c-p>
inoremap E E<c-n><c-p>
inoremap F F<c-n><c-p>
inoremap G G<c-n><c-p>
inoremap H H<c-n><c-p>
inoremap I I<c-n><c-p>
inoremap J J<c-n><c-p>
inoremap K K<c-n><c-p>
inoremap L L<c-n><c-p>
inoremap M M<c-n><c-p>
inoremap N N<c-n><c-p>
inoremap O O<c-n><c-p>
inoremap P P<c-n><c-p>
inoremap Q Q<c-n><c-p>
inoremap R R<c-n><c-p>
inoremap S S<c-n><c-p>
inoremap T T<c-n><c-p>
inoremap U U<c-n><c-p>
inoremap V V<c-n><c-p>
inoremap W W<c-n><c-p>
inoremap X X<c-n><c-p>
inoremap Y Y<c-n><c-p>
inoremap Z Z<c-n><c-p>
inoremap _ _<c-n><c-p>
inoremap 0 0<c-n><c-p>
inoremap 1 1<c-n><c-p>
inoremap 2 2<c-n><c-p>
inoremap 3 3<c-n><c-p>
inoremap 4 4<c-n><c-p>
inoremap 5 5<c-n><c-p>
inoremap 6 6<c-n><c-p>
inoremap 7 7<c-n><c-p>
inoremap 8 8<c-n><c-p>
inoremap 9 9<c-n><c-p>

" Autocomplete with dictionary words when :set spell
set complete+=kspell
" For better performance in auto complete.
set complete-=i
set pumheight=12
set completeopt=menu

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
