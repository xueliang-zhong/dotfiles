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
Plug 'jceb/vim-orgmode'
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
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
set background=dark
" set colorcolumn=100  " useful in code review

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

" Spell, use British English.
set spell spelllang=en_gb
set nospell

" keep visual selection when indenting/outdenting
vmap < <gv
vmap > >gv

set ttyfast " faster redrawing

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! Nonu set nonu norelativenumber
command! Noline set laststatus=0
command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Simplewindow set nonu norelativenumber laststatus=2 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu   relativenumber laststatus=2   cursorline   ruler
command! Richwindow   set   nu   relativenumber laststatus=2   cursorline   ruler

command! XueliangCdGitRoot cd %:h | cd `git rev-parse --show-toplevel`
command! XueliangLinaroDebug call XueliangLinaroDebug_Func()

" commands using fzf framework
command! XueliangOpenlink terminal ++close links
command! XueliangProjects call fzf#run({'source': 'cat ~/Dropbox/vim/xzhong-projects.txt',  'sink': 'e', 'down' : '51%'})

" Get some nice syntax highlighting
autocmd BufRead *.def set filetype=c
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
autocmd BufRead *.org set filetype=asm

" In vimrc/.vim files, the K looks for vim help instead of man command.
autocmd BufRead {vimrc,.vimrc,*.vim} nmap K <ESC>:exe "help ".expand("<cword>")<CR>
" vimrc buffer local <F8> key.
autocmd BufRead {vimrc,.vimrc} nmap <buffer> <F8> <ESC>:BLines ^" =><CR>

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
function! XueliangLinaroDebug_Func()
  " Better colors in debugger
  :set nocursorline
  :colorscheme elflord
  " Key mappings in debugger.
  :nmap <C-F5>  <ESC>:Continue<CR>
  :nmap <C-F9>  <ESC>:Break<CR>
  :nmap <C-F10> <ESC>:Over<CR>
  :nmap <C-F11> <ESC>:Step<CR>
  :nmap <S-F11> <ESC>:Finish<CR>
  :nmap U <ESC>:call TermDebugSendCommand('up')<CR>
  :nmap D <ESC>:call TermDebugSendCommand('down')<CR>
  :nmap F <ESC>:Finish<CR>
  :nmap N <ESC>:Over<CR>
  " Make sure the debugger's in the right folder.
  :cd ~/workspace/linaro/
  " Start the debugger and execute the init gdb commands.
  :packadd termdebug
  :Termdebug
  :call TermDebugSendCommand('source ~/workspace/linaro/gdb.init')
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

" Fzf
" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
"imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Auto complete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" This is quite awesome completion already.
function! OpenCompletion()
  if pumvisible()
    return
  endif
  if (v:char >= 'a' && v:char <= 'z') || (v:char >= 'A' && v:char <= 'Z') || (v:char >= '0' && v:char <= '9') || (v:char == '_')
    " Use C-x C-N local completion or C-N completion based on buffer count.
    let buffer_count = len(getbufinfo({'buflisted':1}))
    call feedkeys (buffer_count >= 3 ? "\<C-x>\<C-n>\<C-p>" : "\<C-n>\<C-p>")
  endif
endfunction
"" Turn on auto complete by default.
" autocmd InsertCharPre * call OpenCompletion()
"" I can still disable autocomplete when it becomes annoying.
" command! Nocomplete autocmd! InsertCharPre *

" Improve <Enter> key's behaviour in autocomplete.
" inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"
" Improve <Tab> key's behaviour in autocomplete, like a clever tab.
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

function! CleverTab()
   if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
      return "\<Tab>"
   else
      return "\<C-N>"
   endif
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>

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
" tnoremap <Esc> <C-W>N
set notimeout ttimeout timeoutlen=100

" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
nnoremap <Tab> za

" <leader> key mappings
map <leader>* <ESC>:XueliangCdGitRoot<CR><ESC>:exe "Ag! " . expand("<cword>")<CR>
map <leader><leader> <ESC>:noh<CR><ESC>:History<CR>
map <leader>/ <ESC>:BLines<CR>
map <leader>f <ESC>:XueliangCdGitRoot<CR><ESC>:GFiles!<CR>
map <leader>F <ESC>:Files!<CR>
map <leader>g <ESC>:Gstatus<CR>
map <leader>p <ESC>:XueliangProjects<CR><ESC>:GFiles!<CR>
map <leader>: <ESC>:History:<CR>
map <leader>x <ESC>:History:<CR>
map <leader>X <ESC>:Commands<CR>

" Show key bindings
nnoremap <Leader>? :Maps<CR>

" tag jumping
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>

map <F4>      <ESC>:x<CR>
map <F5>      <ESC>:terminal<CR>

nnoremap <F8>         :BTags<CR>
nnoremap <leader><F8> :BTags<CR>
nnoremap <C-F8>       :TagbarToggle<CR>
nnoremap <F9>         :Files<CR>
nnoremap <C-F9>       :call ToggleNerdTree()<CR>
nnoremap <F12>        :XueliangOpenlink<CR>
nnoremap <C-F12>      :!~/bin/daily-work.sh<CR>
