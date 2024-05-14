""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin List
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'               " the ultimate git helper: Gdiff, Glog, Gstatus ...
Plug 'airblade/vim-gitgutter'           " show modifications to the file.
Plug 'tpope/vim-commentary'             " comment/uncomment lines with gcc or gc in visual mode
Plug 'majutsushi/tagbar'                " Tagbar
Plug 'preservim/nerdtree'
Plug 'vim-airline/vim-airline'          " Better status bar
Plug 'vim-airline/vim-airline-themes'
Plug 'ntpeters/vim-better-whitespace'   " Shows trailing whitespace, etc.
Plug 'jnurmine/Zenburn'                 " Theme.
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " Fuzzy find Plugins.
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-peekaboo'

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
" good options: evening, elflord, desert, delek, koehler, lunaperche, pablo, jellybeans, zenburn
colorscheme zenburn
if &diff
    colorscheme desert
endif
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
command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu norelativenumber laststatus=2   cursorline ruler   colorcolumn=100

command! XueliangCdGitRoot cd `git rev-parse --show-toplevel`
command! XueliangTABTrailingSpaces retab | %s/\s\+$//e | noh

" works better than Gdiffsplit
command! Gdiff vert terminal git diff HEAD

" Get some nice syntax highlighting
autocmd BufRead *.def set filetype=c
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
autocmd BufRead *.sc  set filetype=python
autocmd BufRead *.org set filetype=asm
autocmd BufRead *.cl  set filetype=c

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
function! Xueliang_fzf_git_log() abort
    " Get the list of commit hashes from git log
    let commits = systemlist('git log --format="%h %s"')

    " Open fzf in a floating window to select a commit
    let selected_commit = fzf#run({
        \ 'source': commits,
        \ 'options': ['--ansi', '--prompt', '> ', '--preview', 'git show {1} | bat --style=plain --color=always -l diff', '--preview-window', 'right:60%:wrap', '--reverse']
        \ })
endfunction

command! Glog  call Xueliang_fzf_git_log()
command! GlLog call Xueliang_fzf_git_log()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-airline
let g:airline_theme='zenburn'  " papercolor, zenburn, jellybeans

" Use Ag over Grep
" set grepprg=ag\ --nogroup\ --nocolor

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

" Don't start vim with NERDTree, my workflow is always try to go back to
" previous file by using History or Ctrl-o.
" autocmd VimEnter * NERDTree

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Auto complete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" My awesome auto complete feature. NOTE: For simplicity, I chose my own auto
" complete over coc plugin:
" if v:version >= 801
"   Plug 'neoclide/coc.nvim', {'branch': 'release'}
" endif
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

autocmd BufRead * call XueliangAutoComplete_ON()
command! XueliangAutoCompleteOFF call XueliangAutoComplete_OFF()

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

" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
nnoremap <Tab> za

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
" the + register is the register used for the OS clipboard in Vim.
map <leader>fy <ESC>:let @+=expand('%:p')<CR>
map <leader>gg <ESC>:Git<CR>
map <leader>gl <ESC>:Glog<CR><CR>
map <leader>gd <ESC>:Gdiff<CR><CR>
map <leader>th <ESC>:set cursorline<CR>
map <leader>x <ESC>:History:<CR>

" Show key bindings
nnoremap <Leader>? :Maps<CR>

" tag jumping
map <C-]>     <ESC>:exe "tj  " . expand("<cword>")<CR>

map <F4>      <ESC>:x<CR>

" <F5> given to tmux's split window, i.e. create a terminal in tmux, rather than vim.
" In vim, if I need, :terminal is quite easy to type anyway.
" map <F5>    <ESC>:terminal<CR>

nnoremap <F7>  :NERDTreeToggle<CR>
nnoremap <F8>  :TagbarToggle<CR>
nnoremap <F9>  :Files<CR>
nnoremap <F12> :!~/bin/links<CR><CR>

