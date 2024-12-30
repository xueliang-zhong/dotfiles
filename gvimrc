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

let mapleader=" "

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
command! Gdiff   :terminal git diff HEAD
command! Gblame  :terminal git blame %
command! Gcommit :terminal git commit

command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu norelativenumber laststatus=2   cursorline ruler   colorcolumn=100

command! XueliangCdGitRoot cd `git rev-parse --show-toplevel`
command! XueliangTABTrailingSpaces retab | %s/\s\+$//e | noh

" Get some nice syntax highlighting
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
autocmd BufRead *.org set filetype=asm
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

nnoremap <leader><leader> <ESC>:buffers<CR>:buffer<Space>

" this should be consistent with <f9>
nnoremap <leader>ff :find **<C-d><Delete><Delete>

nnoremap <leader>gg <ESC>:terminal git status<CR>

nnoremap <leader>bs <ESC><C-W><C-N>
nnoremap <leader>fy <ESC>:let @+=expand('%:p')<CR>:echom "File path coped"<CR>
nnoremap <leader>th <ESC>:set cursorline<CR>
nnoremap <leader>? :map<CR> " Show key bindings

nnoremap <leader>* <ESC>:on<CR>ma:grep <cword> %<CR>:copen<CR><C-w><C-w>`a

vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

nnoremap <F3>  <ESC>:leftabove vs .<CR>:vertical resize 30<CR>
nnoremap <F4>  <ESC>:x<CR>
nnoremap <F6>  <ESC>:registers<CR>
nnoremap <F5>  <ESC>:terminal<CR>
nnoremap <F7>  <ESC>:make<CR>:copen<CR>

" both approaches are cool
" nnoremap <F9>  <ESC>:vi .<CR>
nnoremap <F9> :find **<C-d><Delete><Delete>

nnoremap <F11> <ESC>:on<CR><ESC>:copen<CR><C-w><C-w><ESC>:leftabove vs .<CR>:vertical resize 30<CR><C-w><C-w>
nnoremap <F12> <ESC>:echo "TODO: !~/bin/links"<CR><CR>
