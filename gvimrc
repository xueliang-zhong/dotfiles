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
command! Gdiff   :vertical terminal git diff HEAD
command! Gblame  :terminal git blame %
command! Gcommit :terminal git commit -m "update %s"
command! Glog    :terminal git log
command! Gshow   :terminal git show

command! Smallwindow  set nonu norelativenumber laststatus=0 nocursorline noruler colorcolumn=0
command! Bigwindow    set   nu norelativenumber laststatus=2   cursorline ruler   colorcolumn=100

command! XueliangCdGitRoot cd `git rev-parse --show-toplevel`
command! XueliangTABTrailingSpaces retab | %s/\s\+$//e | noh

" Get some nice syntax highlighting
autocmd BufRead *.log set filetype=asm
autocmd BufRead *.txt set filetype=asm
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

nnoremap <C-x><C-o> :call DeleteBlankLine()<CR>
inoremap <C-x><C-o> :call DeleteBlankLine()<CR>

" My fuzzy search in /
cnoremap <expr> <Space> getcmdtype() == '/' ? '.*' : ' '

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Windows GVIM Specific
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    " GUI SETTINGS
    set guifont=JetBrains\ Mono\ NL:h11
    set guioptions-=T

    " KEYS
    nnoremap <F8>  <ESC>:grep "^ \*" %<CR>:copen<CR>
    nnoremap <F11> <ESC>:call OpenUrlWithExplorer()<CR>
    nnoremap <F12> <ESC>:sp ~/workspace/org-notes/xzhong-links.txt<CR><ESC>/

    " AUTOCMD & COMMANDS
    autocmd BufEnter xzhong-links.txt nnoremap <CR> :call OpenUrlWithExplorer()<CR>
    command! MyWorkSpace call MyWorkSpace()
    command! MyDailyWebsite call MyDailyWebsite()
endif

function! OpenUrlWithExplorer()
  " Get the current line
  let line = getline('.')
  " Extract URL using a regex
  let match = matchstr(line, 'https\?://[^\s]*')
  " If a URL is found, open it in the default browser
  if !empty(match)
    execute 'terminal explorer.exe ' . shellescape(match)
  else
    echo "No URL found on this line."
  endif
endfunction

function! MyWorkSpace()
  execute 'only'
  " Open a vertical split with the specified file
  execute 'vs ~/workspace/org-notes/'
  execute 'sp ~/.vimrc'
  " Move to the first window
  execute 'wincmd w'
  execute 'vi ~/workspace/org-notes/vim-work-2025.org'
  execute 'set filetype=asm'
endfunction

function! MyDailyWebsite()
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/owa/'
  execute 'terminal explorer.exe ' . 'https://mail.google.com/mail/u/0/#inbox'
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/owa/'
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/calendar/'
  execute 'terminal explorer.exe ' . 'https://arm-ce.slack.com/messages/'
  execute 'terminal explorer.exe ' . 'https://confluence.arm.com/display/~xuezho01/GPU+COMPUTE+TRASH+PANDA'
  execute 'terminal explorer.exe ' . 'https://outlook.office.com/calendar/group/arm.com/unitedstatesofcompute/view/workweek'
  execute 'terminal explorer.exe ' . 'https://talent.arm.com'
  execute 'terminal explorer.exe ' . 'https://confluence.arm.com/plugins/inlinetasks/mytasks.action'
  call MyWorkSpace()
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => My Org Mode
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <ESC> :noh<CR><ESC>
inoremap <ESC> <ESC>:noh<CR><ESC>
autocmd BufRead,BufEnter *.org set filetype=diff

nnoremap <S-Left>  <ESC>:call ToggleDiffLine()<CR>
nnoremap <S-Right> <ESC>:call ToggleDiffLine()<CR>
inoremap <S-Left>  <ESC>l<ESC>:call ToggleDiffLine()<CR>
inoremap <S-Right> <ESC>l<ESC>:call ToggleDiffLine()<CR>

" Fold
" <Tab> in normal mode will toggle folds, similar to emacs org-mode.
" - create fold: <tab> in visual mode (vip zf)
" - delete fold: zd
" - toggle fold: <Tab> or <Shift-Tab> (za)
vnoremap <Tab> :fold<CR>
nnoremap <Tab> za
nnoremap <S-Tab> :let &foldlevel = (&foldlevel == 0 ? 99 : 0)<CR>

" Define the function to toggle the first character
function! ToggleDiffLine()
  let line = getline(".")
  " If the first character is '-' or '+', toggle it
  if line[0] == '-' || line[0] == '+'
    let newchar = line[0] == '-' ? '+' : '-'
    call setline('.', newchar . line[1:])
  " If the first character is '*', prepend another '*'
  elseif line[0] == '*'
    call setline('.', '*' . line)
  endif
endfunction

function! DeleteBlankLine()
  " Check if the current line is blank & delete
  let line = getline(".")
  if line =~ '^\s*$'
    execute "normal! dd"
  endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => My Snippets
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NOTE: , as prefix for all my snippets
" Create org structure and fold it
nnoremap ,o <ESC>o<ESC>:read ~/workspace/dotfiles/org-snippets.txt<CR>vip:fold<CR>

