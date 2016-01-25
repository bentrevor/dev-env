call pathogen#infect()


""" settings """
""""""""""""""""
filetype on
filetype plugin on
filetype indent on
set nocompatible

set encoding=utf8
set termencoding=utf8

if &term == 'xterm' || &term == 'screen'
  set t_Co=256
endif

syntax on
set background=dark
color tomorrownight

set nobackup
set nowb
set noswapfile

set cmdheight=2 " Height of the command bar
set so=3 " keep three lines between cursor and top/bottom of window
set history=5000
set hid " A buffer becomes hidden when it is abandoned
set number " line numbers
set cursorline " highlight current line and line number
set expandtab " spaces instead of tabs
set showmatch " Show matching brackets when text indicator is over them
set showcmd " Show commands as they're typed
set mat=2 " How many tenths of a second to blink when matching brackets
set foldmethod=manual " don't fold text automatically
set nowrap " <leader>w to toggle
set laststatus=2 " Always show the status line

" when a search pattern is all lowercase, it is case-insensitive
" when a search pattern contains a capital, it is case-sensitive
set ignorecase
set smartcase
set hlsearch " highlight search results
set incsearch " execute search after each character is typed

" Backspace can delete newlines
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" unlimited characters per line
set lbr
set tw=0

" default to two spaces indentation
set tabstop=2
set shiftwidth=2

" filetype-specific indentation
autocmd FileType clojure,ruby,css,html setlocal ts=2 sts=2 sw=2
autocmd FileType python,java,javascript setlocal ts=4 sts=4 sw=4

" filetype settings
autocmd FileType clojure setlocal lispwords+=describe,it,context,around
autocmd FileType clojure setlocal wildignore+=target/**/*
autocmd BufNewFile,BufRead *.hiccup set filetype=clojure
autocmd BufNewFile,BufRead *.cljs set filetype=clojure
autocmd BufNewFile,BufRead *.ejs set filetype=eruby.html
autocmd BufNewFile,BufRead *.erb set filetype=eruby.html
autocmd BufNewFile,BufRead *.json set filetype=javascript

" config files with ruby syntax
autocmd BufEnter,BufNewFile,BufRead Vagrantfile,Berksfile,Gemfile set filetype=ruby

" pressing tab in command shows options
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,node_modules,tmp,coverage
if has("win16") || has("win32")
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
else
    set wildignore+=.git\*,.hg\*,.svn\*
endif

" highlight trailing whitespace
" (this got annoying)
" autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
" autocmd BufRead,InsertLeave * match ExtraWhitespace /\s\+$/

" " set up highlight group & retain through colorscheme changes
highlight ExtraWhitespace ctermbg=red guibg=red
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
" map <silent> <LocalLeader>ws :highlight clear ExtraWhitespace<CR>




""" plugin stuff """
""""""""""""""""""""
" ignore patterns for nerdtree
let NERDTreeIgnore=['\~$', 'target', 'repl$', 'out$', 'bin$', 'tmp']

" open nerd tree
map <leader>nt :NERDTreeToggle<cr>

" resize nerd tree
map <leader>ns :NERDTreeFocus<cr>:vert<space>res<space>32<cr>

let g:CommandTMaxFiles=40000
map <leader>t :CommandTFlush<CR>:CommandT<CR>

let vimclojure#FuzzyIndent=1
let vimclojure#ParenRainbow=1

" always use my rainbow parens for lisp
autocmd FileType clojure,scheme RainbowParenthesesLoadRound
autocmd FileType clojure,scheme RainbowParenthesesActivate



""" mappings """
""""""""""""""""
nnoremap ; :

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Remap x to delete instead of cut
noremap x "_dl

" set * to not automatically jump to next word
nnoremap * ma*`a

" unmap ex mode, F1 help
nnoremap Q <nop>
nnoremap <C-a> <nop>
nmap <F1> <nop>
imap <F1> <nop>

" \e evals lisp code with fireplace.vim
map <leader>e :Eval<cr>

" \ws resizes the current window to be about half of my terminal's width
map <leader>ws :vert<space>res<space>122<cr>

" \ss shows the current syntax highlighting group
map <leader>ss :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<cr>

" comment
" \cr ruby
nnoremap <leader>cr maI#<space><esc>$`a
vnoremap <leader>cr 0I#<space><esc>
" \cj javascript
nnoremap <leader>cj maI//<esc>$`a
vnoremap <leader>cj 0I//<esc>
" \ch haskell
nnoremap <leader>ch maI--<esc>$`a
vnoremap <leader>ch 0I--<esc>
" \cc clojure
nnoremap <leader>cc maI;<space><esc>$`a
vnoremap <leader>cc 0I;<space><esc>

" delete trailing whitespace
nmap <leader><leader>w :%s/\s\+$//<cr>ma/<up><up><cr>:noh<cr>`a



""" settings mappings """
"""""""""""""""""""""""""
" <alt-w> toggles word wrap
map ∑ :set invwrap<CR>:set wrap?<CR>

" <alt-v> reloads vimrc
map √ :so $MYVIMRC<cr>

" <alt-=> aligns entire document
map ≠ mhggVG=`hzz

" <alt-r> reloads document
map ® :e<cr>zz

" <alt-p> toggles paste mode
map π :set invpaste<CR>:set paste?<CR>


