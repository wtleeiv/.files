execute pathogen#infect()
syntax on
filetype plugin indent on
set nocompatible
set nomodeline
" kEY REMAPPINGS
nnoremap , :
nnoremap : ,
let mapleader = " "
" move by visual line
nnoremap j gj
nnoremap k gk
" Y : yank til EOL
nnoremap Y y$
" preserve visual indenting
vnoremap > >gv
vnoremap < <gv
" normal regexs
nnoremap / /\v
vnoremap / /\v
" use tab to match parens
nnoremap <tab> %
vnoremap <tab> %
" LEADER/PLUGIN SETTINGS
nnoremap <leader><space> :noh<cr>
nnoremap <leader>t :NERDTreeToggle<CR>
autocmd VimEnter * RainbowParentheses
" TABS
set tabstop=4
set shiftwidth=4
set softtabstop=4
augroup TabLengths
    autocmd FileType lisp,clojure setlocal tabstop = softtabstop = shiftwidth = 2
augroup END
set expandtab
" SEARCH
set hlsearch
set incsearch
set showmatch
set smartcase
set ignorecase

set encoding=utf-8

set scrolloff=3
set wildmenu
set wildmode=list:longest
set autoindent
set showmode
set showcmd
set visualbell
set cursorline
set lazyredraw
set ttyfast
set ruler
set relativenumber
set hidden
set undofile
set backspace=indent,eol,start
" always show modeline
set laststatus=2
" global search&replace by default
set gdefault
" LINE WRAPPING
set wrap
set textwidth=79
set formatoptions=qrn1
"set colorcolumn=85
