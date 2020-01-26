execute pathogen#infect()
syntax on
filetype plugin indent on

nnoremap , :
nnoremap : ,
let mapleader = " "
nnoremap j gj
nnoremap k gk
nnoremap Y y$
vmap > >gv
vmap < <gv

map <leader>t ,NERDTreeToggle<CR>

set number
set showcmd
set cursorline
set wildmenu
set lazyredraw
set showmatch
set incsearch
set hlsearch
set ignorecase
set smartcase
set noeb vb t_vb=
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
