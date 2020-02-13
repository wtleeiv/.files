execute pathogen#infect()
syntax on
filetype plugin indent on
set nocompatible
set nomodeline
" COLORSCHEME
set background=dark
colorscheme palenight
let g:airline_theme = "palenight"
" KEY REMAPPINGS
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
"airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tablne#formatter = 'unique_tail_improved'
" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_perl_checkers = ['perl']
let g:syntastic_enable_perl_checker = 1
" TABS
set tabstop=4
set shiftwidth=4
set softtabstop=4
augroup TabLengths
    autocmd FileType lisp,clojure setlocal tabstop=2 softtabstop=2 shiftwidth=2
augroup END
set expandtab
" SEARCH
set hlsearch
set incsearch
set showmatch
set smartcase
set ignorecase
" MISC
set encoding=utf-8
set scrolloff=3
set wildmenu
set wildmode=list:longest
set autoindent
set showmode
set showcmd
set noeb vb t_vb=
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
