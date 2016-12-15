set visualbell
set shiftwidth=4
set tabstop=4
set expandtab
set relativenumber
set clipboard=unnamed
colorscheme hybrid
set background=dark
set autoindent
set t_Co=256
set nocompatible
set undofile
set encoding=utf-8
set fileencoding=utf-8
scriptencoding utf-8
set noshowmode
set wildmenu
set wildmode=list:longest
set ruler
set statusline=%F
set formatoptions=qrn1
set colorcolumn=85

" saner regex
set ignorecase
set smartcase
set gdefault

" highlight search results when typing
set incsearch
set showmatch
set hlsearch

" causes unsaved buffer to be hidden, not closed
set hidden

" keep cursor towards center when scrolling
set scrolloff=3

" faster scrolling
set ttyfast
set lazyredraw

" syntax on
filetype plugin indent on

" let backspace work in insert mode
set backspace=indent,eol,start
