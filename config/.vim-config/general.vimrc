set visualbell
set shiftwidth=4
set tabstop=4
set expandtab
set clipboard=unnamed
colorscheme PaperColor
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

" add src path for go so i can jump to files and imports with `gf`
set path+=$GOPATH/src
set laststatus=2

" Syntax coloring lines that are too long just slows down the world
set synmaxcol=128
set number

"netrw (Explore) customization
"let g:netrw_banner = 0
"let g:netrw_liststyle = 3
"let g:netrw_browse_split = 4
"let g:netrw_altv = 1
"let g:netrw_winsize = 25
"augroup ProjectDrawer
"  autocmd!
"  autocmd VimEnter * :Vexplore
"augroup END
"
set path+=**
