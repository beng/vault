fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

call plug#begin()
Plug 'fatih/vim-go'
" Plug 'Valloric/YouCompleteMe'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'NLKNguyen/papercolor-theme'
Plug 'avakhov/vim-yaml'
Plug 'hdima/python-syntax'
Plug 'luochen1990/rainbow'
Plug 'rust-lang/rust.vim'
Plug 'Chiel92/vim-autoformat'
Plug 'scwood/vim-hybrid'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'neomake/neomake'
Plug 'jdkanani/vim-material-theme'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-go', { 'do': 'make'}
call plug#end()

let g:hybrid_custom_term_colors = 1

set visualbell
set shiftwidth=4
set tabstop=4
set expandtab
set relativenumber
set clipboard=unnamed
colorscheme Tomorrow-Night-Eighties
set background=dark
set autoindent
"set t_Co=256
set nocompatible
set undofile
set encoding=utf-8
set fileencoding=utf-8
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

let mapleader = ","

" Directory view
let g:netrw_liststyle=3
nnoremap <leader>d :Explore<cr>

" clear search highlighting
nnoremap <leader><space> :noh<cr>

" let backspace work in insert mode
set backspace=indent,eol,start

" causes unsaved buffer to be hidden, not closed
set hidden

" keep cursor towards center when scrolling
set scrolloff=3

" faster scrolling
set ttyfast
set lazyredraw

" syntax on
filetype plugin indent on

" --- various remaps
" match bracket pairs
nnoremap <tab> %
vnoremap <tab> %

imap jj <Esc>
nnoremap <C-j> <C-W><C-j>
nnoremap <C-k> <C-W><C-k>
nnoremap <C-l> <C-W><C-l>
" nnoremap <C-h> <C-W><C-h>
" bs workaround for C-h not working
nmap <BS> <C-W>h

" quick vertical split
nnoremap <leader>wv <C-w>v<C-w>l

" quick horizontal split
noremap <leader>wh <C-w>s<C-w>l

" Switch out of relative line number
nnoremap <C-n> :call NumberToggle()<cr>

nmap <Leader>f :Autoformat<CR>

" run py tests
nmap <leader>pr :w <bar> !py.test tests -s<CR>
augroup vimrc_autocmd
    autocmd!
    " Use absolute line numbers when in insert mode
    autocmd InsertEnter * :set number
    autocmd InsertLeave * :set relativenumber
    autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
    autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

    " autofmt on save
    au BufWrite * :Autoformat

    " save when vim loses focus
    autocmd FocusLost * :wa

    au FileType go nmap <leader>r <Plug>(go-run)
    au FileType go nmap <leader>b <Plug>(go-build)
    au FileType go nmap <leader>t <Plug>(go-test)
    au FileType go nmap <leader>c <Plug>(go-coverage)
augroup END

let python_highlight_all = 1
let g:vim_markdown_folding_disabled=1
let g:rainbow_active = 1

" --- autofmt plugin settings
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

" --- ycm settings
" let g:ycm_autoclose_preview_window_after_completion = 1
"
" " rust settings
" let g:ycm_rust_src_path = '/Users/goro/Documents/personal/rustc-1.8.0/src'
" let g:formatdef_rustfmt = '"rustfmt"'
" let g:formatters_rust = ['rustfmt']
" let g:rustfmt_autosave = 1

" --- vim-go settings
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_disable_autoinstall = 1

" CtrlP settings
let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g ""'

" --- neomake settings
autocmd! BufWritePost * Neomake

" python
let g:neomake_python_flake8_maker = {
    \ 'exe': 'python3.4',
    \ 'errorformat': '%A%f: line %l\, col %v\, %m \ (%t%*\d\)',
    \ }
let g:neomake_python_enable_makers = ['flake8', 'pep8', 'pylint']

" --- deoplete settings

" Path to python interpreter for neovim
let g:python3_host_prog  = '/usr/local/bin/python3.5'

" Skip the check of neovim module
let g:python3_host_skip_check = 1
let g:deoplete#enable_at_startup = 1
"
" deoplete-go settings
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
