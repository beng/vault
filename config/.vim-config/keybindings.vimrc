fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

function! NumberToggle()
  if(&relativenumber == 1)
    set noru
    set number
  else
    set rnu
    set nonumber
  endif
endfunc

let mapleader = ","

" Directory view
let g:netrw_liststyle=3
nnoremap <leader>d :Explore<cr>

" clear search highlighting
nnoremap <leader><space> :noh<cr>

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

" quick buffer switching
nmap <space>n :bn<cr>
nmap <space>p :bp<cr>

" quickly close current buffer
nmap <space>d :bd<cr>

" quick save
nmap <space>w :w<cr>

" quick vertical split
nnoremap <leader>wv <C-w>v<C-w>l

" quick horizontal split
noremap <leader>wh <C-w>s<C-w>l

" Switch out of relative line number
nnoremap <C-n> :call NumberToggle()<cr>

" Use ; for commands.
nnoremap ; :

nmap <Leader>f :Autoformat<CR>

" use leader to interact with the system clipboard
nnoremap <Leader>p "*p
nnoremap <Leader>P "*P

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

    " au FileType go nmap <leader>gr <Plug>(go-run)
    " au FileType go nmap <leader>gb <Plug>(go-build)
    " au FileType go nmap <leader>gt <Plug>(go-test)
    " au FileType go nmap <leader>gc <Plug>(go-coverage)
augroup END

