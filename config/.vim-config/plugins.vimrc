" auto installation of vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'NLKNguyen/papercolor-theme'
Plug 'avakhov/vim-yaml'
Plug 'hdima/python-syntax'
Plug 'luochen1990/rainbow'
Plug 'Chiel92/vim-autoformat'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'jdkanani/vim-material-theme'
Plug 'Raimondi/delimitMate'
Plug 'tomasr/molokai'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'fatih/vim-go'
Plug 'christoomey/vim-tmux-navigator'
Plug 'w0ng/vim-hybrid'
Plug 'maralla/completor.vim'
Plug 'w0rp/ale'
Plug 'tomlion/vim-solidity'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'arcticicestudio/nord-vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'
Plug 'hashivim/vim-terraform'
call plug#end()

let g:hybrid_custom_term_colors = 1
let python_highlight_all = 1
let g:vim_markdown_folding_disabled=1
let g:rainbow_active = 1

" --- autofmt plugin settings
let g:autoformat_autoindent = 1
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 1
let g:formatter_yapf_style = 'pep8'

" --- vim-go settings
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_disable_autoinstall = 1

" --- CtrlP settings
let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g ""'

" --- neomake settings
"autocmd! BufWritePost * Neomake

let g:lightline = {
            \ 'colorscheme': 'wombat',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark'] ],
            \   'right': [ [ 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
            \ },
            \ 'component_function': {
            \   'fugitive': 'LightLineFugitive',
            \   'filename': 'LightLineFilename',
            \   'fileformat': 'LightLineFileformat',
            \   'filetype': 'LightLineFiletype',
            \   'fileencoding': 'LightLineFileencoding',
            \   'mode': 'LightLineMode',
            \   'ctrlpmark': 'CtrlPMark',
            \ },
            \ 'subseparator': { 'left': '|', 'right': '|' }
            \ }

function! LightLineModified()
    return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightLineReadonly()
    return &ft !~? 'help' && &readonly ? 'RO' : ''
endfunction

function! LightLineFilename()
    let fname = expand('%:p')
    return fname == 'ControlP' && has_key(g:lightline, 'ctrlp_item') ? g:lightline.ctrlp_item :
                \ ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
                \ ('' != fname ? fname : '[No Name]') .
                \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endfunction

function! LightLineFugitive()
    try
        if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
            let mark = ''  " edit here for cool mark
            let branch = fugitive#head()
            return branch !=# '' ? mark.branch : ''
        endif
    catch
    endtry
    return ''
endfunction

function! LightLineFileformat()
    return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightLineFiletype()
    return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightLineFileencoding()
    return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightLineMode()
    let fname = expand('%:t')
    return fname == 'ControlP' ? 'CtrlP' :
                \ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

function! CtrlPMark()
    if expand('%:t') =~ 'ControlP' && has_key(g:lightline, 'ctrlp_item')
        call lightline#link('iR'[g:lightline.ctrlp_regex])
        return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                    \ , g:lightline.ctrlp_next], 0)
    else
        return ''
    endif
endfunction

let g:ctrlp_status_func = {
            \ 'main': 'CtrlPStatusFunc_1',
            \ 'prog': 'CtrlPStatusFunc_2',
            \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
    let g:lightline.ctrlp_regex = a:regex
    let g:lightline.ctrlp_prev = a:prev
    let g:lightline.ctrlp_item = a:item
    let g:lightline.ctrlp_next = a:next
    return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
    return lightline#statusline(0)
endfunction

""""""""""""""
" Jedi Settings for Neocomplete
""""""""""""""
"autocmd FileType python setlocal omnifunc=jedi#completions
"let g:jedi#show_call_signatures = "0"
""""""""""""""
" Completor Settings
""""""""""""""
"let g:completor_python_binary = '/Users/sesshin/.pyenv/shims/python'

" Use Tab to select completion
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

""""""""""""""
" ALE Settings
""""""""""""""
let b:ale_linters = ['pylint', 'mypy', 'autopep8']
" Fix Python files with autopep8 and yapf.
"let g:ale_linters = {'rust': ['rls']}
"let g:ale_rust_cargo_use_check = 1
"let g:ale_rust_rls_toolchain = '/Users/sesshin/.cargo/bin/rls'

"""
" Rust settings
"""
set hidden
let g:racer_cmd="/Users/sesshin/.cargo/bin/racer"
let g:racer_experimental_completer = 1

let g:autofmt_autosave = 1
let g:rustfmt_autosave = 1

au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

"""
" Gutentags
"""

" enable gtags module
let g:gutentags_modules = ['ctags']

" config project root markers.
let g:gutentags_project_root = ['.root']

" generate datebases in my cache directory, prevent gtags files polluting my project
"let g:gutentags_trace=1
let g:gutentags_cache_dir = expand('~/.cache/tags')
augroup MyGutentagsStatusLineRefresher
    autocmd!
    autocmd User GutentagsUpdating call lightline#update()
    autocmd User GutentagsUpdated call lightline#update()
augroup END
" forbid gutentags adding gtags databases
"let g:gutentags_auto_add_gtags_cscope = 0


""""""""""""""
" ensime config for scala
""""""""""""""

" typechecking after writing
"autocmd BufWritePost *.scala silent :EnTypeCheck

" easy type inspection
"nnoremap <localleader>t :EnType<CR>

" terraform plugin
let g:terraform_align=1
