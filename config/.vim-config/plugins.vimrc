call plug#begin()
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
"Plug 'NLKNguyen/papercolor-theme'
Plug 'avakhov/vim-yaml'
Plug 'hdima/python-syntax'
Plug 'luochen1990/rainbow'
"Plug 'rust-lang/rust.vim'
Plug 'Chiel92/vim-autoformat'
"Plug 'chriskempson/vim-tomorrow-theme'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
"Plug 'neomake/neomake'
"Plug 'jdkanani/vim-material-theme'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'Raimondi/delimitMate'
"Plug 'the-lambda-church/merlin'
"Plug 'OCamlPro/ocp-indent'
"Plug 'cespare/vim-toml'
Plug 'tomasr/molokai'
"Plug 'itchyny/lightline.vim'
"Plug 'tpope/vim-fireplace'
"Plug 'tpope/vim-sexp-mappings-for-regular-people'
"Plug 'guns/vim-clojure-static'
"Plug 'guns/vim-sexp'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'fatih/vim-go'
"Plug 'derekwyatt/vim-scala'
"Plug 'ensime/ensime-vim'
Plug 'christoomey/vim-tmux-navigator'
"Plug 'Shougo/neocomplete.vim'
Plug 'w0ng/vim-hybrid'
"Plug 'davidhalter/jedi-vim'
Plug 'maralla/completor.vim'
Plug 'w0rp/ale'
Plug 'tomlion/vim-solidity'
call plug#end()

let g:hybrid_custom_term_colors = 1

let python_highlight_all = 1
let g:vim_markdown_folding_disabled=1
let g:rainbow_active = 1

" --- autofmt plugin settings
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0
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

" python
let g:neomake_python_flake8_maker = {
    \ 'exe': 'python3.6',
    \ 'errorformat': '%A%f: line %l\, col %v\, %m \ (%t%*\d\)',
    \ }
let g:neomake_python_enable_makers = ['flake8', 'pep8', 'pylint']

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
  let fname = expand('%:t')
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
" Neocomplete settings
""""""""""""""

"let g:neocomplete#enable_at_startup = 1
"let g:neocomplete#enable_smart_case = 1
"
"inoremap <expr><C-g>     neocomplete#undo_completion()
"inoremap <expr><C-l>     neocomplete#complete_common_string()
"
"" <CR>: close popup and save indent.
"inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
"function! s:my_cr_function()
"  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
"endfunction
"
"" Tab completion (shift Tab for going backwards) for Neocomplete
"inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"
"
"" <Down> and <Up> cycle like <Tab> and <S-Tab>
"inoremap <expr> <Down>  pumvisible() ? "\<C-n>" : "\<Down>"
"inoremap <expr> <Up>    pumvisible() ? "\<C-p>" : "\<Up>"
"
"" <C-h>, <BS>: close popup and delete backword char.
"inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
"inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
"
"" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1

" Enable omni completion.
" autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
" autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
" autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
" autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

""""""""""""""
" Jedi Settings for Neocomplete
""""""""""""""
"autocmd FileType python setlocal omnifunc=jedi#completions
""""""""""""""
" Completor Settings
""""""""""""""
let g:completor_python_binary = '/usr/local/bin/python3.6'

" Use Tab to select completion
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

""""""""""""""
" ALE Settings
""""""""""""""
let b:ale_linters = ['pylint']

