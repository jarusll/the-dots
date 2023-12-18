" relative line numbers
set relativenumber

filetype plugin on

" highlight current line
set cursorline
" ignore case during search
set ignorecase

" disable wrapping
set textwidth=0
set wrapmargin=0
set wrap!

" Ctrl+s saves
nnoremap <silent><c-s> :<c-u>update<cr>

" load vimrc
nnoremap <F3> :e $MYVIMRC<CR>
" reload vimrc
map <leader><F3> :source $MYVIMRC<CR>
