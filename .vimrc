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

" call this to install vundle
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
" Provides language packs and indentation
Plugin 'sheerun/vim-polyglot'

" auto complete pairs
Plugin 'jiangmiao/auto-pairs'

" file system explorer
Plugin 'preservim/nerdtree'

" Tagbar 
Plugin 'preservim/tagbar'

" Git changes in gutter like vscode
Plugin 'airblade/vim-gitgutter'

" Use git in vim
Plugin 'tpope/vim-fugitive'

" fzf for fast file search
Plugin 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plugin 'junegunn/fzf.vim'

" automatically resize pane
Plugin 'roman/golden-ratio'

call vundle#end()         

" keybinds
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
" I dont know what this does
" nnoremap <C-f> :NERDTreeFind<CR>

" Ctrl+s saves
nnoremap <silent><c-s> :<c-u>update<cr>

" load my vimrc
nnoremap <F3> :e $MYVIMRC<CR>
map <leader><F3> :source $MYVIMRC<CR>
