"Mario Gajardo Tassara
"MarioGT Software
"https://www.mariogt.com
"mario@mariogt.com

set nocompatible

call plug#begin('~/.vim/plugged')
"formating
Plug 'junegunn/vim-easy-align'
Plug 'preservim/nerdcommenter'
Plug 'https://github.com/preservim/nerdtree'
Plug 'https://github.com/Townk/vim-autoclose'
"fuzzy search
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
"git
Plug 'https://github.com/junegunn/vim-github-dashboard.git'
Plug 'https://github.com/tpope/vim-fugitive'
"language support
Plug 'https://github.com/mityu/vim-applescript'
Plug 'https://github.com/eraserhd/vim-ios/'
Plug 'https://github.com/keith/swift.vim'
Plug 'https://github.com/xavierd/clang_complete'
"ctags
Plug 'https://github.com/vim-scripts/taglist.vim'
Plug 'https://github.com/craigemery/vim-autotag'
"snippets
Plug 'https://github.com/honza/vim-snippets'
Plug 'https://github.com/vim-scripts/AutoComplPop'
"gui & colorscheme
Plug 'https://github.com/ryanoasis/vim-devicons'
Plug 'https://github.com/altercation/vim-colors-solarized'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'https://github.com/AlessandroYorba/Alduin'
Plug 'https://github.com/gruvbox-community/gruvbox.git'
"Plug 'jooize/vim-colemak'

call plug#end()

"change the Leader key from \ to ,
let mapleader = ","

syntax enable
filetype on
filetype plugin on
filetype indent on

"set copy to mac clipboard
set clipboard=unnamed

set encoding=UTF-8
set visualbell
set nocompatible
set showfulltag
set showcmd
set showmode
set laststatus=2
set statusline=%t       "tail of the filename - F = fullpath
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file
set mouse=a
set history=500
set undolevels=500
set ai
set vb
set cindent
set backspace=indent,eol,start
set autoread
set hidden
set splitright "splitbelow
set lazyredraw
"set cul!
set number
set relativenumber
set ruler
set showmatch
set colorcolumn=80
"set wrap
set linebreak
set wildmenu

" ================ Completion =======================
set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*DS_Store*
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

"INDENTATION / TAB
set shiftwidth=4 "> and < move 4 spaces
set sts=4
set ts=4
set autoindent

"COLOR & UI
let g:dracula_italic = 0
set background=dark
colorscheme Gruvbox
set guifont=Hack:h14

"show invisibles on nerd tree
let NERDTreeShowHidden=1

"search
set path+=**
set hlsearch
set incsearch
set ignorecase
set smartcase

"fzf fuzzy search
map ; :Files<CR>
map <Leader>; :Buffer<CR>glist"
map <Leader>/ :Tags<CR>
map <Leader>. :BTags<CR>

"folding
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

"remaps
"folding
nnoremap <space> za

"formating & indent
nnoremap <F11> =G

nnoremap <silent> <F5> :Tlist<CR>           "taglist"
nnoremap <silent> <F6> :qa<CR>              "quit all"
nnoremap <silent> <F7> :w<CR>               "save"
nnoremap <silent> <F8> :wa<CR>              "save all"

inoremap <F9> <Esc>:NERDTreeToggle<CR>
inoremap <F10> <Esc>:noh<CR>                "no highlight"
noremap <F9> <Esc>:NERDTreeToggle<CR>
noremap <F10> <Esc>:nohlsearch<CR>

"remap for spacing lines
nnoremap <Leader>i O<ESC>k
nnoremap <Leader>o o<ESC>j

"remap split windows navigation
map <C-h> :wincmd h<CR>
map <C-k> :wincmd j<CR>
map <C-j> :wincmd k<CR>
map <C-l> :wincmd l<CR>

"Buffer switching
"F1 = next buf F2 = prev buf F3 = close buf
inoremap <F1> <ESC>:bp<CR>
inoremap <F2> <ESC>:bn<CR>
inoremap <F3> <ESC>:bd<CR>
noremap <F1> <ESC>:bp<CR>
noremap <F2> <ESC>:bn<CR>
noremap <F3> <ESC>:bd<CR>

"splits resize
nnoremap <silent> ) : vertical resize -5<cr>
nnoremap <silent> ( : vertical resize +5<cr>
nnoremap <silent><Leader> ñ : resize -5<cr>
nnoremap <silent> + : resize +5<cr>

"next or previous buffer
noremap <Leader><TAB>   :bn<CR>
noremap <Leader><S-TAB> :bp<CR>

"reload vimrc when save
augroup VimConfig
	au!
	autocmd BufWritePost ~/.vimrc so ~/.vimrc
augroup END

"programming setups
"ctags optimization
set autochdir
set tags+=tags;$HOME

let c_gnu=1
let c_comment_strings=1
"LET C_space_errors=1
"ctags for objective-c
let tlist_objc_settings = 'ObjectiveC;P:protocols;i:interface;c:class;m:method;p:property'
:nmap <leader>ct :!ctags . <CR> "build ctags for the current directory

"GDB setup
let g:termdebug_popup = 0
let g:termdebug_wide = 140

"syntastic prefs
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

"uncomment this if you want to disable objc and objc++ support
"because syntastic hangs in large objc files
"let g:syntastic_mode_map = { 'passive_filetypes': ['objc', 'objcpp'] }

"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_enable_signs=1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0

"objc
"let g:syntastic_objc_compiler = 'llvm'
"let g:syntastic_objc_remove_include_errors = 0
"let g:syntastic_objc_check_header = 0
"let g:syntastic_objc_errorformat = '%f:%l:%c: %trror: %m'

"clang complete
let g:clang_exec='/opt/homebrew/Cellar/llvm/12.0.0_1/bin/clang'
let g:clang_library_path='/opt/homebrew/Cellar/llvm/12.0.0_1/lib/libclang.dylib'
