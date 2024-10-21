set nocompatible

syntax enable
filetype plugin on

set path+=**

set wildmenu

set rtp+=/opt/homebrew/opt/fzf

set number
set backspace=indent,eol,start  " more powerful backspacing
set listchars=tab:→\ ,eol:↲,nbsp:␣,space:·,trail:·,extends:⟩,precedes:⟨

set autowrite	      "automatically save file when switching apps
set noswapfile	      "no swapfile annoyances. When did your vim crashed last time?
set backup            "backup is a must though. Backup location see below.
set backupdir=.backup/,~/.backup/,/tmp//
set directory=.swp/,~/.swp/,/tmp//
set undodir=.undo/,~/.undo/,/tmp//

syntax on "vim can automatically highlight known textfile(code) formats

"hi Visual term=reverse cterm=reverse guibg=Grey

set showmatch	"highlights matching brackets on cursor hover
set ruler	"show cursor position in status bar
set showcmd	"shows the normal mode command before it gets executed

set encoding=utf-8
set fileformats=unix,dos,mac

set hlsearch		"highlights searches
set incsearch		"incremental search (searches character by character)
set ignorecase		"ignores the case of a search
set smartcase		"only ignores case if there are no capital letters in search
set linebreak		"prevents words from being split when wrapping.
set tabstop=4		"the amount of spaces that vim will equate to a tab character
set softtabstop=4	"like tabstop, but for editing operations (insert mode)
set shiftwidth=4	"used for autoindent and << and >> operators in normal mode
set autoindent		"copies indent from current line to the next line
set expandtab		"tabs will expand to whitespace characters
set smarttab		"adjusts the number of spaces when you press Tab based on the context.
set showbreak=↪\ 
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨

" allow syntax and filetype plugins
syntax enable
filetype plugin indent on
runtime macros/matchit.vim

" set text width before wrapping line
set textwidth=73

" seems like 999 represents that whenever you scroll cursor will always
" remain in the middle
set scrolloff=999

" delete format rules
set formatoptions-=oravb
" add format rules
set formatoptions+=1wctqjlmM

" autocmds
"---------
augroup general
    autocmd!
    "keep equal proportions when windows resized
    autocmd VimResized * wincmd =
    "save cursor position in a file
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"")
                \ <= line("$") | exe "normal! g'\"" | endif
augroup END

" Set formatting rules for specific file formats
augroup languages
    autocmd!
    autocmd BufNewFile,BufRead *.bash set syntax=sh
    autocmd FileType go set noexpandtab
    autocmd FileType python set ts=4 sts=4 sw=4 tw=79 expandtab autoindent fileformat=unix
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType html syntax sync fromstart
    autocmd FileType html,javascript,css,json,yaml,sh,ruby setlocal ts=2 sts=2 sw=2 expandtab
augroup END

augroup hl_whitespace
    autocmd!
    autocmd FileType python,c,cpp match BadWhitespace /\s\+$/
augroup END

" map highlight deselection to §(macbook) or ~(Linux) shortcut. It is annoying that highlight remains after the search and
" I need a shortcut to turn it off
nnoremap ~ :nohl<CR>

" Enable folding
set foldmethod=indent
set foldlevel=99
