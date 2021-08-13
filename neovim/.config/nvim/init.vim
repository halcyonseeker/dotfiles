" Use syntax highlighting
syntax enable
filetype plugin indent on

" Move around with a mouse
set mouse=a

" Stop vim from crapping in ~/.vim, and prevent vim and neovim from 
" arguing about iNvAliD ShAda fIlEs
if !has('nvim')
	set undodir=$XDG_DATA_HOME/vim/undo
	set directory=$XDG_DATA_HOME/vim/swap
	set backupdir=$XDG_DATA_HOME/vim/backup
	set viewdir=$XDG_DATA_HOME/vim/view
	set viminfo+='1000,n$XDG_DATA_HOME/vim/viminfo
	set runtimepath=$XDG_CONFIG_HOME/nvim,$VIMRUNTIME,$XDG_CONFIG_HOME/nvim/after
	let &t_SI = "\e[6 q"
	let &t_EI = "\e[2 q"
endif

" Indent with 8-column tabs
set noexpandtab
set tabstop=8

" 72 column line wrap in text modes
autocmd FileType text setlocal textwidth=72
