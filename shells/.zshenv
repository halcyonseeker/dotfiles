#
# In its capacity as ~/.zshenv this file is source by zsh on both
# login and non-login shells and defines my entire environment.  I've
# done my best to keep the zsh-specific configs gated behind a
# conditional so that it might also be sourced by any shell that loads
# ~/.profile on login.
#


# These aren't always set by default
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
[ -z "$XDG_RUNTIME_DIR" ] && export XDG_RUNTIME_DIR="$XDG_CACHE_HOME"

# Set preferred applications for use in scripts.
EDITOR="vi"
command -v vim >/dev/null && EDITOR="vim"
command -v nvim >/dev/null && EDITOR="nvim"
export EDITOR
export VISUAL="$EDITOR"
export TERMINAL="xterm"
export PAGER="less"

# Tell emacsclient to run emacs --daemon if it can't connect
export ALTERNATE_EDITOR=""

# Set word-delimiters for cutting text with ^W
export WORDCHARS='*?_[]~=&;!#$%^(){}'

# Clean home directory
export PASSWORD_STORE_DIR="$HOME/.local/password-store"
export LESSHISTFILE="-"
export VIMINIT=":source $XDG_CONFIG_HOME/nvim/init.vim"
export XAUTHORITY="$XDG_RUNTIME_DIR/xauthority"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export GOPATH="$HOME/.local/go"
export MPV_HOME="$XDG_CONFIG_HOME/mpv"
export SQLITE_HISTORY="$XDG_CACHE_HOME/sqlite_history"
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export PATH="$HOME/.local/bin:$GOPATH/bin:$CARGO_HOME/bin:$PATH"

# General aliases
alias mv="mv -iv"
alias cp="cp -riv"
alias rm="rm -iv"
alias sl="ls"
alias mdkir="mkdir"
alias ec="emacsclient -nw -c"
alias se="sudoedit"
alias xo="xdg-open"

# Colorful aliases
if [ -x /usr/bin/dircolors ] ; then
	alias ls="ls -F --color=auto"
	alias grep="grep --color=auto"
	alias diff="diff --color=auto"
elif [ -x /usr/local/bin/colorls ] ; then
	alias ls="colorls -GF"
else
	alias ls="ls -GF"
fi

set -o emacs

# Keep zsh-specific stuff here just in case this file is being loaded
# by ksh, bash, or /bin/sh
if [ "$0" = "zsh" ] || [ "$0" = "-zsh" ]; then
	zstyle :compinstall filename '$HOME/.config/shrc'
	autoload -Uz compinit
	compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION

	# History behavior
	HISTFILE="$HOME"/.cache/zsh_histfile
	HISTSIZE=5000
	SAVEHIST=5000
	setopt appendhistory

	# Load plugins
	zsyntaxhl="zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
	if [ -f /usr/share/zsh/plugins/"$zsyntaxhl" ]; then
		source /usr/share/zsh/plugins/"$zsyntaxhl"
	elif [ -f /usr/share/"$zsyntaxhl" ]; then
		source /usr/share/"$zsyntaxhl"
	elif [ -f /usr/local/share/"$zsyntaxhl" ] ; then
		source /usr/local/share/"$zsyntaxhl"
	fi

	# Make keys work
	bindkey '^[[3~' delete-char
	bindkey '^[[5~' up-line-or-history
	bindkey '^[[6~' down-line-or-history
	bindkey '^[[H' beginning-of-line
	bindkey '^[[F' end-of-line

	# Set nice prompt
	if [ -n "$SSH_CONNECTION" ] ; then
		case "$EUID" in
			0) export PROMPT="%B%F{red}[%m: %~]%# %b%f" ;;
			*) export PROMPT="%B%F{green}[%m: %~]%# %b%f" ;;
		esac
	else
		case "$EUID" in
			0) export PROMPT="%B%F{red}[%~]%# %b%f" ;;
			*) export PROMPT="%B%F{green}[%~]%# %b%f" ;;
		esac
	fi

fi
