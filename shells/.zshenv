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

# Tell audible-dl to keep everything in one place
export AUDIBLE_DL_ROOT="$HOME/media/audiobooks/audible"

# Clean home directory
export PASSWORD_STORE_DIR="$HOME/secrets/password-store"
export LESSHISTFILE="-"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export GOPATH="$HOME/.local/go"
export MPV_HOME="$XDG_CONFIG_HOME/mpv"
export SQLITE_HISTORY="$XDG_CACHE_HOME/sqlite_history"
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export PATH="$HOME/.local/bin:$GOPATH/bin:$CARGO_HOME/bin:$PATH"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"

# Almost 20 years and still no thumbnail view in the GTK file picker…
export GTK_USE_PORTAL=1

# General aliases
alias mv="mv -iv"
alias cp="cp -riv"
alias rm="rm -iv"
alias sl="ls"
alias mdkir="mkdir"
alias ec="emacsclient -nw -c"
alias se="sudoedit"
alias xo="xdg-open"
alias yta="yt-dlp -x --audio-format opus --audio-quality 0 --embed-metadata --embed-thumbnail --sponsorblock-mark all"
alias ytv="yt-dlp --merge-output-format mkv --audio-quality 0 --embed-metadata --embed-thumbnail --embed-subs --convert-thumbnails jpg --sponsorblock-mark all"


# Colorful aliases
if [ -x /usr/bin/dircolors ] ; then
	alias ls="ls -F --color=auto"
	alias grep="grep --color=auto"
	alias egrep="egrep --color=auto"
	alias diff="diff --color=auto"
elif [ -x /usr/local/bin/colorls ] ; then
	alias ls="colorls -GF"
else
	alias ls="ls -GF"
fi

set -o emacs

# Keep zsh-specific stuff here just in case this file is being loaded
# by ksh, bash, or /bin/sh
if [ "$0" = "zsh" ] || [ "$0" = "-zsh" ] || [ "$(basename $0)" = "zsh" ]; then
	mkdir -p "$XDG_CACHE_HOME/zsh"
	zstyle :compinstall filename "$HOME/.zshenv"
	zstyle ':completion:*' menu select
	autoload -Uz compinit
	autoload -Uz add-zsh-hook
	autoload -Uz edit-command-line
	compinit -d "$XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION"

	# History behavior
	HISTFILE="$XDG_CACHE_HOME/zsh/histfile"
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

	# Edit the current command in $EDITOR
	zle -N edit-command-line
	bindkey '^X^E' edit-command-line

	yank-to-x-clipboard() {
		zle copy-region-as-kill
		print -Rn $CUTBUFFER | xclip -selection clipboard
	}
	zle -N yank-to-x-clipboard
	bindkey '^X^Y' yank-to-x-clipboard

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

	# Set a simple window title
	function xterm_title_precmd  () { print -Pn -- '\e]2;zsh\a' }
	function xterm_title_preexec () { print -Pn -- '\e]2;' && print -n -- "${(q)1}\a" }
	add-zsh-hook -Uz precmd  xterm_title_precmd
	add-zsh-hook -Uz preexec xterm_title_preexec
fi
