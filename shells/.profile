#
# General Bourne Shell Profile.
#
# This file functions as ~/.profile, ~/.zprofile, or ~/.bash_profile
# depending on the name of the symlink and the value of $SHELL
#

# Set preferred applications for use in scripts. With the exception of
# $EDITOR and $VISUAL, none of thses variables are respected.  Other
# programs use the XDG specification to determine default applications.
# The XDG default application handler should reflect the preferences
# defined here.
export EDITOR="vi"
which vim >/dev/null 2>&1 && export EDITOR="vim"
which nvim >/dev/null 2>&1 && export EDITOR="nvim"
export VISUAL="$EDITOR"

export TERMINAL="xterm"

export FILE="$(xdg-mime query default inode/directory | tr -d '.desktop')"
which nnn >/dev/null 2>&1 && export FILE="nnn"
which ranger >/dev/null 2>&1 && export FILE="ranger"

export PAGER="less"

# Tell emacsclient to run emacs --daemon if it can't connect
export ALTERNATE_EDITOR=""

# Set word-delimiters for cutting text with ^W
export WORDCHARS='*?_[]~=&;!#$%^(){}'

export PATH="$HOME/.local/bin:$GOPATH/bin:$PATH"

# Set a stupid-simple prompt. Ksh, Zsh, and Bash will override this.
if [ -n "$SSH_CONNECTION" ] ; then
	case "$UID" in
		0) export PS1='[$(hostname): $(basename $PWD)]# ' ;;
		*) export PS1='[$(hostname): $(basename $PWD)]$ ' ;;
	esac
else
	case "$UID" in
		0) export PS1='[$(basename $PWD)]# ' ;;
		*) export PS1='[$(basename $PWD)]$ ' ;;
	esac
fi

# These aren't always set by default
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
[ -z "$XDG_RUNTIME_DIR" ] && export XDG_RUNTIME_DIR="$XDG_CACHE_HOME"

# Clean home directory
export PASSWORD_STORE_DIR="$HOME/.local/password-store"
export LESSHISTFILE="-"
export VIMINIT=":source $XDG_CONFIG_HOME/nvim/init.vim"
export XAUTHORITY="$XDG_RUNTIME_DIR/xauthority"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export GOPATH="$HOME/.local/go"
export MPV_HOME="$XDG_CONFIG_HOME/mpv"
export FVWM_USERDIR="$XDG_CONFIG_HOME/fvwm"
export FVWM_DATADIR="$XDG_DATA_HOME/fvwm"
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"

# Customize GTK and QT
export GTK_THEME="Raleigh-Reloaded"
export GTK_OVERLAY_SCROLLING=0

# Basic history settings. Zsh will override this.
export HISTFILE="$XDG_CACHE_HOME/sh_history"
export HISTSIZE=5000

# NNN config
which trash >/dev/null 2>&1 && export NNN_TRASH=1

# Make some directories
mkdir -p "$XDG_CACHE_HOME"/zsh >/dev/null 2>&1
mkdir -p "$XDG_DATA_HOME"/vim/undo >/dev/null 2>&1
mkdir -p "$XDG_DATA_HOME"/vim/swap >/dev/null 2>&1
mkdir -p "$XDG_DATA_HOME"/vim/backup >/dev/null 2>&1
mkdir -p "$XDG_CONFIG_HOME"/emacs/backup/ >/dev/null 2>&1
mkdir -p "$XDG_CONFIG_HOME"/emacs/org-timestamps/ >/dev/null 2>&1

# Do shell specific things with the rc file
case "$(basename $SHELL)" in
	"bash") export BASH_ENV="$HOME"/.bashrc && source "$BASH_ENV" ;;
	"zsh") source "$HOME"/.zshrc ;;
	*) export ENV="$HOME/.shrc" && sh "$ENV" ;;
esac
