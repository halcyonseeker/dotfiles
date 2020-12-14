#
# General Bourne Shell Profile.
#
# This file functions as ~/.profile, ~/.zprofile, or ~/.bash_profile
# depending on the name of the symlink and the value of $SHELL
#

# Set editor in order of preference
export EDITOR="vi"
which vim >/dev/null 2>&1 && export EDITOR="vim"
which nvim >/dev/null 2>&1 && export EDITOR="nvim"
export VISUAL="$EDITOR"

# Set terminal emulator in order of preference
export TERMINAL="xterm"
which st >/dev/null 2>&1 && export TERMINAL="st"

# Important variables
export PATH="$HOME/.local/bin:$GOPATH/bin:$PATH"
export WORDCHARS='*?_[]~=&;!#$%^(){}'

# Set a simple, portable prompt
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

# GUI variables
export GTK_THEME=Adwaita:dark
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"

# History settings
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
