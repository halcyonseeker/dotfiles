#
# General (posix-compliant) shell profile
# Set up a nice environment with portable variable and aliases.
#
# Aliases don't persist post-login, so I source this file from a relevant rc
# file (eg, ~/.bashrc, ~/.zshrc, ~/.kshrc). Those files may additionally
# contain shell-specific configurations. Zsh doesn't source ~/.profile, so
# the symlink to this file may need to be renamed to ~/.zprofile.
#

# Use emacs editing mode
set -o emacs

# Set terminal and editor
export TERMINAL="xterm"
export EDITOR="vi"
which st >/dev/null 2>&1 && export TERMINAL="st"
which vim >/dev/null 2>&1 && export EDITOR="vim"
export VISUAL="$EDITOR"

# Important variables
export PATH="$HOME/.local/bin:$GOPATH/bin:$PATH"
export WORDCHARS='*?_[]~=&;!#$%^(){}'
export color_host="$(print -n "\E[32;1m`hostname`\E[0m")"
case "$UID" in 
	#0) export PS1='[$(basename $PWD)]# ' ;;
	#*) export PS1='[$(basename $PWD)]$ ' ;; 
	0) export PS1='[$color_host: $(basename $PWD)]# ' ;;
	*) export PS1='[$color_host: $(basename $PWD)]$ ' ;;
esac

# These aren't always set by default
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# Clean home directory
export PASSWORD_STORE_DIR="$HOME/.local/password-store"
export LESSHISTFILE="-"
# This must be commented unless link is ~/.zprofile to avoid chicken-and-egg
#export ZDOTDIR="$HOME/.config/zsh"
export VIMINIT=":source $XDG_CONFIG_HOME/vim/vimrc"
export XAUTHORITY="$XDG_RUNTIME_DIR/xauthority"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export GOPATH="$HOME/.local/go"
# Because wm4 is a skidmark
export MPV_HOME="$XDG_CONFIG_HOME/mpv"
export HISTFILE="$XDG_CACHE_HOME/sh_history"
export HISTSIZE=5000

# NNN config
# This requires trash-cli to be installed
which trash >/dev/null 2>&1 && export NNN_TRASH=1

# Make some directories
[ -f "$XDG_CACHE_HOME"/zsh/histfile ] || mkdir -p "$XDG_CACHE_HOME"/zsh
[ -d "$XDG_DATA_HOME"/vim ] || mkdir -p "$XDG_DATA_HOME"/vim/undo
[ -d "$XDG_DATA_HOME"/vim ] || mkdir -p "$XDG_DATA_HOME"/vim/swap
[ -d "$XDG_DATA_HOME"/vim ] || mkdir -p "$XDG_DATA_HOME"/vim/backup
[ -d "$HOME"/.emacs.d/backup/ ] || mkdir -p "$HOME"/.emacs.d/backup/
[ -d "$HOME"/.emacs.d/org-timestamps/ ] || mkdir -p "$HOME"/.emacs.d/org-timestamps/

# Aliases
if [ -x /usr/bin/dircolors ] ; then
	alias ls="ls -F --color=auto"
	alias grep="grep --color=auto" 
	alias diff="diff --color=auto"
elif [ -x /usr/local/bin/colorls ] ; then
	alias ls="colorls -GF"
fi
alias mv="mv -iv"
alias cp="cp -riv"
alias rm="rm -iv"
alias sl="ls"
alias mdkir="mkdir"
alias scp="scp -r"
alias untar="tar -xzvf"
alias lynx="lynx -vikeys"
alias yta="youtube-dl --add-metadata -i -x -f bestaudio/best"
alias ec="emacsclient -nw -c"
