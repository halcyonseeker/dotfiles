#
# Zsh specific run-control file
# General configurations can go in ~/.profile
# Delete the symlink to this file if a different shell is in use
#

# Source general environment variables and aliases
source ~/.profile

# The following lines were added by compinstall
zstyle :compinstall filename '$HOME/.config/zsh/.zshrc'
autoload -Uz compinit
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
# End of lines added by compinstall
# Lines figured by zsh-newuser-install
HISTFILE=$HOME/.cache/zsh/histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory
# End of lines configured by zsh-newuser-install

# Load zsh plugins
[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] && \
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Prevent "no matches found" problem with mpv & youtube-dl
setopt +o nomatch
# Set terminal title
precmd () {print -Pn "\e]0;%n@%m: %~\a"}

# Make keys work
bindkey '^[[P' delete-char
bindkey '^[[5~' up-line-or-history
bindkey '^[[6~' down-line-or-history
bindkey '^[[1~' beginning-of-line
bindkey '^[[4~' end-of-line

# Override PS1 in ~/.profile
if [ "$EUID" -ne 0 ]; then
	PROMPT="%B%F{green}%n@%m:%~%(!.#.>) %b%f"
	#PROMPT="%B%F{green}[%~]%# %b%f"
else
	#PROMPT="%B%F{red}[%~]%# %b%f"
	PROMPT="%B%F{red}%n@%m:%~%(!.#.>) %b%f"
fi
