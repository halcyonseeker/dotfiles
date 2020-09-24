# dotfiles
My configs, scripts, wallpapers, and miscellaneous files. I manage them with GNU Stow. They've been most tested on Arch GNU/Linux, but I'm working on migrating them to OpenBSD and some other systems.

## Repository Contents
+ **root/**
+ **shells/** 
+ **base/**
+ **gui/** 

## Setup
+ **Add and configure a user**
  + `useradd -m -G wheel username`
  + `usemod -s zsh username` (or whatever)
  + **sudo** `%wheel ALL=(ALL) ALL` in /etc/sudoers
  + **doas** `permit persist keepenv :wheel` in /etc/doas.conf

## Alternative Configurations
If a display manager is used, rename `~/.xinitrc` to `~/.xsession` after creating the symlinks.

If a (posix-compliant) shell other then zsh is used, create the symlinks then rename `~/.zprofile` to `~/.profile` or whatever is appropriate. 
