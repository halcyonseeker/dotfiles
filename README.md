# dotfiles
My configs, scripts, wallpapers, and miscellaneous files. I manage them with the [install script](https://github.com/halcyonseeker/dotfiles/blob/master/install). They've been most tested on Arch GNU/Linux, but I'm working on migrating them to OpenBSD and some other systems.

## Repository Contents
+ **root/** System-wide configurations. I've put very little effort into this because there's only one file here right now.
+ **home/** My comfy configs
+ **install** Installation/management script. Still a little jankey, needs refinement and testing.

## Setup
+ **Add and configure a user**
  + `useradd -m -G wheel username`
  + `usemod -s zsh username` (or whatever)
  + `%wheel ALL=(ALL) ALL` in /etc/sudoers
  + or
  + `permit persist keepenv :wheel` in /etc/doas.conf

## The Install Script
Just run `./install <options> <argument>`
+ `-o <arg>` Specify the name of the OS. 


## Alternative Configurations
If a display manager is used, rename `~/.xinitrc` to `~/.xsession` after creating the symlinks.

If a (posix-compliant) shell other then zsh is used, create the symlinks then rename `~/.zprofile` to `~/.profile` or whatever is appropriate. 
