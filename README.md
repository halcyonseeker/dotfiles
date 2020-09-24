# dotfiles
My configs, scripts, wallpapers, and miscellaneous files. I symlink
them to my home directory with GNU Stow. They've been most tested on
Arch GNU/Linux, but I'm working on migrating them to OpenBSD and some
other systems.

## Repository Contents
+ **root/** Some minor files that could be symlinked to /.
+ **shells/** A simple but comfortable shell configuration. I mostly
  use zsh, but I'm working on making this portable to any
  posix-superset shells. These are kept seperate from base because I
  don't need any fancy stuff on my servers.
+ **base/** Configurations that don't explicitely depend on a
  graphical user interface. I keep these seperate from GUI to make it
  easier to use machines that I don't have as much control over.
+ **gui/** Configurations for X11, various window managers, and
  explicitly graphical apps

## Setup
+ **Add and configure a user**
  + `useradd -m -G wheel USERNAME`
  + `usemod -s SHELL USERNAME`
+ **sudo** `%wheel ALL=(ALL) ALL` in /etc/sudoers
+ **doas** `permit persist keepenv :wheel` in /etc/doas.conf

## Alternative Configurations
If a display manager is used, rename `~/.xinitrc` to `~/.xsession`
after creating the symlinks.
