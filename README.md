dotfiles
========

My personal configuration files.  I've put effort into making my setup
reasonably modular and portable.  So far it's been tested on several
distributions of GNU Linux, two BSDs, and MacOS.

![A screenshot of Fvwm with Neofetch showing FreeBSD](./screen.png)

The information in this file is probably out of date.

Management
============

I manage these as a farm of symlinks where each directory (or package)
contains a number of files or directories for which corresponding
symlinks are created in my home directory.  The `mk` and `unmk` scripts
create and remove symlinks respectively.  Both install all packages by
default but can take arguments.  Note that `unmk` doesn't yet know how
to remove symlinks that were renamed as will be discussed in the
shells section.

Commentary
==========

## XDG Base Directory Bigotry
I keep a clean ~.  A significant portion of my shell configuration is
dedicated to telling wayward programs to look for their files in the
appropriate XDG directories.

## A Note on Shells
I have a kinda cool but also really dumb polyglot shell configuration.

The file ~/.profile is read at login by the Korn shell, the Bourne
shell, and the Boomer shell.  It primarily contains portable
environment declarations that I think should work on any Posix
superset shell.  Because ~/.profile is only read by ksh, sh, and bash,
if `$SHELL` is set to the Zoomer shell then the symlink should be
renamed accordingly:

    % [ $(basename $SHELL) = zsh ] && mv .profile .zprofile

The file ~/.shrc is a weird multi-shell polyglot file that is run by
non-login shells.  For sh and ksh it is set as the value of the `$ENV`
variable and ran.  For bash and zsh the symlink should be renamed:

    % [ $(basename $SHELL) = zsh ] && mv .shrc .zshrc
    $ [ $(basename $SHELL) = bash ] && mv .shrc .bashrc

This works perfectly for zsh and OpenBSD ksh, and require some tweaks
under bash.  It sort-of works for dash and FreeBSD's `/bin/sh`, though
those require more work.  At some point I'll test it with Busybox's
ash and the other Korn shells like pdksh and mksh.

Note that this won't work with shells like csh, tcsh, Plan 9 rc, Nu,
and Fish that aren't derived directly from the Bourne Shell.

## Emacs Tweaks
I keep my Emacs config in ~/.config/emacs instead of ~/.emacs.d, which
only works for version >27.  To work with older versions of Emacs a
symlink is required:

    ln -s ~/.config/emacs ~/.emacs.d 
	
Emacs is the ideal environment for working with text, however it has
some limitations that force some major modes to rely on external
programs.  I end up building these (mu4e and telega) from source in
order to keep up with their slow release cycle on FreeBSD.

In order to reduce startup times and keep a nice, multi-headed, cache
of things I'm working on I like to start an Emacs daemon at login and
connect to it using the Emacsclient program.  Unfortunately there's a
GTK bug that causes it to crash in the (admittedly rare) event that X
dies, so I prefer to build it from source using the Lucid toolkit.  On
FreeBSD this means:

    sudo pkg install `pkg rquery %dn emacs` isync msmtp gnupg imagemagick7
	git clone https://git.savannah.gnu.org/git/emacs.git && cd emacs
	./configure --with-x-toolkit=lucid --without-libgmp
	make && sudo make install
	
## neo(vim)
I prefer neovim to vim, but this configuration works fine with plain
old vim provided that ~/.profile exists with `$VIMINIT` set to the
right file.

## X Stuff
I've been using pretty much the same Fvwm config for quite some time,
though I used to hop between window managers quite frequently.  As
such there's a bunch of legacy stuff in cwm/ and my xinitrc.  All
xorg stuff lives in ~/.config/xorg with `$XINITRC` set to the
appropriate file.  Sadly the startx script (a wrapper around xinit)
doesn't support this variable (despite it requiring a 1-line change
and an issue having been open for *years*), so at some point I'll
probably just role my own.

My Fvwm configuration was created for Fvwm2 with a specific layout and
widgets, but I'm working on making the desktop stuff more modular.
Fvwm3 has some pretty neat improvements for multiple monitors so I'll
probably switch to a copy of that built from source.

## Freedesktop S&!t
The XDG utils and specifications are generally stupid and obtuse, so
my configurations for URL scheme handlers and default applications are
probably fragile.  Everything is in desktop/.config/mimeapps.list
though I should probably just write a script to auto-generate things,
god knows Firefox, Gimp, and LibreOffice love to walk all over it.
