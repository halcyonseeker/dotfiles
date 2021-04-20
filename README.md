# dotfiles

My personal configuration files. I've put effort into making my setup
reasonably modular and portable. So far it's been tested on Arch
GNU+Linux, MacOS, OpenBSD, and FreeBSD. 

The information in this file is probably out of date.

## Management
I manage these as a farm of symlinks using GNU Stow. Each directory
(or package) contains a number of files or directories for which
stow creates corresponding links in my home directory. For example:

    % cd dotfiles/
    % ls -A shells/
    .profile .shrc
    % stow -t ~ shells
    % ls -la ~
    <snip>
    <snip> .profile -> dotfiles/shells/.profile
    <snip> .shrc -> dotfiles/shells/.shrc
    <snip>
    
## Contents
I sometimes like to reorganise this repository for fun, so this might
not be accurate:
* **base/** Configurations that don't explicitly depend on a
  graphical user interface. I keep these separate from GUI to make it
  easier to use machines that I don't have as much control over.
* **gui/** Configurations for X11, various window managers, and
  explicitly graphical programs.
I'm gradually moving "packages" out of base/ and gui/, right now this 
has been reflected in shells/, xorg/, and neovim/.

## Commentary
### XDG Base Directory Bigotry
I keep a clean ~. A significant portion of my shell configuration is
dedicated to telling wayward programs to look for their files in
`$XDG_CONFIG_HOME`.

### A Note on Shells
I have a kinda cool but also really dumb polyglot shell configuration.

The file ~/.profile is read at login by the Korn shell and the Bourne
shell. It primarily contains portable environment declarations that I
think should work on any Posix-superset shell. Because ~/.profile is
only read by ksh and sh, if `$SHELL` is set to the Boomer shell or the 
Zoomer shell then the symlink should be renamed accordingly:

    % [ $(basename $SHELL) = zsh ] && mv .profile .zprofile
    $ [ $(basename $SHELL) = bash ] && mv .profile .bash_profile

The file ~/.shrc is a weird multi-shell polyglot file that is run by
non-login shells. For sh and ksh it is set as the value of the `$ENV`
variable and ran. For bash and zsh the symlink should be renamed:

    % [ $(basename $SHELL) = zsh ] && mv .shrc .zshrc
    $ [ $(basename $SHELL) = bash ] && mv .shrc .bashrc

This works perfectly for zsh and OpenBSD ksh, and require some tweaks
under bash. It sort-of works for dash and FreeBSD's `/bin/sh`, though
those require more work. At some point I'll test it with Busybox's ash
and the other Korn shells like pdksh and mksh.

Note that this won't work with shells like csh, tcsh, Plan 9 rc, Nu,
and Fish that aren't derived directly from the Bourne Shell.

### Emacs Tweaks
I keep my Emacs config in ~/.config/emacs instead of ~/.emacs.d, which
only works for version >27. To work with older versions of Emacs a
symlink is required:

    ln -s ~/.emacs.d ~/.config/emacs
    
I like to use Emacs for weird things like email and pdf previews of
LaTeX documents, so my configuration relies on some external packages
like imagemagick, texlive, biber, gpg, pass, protonmail-bridge, and mu4e. 

My configuration heavily depends on melpa packages like AucTeX and
Evil Mode, so `M-x package-refresh-contents` and `M-x
package-install-selected-packages` are required. I'm slowly rewriting
everything to just work<sup>TM<sup> with the `use-package` macro. I
should also take advantage of emacs' ability to compile elisp to
reduce the startup time.

### neo(vim)
I prefer neovim to vim, but I think this configuration should work
with plain old vim provided that ~/.profile exists with `$VIMINIT` set
to the right file.

### X Stuff
I used to be a a pathological WM hopper, though I've started to
fossilise around Fvwm. As such I have some keybindings managed by
sxhkdrc as well as a cwm configuration in this repo. In the future
I'll probably make an effort to streamline things around Fvwm. All my
xorg stuff lives in ~/.config/xorg with `$XINITRC` set to the
appropriate file. Sadly the startx script (a wrapper around xinit)
doesn't support this variable (despite it requiring a 1-line change
and an issue having been open for *years*), so at some point I'll
probably just role my own.

I have my GTK3 theme set to Raleigh-Reloaded, which mimics the
traditional GTk2 theme. This works pretty well, though I still need to
figure out how to theme those stupid QT programs.

### Freedesktop S&!t
The XDG utils and specifications are generally stupid and obtuse, so
my configurations for URL scheme handlers and default applications are
probably fragile.
