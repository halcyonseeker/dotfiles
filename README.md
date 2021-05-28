# dotfiles

My personal configuration files. I've put effort into making my setup
reasonably modular and portable. So far it's been tested on Arch
GNU+Linux, MacOS, OpenBSD, and FreeBSD. 

![A screenshot of Fvwm showing Emacs, Xterm, Xteddy, and Conky](./screen.png)

The information in this file is probably out of date.

## Management
I manage these as a farm of symlinks where each directory (or package)
contains a number of files or directories for which corresponding
symlinks are created in my home directory. I used to do this with GNU
stow but have since written my own script. For example:

    % ls -A shells/
    .profile .shrc
    % ./install shells/
    ======> Installing package shells
            Dependencies: ksh, bash, or zsh and zsh-syntax-highlighting
            LN: /home/thalia/.shrc -> /home/thalia/dotfiles/shells/.shrc
            LN: /home/thalia/.profile -> /home/thalia/dotfiles/shells/.profile
    % ls -la ~
    <snip>
    <snip> .profile -> dotfiles/shells/.profile
    <snip> .shrc -> dotfiles/shells/.shrc
    <snip>
    
The installation script is still a work in progress; to install all
packages I just run `ls -F | grep /$ | ./install`, adding the `-d`
flag to uninstall them. Note that right now if you rename the symlinks
(as discused below) the script won't be able to uninstall or list
(`-l`) them. See the script's header comments and here-doc (or
`--help`) for more information.

## Commentary
### XDG Base Directory Bigotry
I keep a clean ~. A significant portion of my shell configuration is
dedicated to telling wayward programs to look for their files in
`$XDG_CONFIG_HOME`.

### A Note on Shells
I have a kinda cool but also really dumb polyglot shell configuration.

The file ~/.profile is read at login by the Korn shell, the Bourne
shell, and the Boomer shell. It primarily contains portable
environment declarations that I think should work on any Posix
superset shell. Because ~/.profile is only read by ksh, sh, and bash,
if `$SHELL` is set to the Zoomer shell then the symlink should be
renamed accordingly:

    % [ $(basename $SHELL) = zsh ] && mv .profile .zprofile

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
LaTeX documents, so my configuration relies on some external packages.

I like to keep a strict distinction between out-of-the-box
functionality and pacakge configurations, the latter just works™ with
the `use-package` macro. I mostly use Emacs in daemon mode but at some
point I'll investigate byte-compiling my init.el in order to improve
startup time.

### neo(vim)
I prefer neovim to vim, but this configuration works fine with plain
old vim provided that ~/.profile exists with `$VIMINIT` set to the
right file.

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

My Fvwm configuration was created for Fvwm2 with a specific layout and
widgets, but I'm working on making the desktop stuff more modular and
getting the basic feel to work with OpenBSD's fvwm.

I have my GTK3 theme set to Raleigh-Reloaded, which mimics the
traditional GTk2 theme. This works pretty well, though I still need to
figure out how to theme those stupid QT programs.

### Freedesktop S&!t
The XDG utils and specifications are generally stupid and obtuse, so
my configurations for URL scheme handlers and default applications are
probably fragile.
