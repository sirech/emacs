# Emacs Configuration

This is a configuration for _Emacs_. It focuses mainly on programming,
with settings for many languages. It also allows custom settings for
each OS/Machine.

This configuration started in Jan 2010, derived from the [Emacs
Starter Kit](http://github.com/technomancy/emacs-starter-kit/).

# Resources for Emacs

Some interesting places to learn more about emacs:

* [The best Wiki about Emacs](www.emacswiki.org)

* [Tips from Steve
  Yegge](http://steve.yegge.googlepages.com/effective-emacs)

* [A pretty good Screencast](http://peepcode.com/products/meet-emacs)

* [[http://www.dotemacs.de/]]

* [[http://snarfed.org/space/why%20I%20don't%20run%20shells%20inside%20Emacs]]
  
* [This page is not actively maintained anymore, but still has good
  stuff](http://emacsblog.org/)
  
* [Some more tricks](http://sachachua.com/wp/category/emacs/)

## Installation

1. Grab GNU Emacs (23.* version!), use apt or another package manager
for linux. For Mac OS X, use [some prebuilt
binaries](http://emacsformacosx.com/) (I dislike both Carbon- and
Aquamacs). Windows users can get it directly [from
GNU](http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-23.1-bin-i386.zip).

2. Link the directory with `ln -s ~/{emacs_dir} ~/.emacs`.

If you find yourself missing some autoloads after an update (which
should manifest itself as "void function: foobar" errors) try M-x
regen-autoloads. After some updates an M-x recompile-init will be
necessary; this should be noted in the commit messages.

## Byte Compiling

This config tries very hard to avoid loading packages or other
resources until they are required, which should keep the startup time
to a minimum. That said, it can useful to byte-compile some
packages. The command to do that is:

`C-u 0 M-x byte-recompile-directory`

It is a good idea to apply this command to the _vendor_ and
_elpa-to-submit_ folders. It is not necessary to do so for _elpa_, as
they are already compiled. __Don't__ apply this to the common
settings, like _bindings_, _misc_, or the _lang_ folder. If you change
the .el file, the .elc might get outdated and emacs will be using the
wrong file.

## Structure

The init.el file is where everything begins. It's the first file to
get loaded. Other relevant files are:

* defuns.el: Some useful functions for editing and configuration.

* bindings.el: Some changes to the default keybindings, plus new ones
  for our custom functions.

* misc.el: Visual settings, colors, UTF-8 for everything, and some
  other settings.

* registers.el: Registers config.

* (e)shell-utils.el: Settings to use shells inside emacs. Not very
  much used as I've been using the shell directly.

* ac-config.el: Settings for the auto complete mode, which is very
  useful for most programming modes.

* flymake-config.el: Helpers and settings to use flymake for syntax
  error highlighting.

* w3m-config: Configuration to use the _w3m_ browser inside emacs.

Files that are pending submission to ELPA are bundled with the starter
kit under the directory elpa-to-submit/. The understanding is that
these are bundled just because nobody's gotten around to turning them
into packages, and the bundling of them is temporary. For these
libraries, autoloads will be generated and kept in the loaddefs.el
file. This allows them to be loaded on demand rather than at startup.

## User/Machine/OS Specific settings

It is possible to add extra configuration, by creating files that are
run depending on where emacs is running.

First, a specific configuration is loaded based on the OS that emacs is
running in. The files are located in the _os_ folder. One example is
configuring the special keys for Mac OS X.

A file based on the hostname of the machine is also loaded. Here I
usually set up the size of the emacs window, the font and some
specific environment variables.

Lastly, it is also possible to run a configuration file based on the
user name. This is currently not used.

## Language settings

As this config is oriented towards programming, languages have usually
their own config file. They are found in the _lang_ folder. They are
centered around setting the correct mode, specific bindings, syntax
error highlight and autocomplete facilities. Some of the modes need
extra packages or external programs. These are not included in the
distribution to avoid bloat. However, each mode has detailed
instructions in its header about how to install them. Usually, the
config auto-detects the availability, so that it is not necessary to
set extra variables, or comment/uncomment things.

## Emacs Lisp Package Archive

Libraries from [ELPA](http://tromey.com/elpa) are preferred when
available since dependencies are handled automatically, and the burden
to update them is removed from the user. In the long term, ideally
everything would be installed via ELPA, and only _package.el_ would
need to be distributed with the starter kit. (Or better yet,
package.el would come with Emacs...) See _starter-kit-elpa.el_ for a
list of libraries that are pending submission to ELPA. Packages get
installed in the _elpa/_ directory.

## Test

This configuration is used almost daily under Mac OS X, frequently
under Linux (Ubuntu), and occasionally under Windows (with help of
cygwin), for general text editing, and for programming. In case of
problems, running the last version of everything usually helps.

## Contributing

If you know your way around Emacs, please try out the starter kit as a
replacement for your regular dotfiles for a while. If there's anything
you just can't live without, add it or let me know so I can add
it. Take a look at what happens in init.el to get started.

Also: see the file TODO. Helping submit new libraries to ELPA is the
easiest way to help out. There are two ways you can do this: either
take new libraries and make them ready for ELPA, dropping them in the
elpa-to-submit directory or take files that are already in
elpa-to-submit, ensuring all their dependencies are correctly loaded
into ELPA, and sending them to the ELPA maintainer. There are details
at http://tromey.com/elpa/upload.html for how ELPA submission
works. Grep the project for TODO for other things.

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.
