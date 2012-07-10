# Emacs Configuration

This is a configuration for _Emacs_. It focuses mainly on programming,
with settings for many languages. It also allows custom settings for
each OS/Machine.

This configuration started in Jan 2010, derived from the [Emacs
Starter Kit](http://github.com/technomancy/emacs-starter-kit/).

## Installation

1. Grab GNU Emacs (23.* version!), use apt or another package manager
for linux. For Mac OS X, use [some prebuilt
binaries](http://emacsformacosx.com/) (I dislike both Carbon- and
Aquamacs). Windows users can get it directly [from
GNU](http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-23.1-bin-i386.zip).

2. Link the directory with `ln -s ~/{emacs_dir} ~/.emacs.d`.

3. Map the CAPS key to control. It is very easy to do in Mac OS X and
Linux, you'll need some registry files for windows.

4. Install el-get as described below.

## Byte Compiling

This config tries very hard to avoid loading packages or other
resources until they are required, which should keep the startup time
to a minimum. That said, it can useful to byte-compile some
packages. The command to do that is:

`C-u 0 M-x byte-recompile-directory`

The _vendor_ folder is automatically compiled at start time, as well
as _el-get-packages_, as they are already compiled. __Don't__ apply
this to the common settings, like _bindings_, _misc_, or the _lang_
folder. If you change the .el file, the .elc might get outdated and
emacs will be using the wrong file.

## Structure

The init.el file is where everything begins. It's the first file to
get loaded. Other relevant files are:

* helpers.el: Functions and settings used during intialization.

* defuns.el: Some useful functions for editing and configuration.

* bindings.el: Some changes to the default keybindings, plus new ones
  for our custom functions.

* misc.el: Visual settings, colors, UTF-8 for everything, and some
  other settings.

* registers.el: Registers config.

* (e)shell-utils.el: Settings to use shells inside emacs. Not very
  much used as I've been using the shell directly.

### Configuration for specific packages

* desktop-config.el: Automatically save/restore the session when
  restarting emacs.

* flymake-config.el: Helpers and settings to use flymake for syntax
  error highlighting.

* git-config.el: Git related configuration.

* ibuffer-config.el: Customize the ibuffer window.

* mode-line-config.el: Customize the status bar.

* w3m-config: Configuration to use the _w3m_ browser inside emacs.

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

### Spell correction

_Flyspell_ is used to provide spell correction for text-mode and
comments in source files. Depending on the system it might be
necessary to install _aspell_ and _aspell-dict-en_.

## el-get

It is intended as a replacement of [ELPA](http://tromey.com/elpa). To
use it, clone it inside the _emacs_ folder with:

`git clone git@github.com:sirech/el-get.git`

Keep in mind that the el-get repository is just a fork, and has to be
synchronized with the original occasionally.

### Downloading packages

Packages defined in _el-get-config.el_ get downloaded automatically
inside _el-get-packages_.

It is assumed that a connection to the internet is available. _git_
(>= 1.7) and _svn_ are also required and have to be reachable from
_emacs_

### Proxies

If you are behind a proxy, _git_ might not work. To solve this,
depending on the protocol used:

* http: Set your proxy via

    (setq url-using-proxy t)
    (setq url-proxy-services
      '(("http" . (concat host ":" port))))

* git: You need to define the environment variable
  _GIT_PROXY_COMMAND_, and set it to a script that does the
  redirecting
  
    (setenv "GIT_PROXY_COMMAND" "git-proxy-cmd")
    
Where the command can be something like:

    nc -x$PROXY_HOST:1080 -X5 $*
    
The command has to be in a directory added to the _PATH_ in emacs
(like `~/bin`).

## Test

This configuration is used almost daily under Mac OS X, frequently
under Linux (Ubuntu), and occasionally under Windows (with help of
cygwin), for general text editing, and for programming. In case of
problems, running the last version of everything usually helps.

# Resources for Emacs

Some interesting places to learn more about emacs:

* [The best Wiki about Emacs](www.emacswiki.org)

* [Tips from Steve
  Yegge](http://steve.yegge.googlepages.com/effective-emacs)

* [A pretty good Screencast](http://peepcode.com/products/meet-emacs)

* http://www.dotemacs.de/

* http://snarfed.org/space/why%20I%20don't%20run%20shells%20inside%20Emacs
  
* [This page is not actively maintained anymore, but still has good
  stuff](http://emacsblog.org/)
  
* [Some more tricks](http://sachachua.com/wp/category/emacs/)
