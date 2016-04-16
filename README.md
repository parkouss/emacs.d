# weemacs
*********

This is my emacs config. The design comes from the excellent Prelude,
but I needed quite some changes (I am not an emacs guru yet) and so I decided
to start from scratch my own emacs config.


## Installation
***************

1. Clone the repo:

```bash
git clone https://github.com/parkouss/emacs.d ~/.emacs.d
```

2. Define your required modules:

```bash
cp ~/.emacs.d/sample/weemacs-modules.el ~/.emacs.d/
```

Then edit this file to enable/disable the required modules.


## Launch weemacs
*****************

You can just run emacs, so for example:
```bash
emacs
```

But, this is not my preferred way anymore. I really love the emacs daemon -
starts a global emacs instance on your system, then use that from everywhere.

To achieve this, I prefer to use `emacsclient` with the `-a ''` (alternate
editor) flag. This will start the emacs instance if not running already.

So from my arch emacs install, I tweak `/usr/share/applications/emacs.desktop`:

```ini
Exec=emacsclient -c -a '' %F
```

and in my .zshrc, I put the following:

```bash
export ALTERNATE_EDITOR=''
alias em="emacsclient -c"
alias e="emacsclient -t"
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"
```
