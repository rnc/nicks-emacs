
Emacs Setup
===========


This repository contains my custom emacs settings. It has been tested on Fedora 16 and supports Emacs 22 and above.

Prerequisites
-------------
Install via yum:
+ emacs-goodies
+ emacs-color-theme
+ emacs-ecb
+ emacs-git

*(If these are not installed then they must be installed manually and added to the load-path)*

Setup
-----
+ Copy the emacs_template to `$HOME/.emacs`
+ Modify the variable `rnc_emacs_home` to point to this repository.
+ Byte compile the General directory e.g.

    emacs --no-site-file --no-init-file --batch --funcall batch-byte-compile `find <repo>/General -name "*.el"`

+ Byte compile the emacs_main.el file

    emacs --no-site-file --load $HOME/.emacs --batch --funcall batch-byte-compile <repo>/emacs_main.el

Unless the site installed emacs files are required a user may find it quicker to start emacs with `emacs --no-site-file`.


### Emacs 22 ###
Emacs 23 has support for all the packages built in. For Emacs 22 the following packages must be downloaded and installed into `emacs/Packages`.
* semantic
* speedbar
* cedet
* cc-mode
* elib
* eieio
* ps-print
* pcvs


Recommended Extras
------------------
Useful external lisp packages are:
+ Groovy-mode
+ Egg (for GIT)
+ Zenburn and Solarized color themes.
