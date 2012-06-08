
Emacs Setup
===========


This repository contains my custom emacs settings. It has been tested on Fedora 16 and supports Emacs 22 and above.

Prerequisites
-------------
Install via yum:
+ emacs-goodies
+ emacs-color-theme
+ emacs-ecb

*(If these are not installed then they must be installed manually and added to the load-path)*

Setup
-----
+ Copy the emacs_template to $HOME/.emacs
+ Modify the variable `rnc_emacs_home` to point to this repository.
+ Byte compile the emacs_main.el file.


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
