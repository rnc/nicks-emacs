
Emacs Setup
===========


This repository contains my custom emacs settings. It has been tested on Fedora 29 & Emacs 26.1.

Prerequisites
-------------
Install via yum/dnf:
+ emacs
+ emacs-auctex
+ emacs-color-theme
+ emacs-common
+ emacs-goodies
+ emacs-json-mode
+ emacs-json-reformat
+ emacs-json-snatcher
+ emacs-rpm-spec-mode
+ emacs-yaml-mode

*(If these are not installed then their RPM contents must be installed manually and added to the load-path)*

Setup
-----
+ Copy the emacs_template to `$HOME/.emacs`
+ Modify the variable `rnc_emacs_home` to point to this repository.
+ Modify other configuration variables in it to your preference.
+ Unless the site installed emacs files are required a user may find it quicker to start emacs with `emacs --no-site-file`.

##### Byte Compile #####

Either byte compile the files manually e.g.

+ Byte compile the General directory e.g.

	``emacs --no-site-file --no-init-file --batch --funcall batch-byte-compile `find <repo>/General -name "*.el"` ``

+ Byte compile the emacs_main.el file

	``emacs --no-site-file --load $HOME/.emacs --batch --funcall batch-byte-compile <repo>/emacs_main.el``

Or, use the supplied install.sh script e.g.

   ``<install-dir>/install.sh``


Recommended Extras
------------------
Useful external lisp packages are:
+ [Groovy-mode]
+ [Yaml-mode] Yaml-mode (also available as RPM `emacs-yaml-mode`)
+ [Json-mode]
+ [Dockerfile-mode] (Note this has a dependency on s below)
+ [s] A string manipulation library
+ [Zenburn] and [Solarized] color themes.
   *By default `rnc_load_colours` will use the zenburn theme if installed.*
+ [Markdown] Mode.
+ [HTML Folding] Mode.

[Groovy-mode]:     https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes.git
[Yaml-mode]:       https://github.com/yoshiki/yaml-mode
[Json-mode]:       https://github.com/joshwnj/json-mode
[Dockerfile-mode]: https://github.com/spotify/dockerfile-mode
[s]:               https://github.com/magnars/s.el
[Zenburn]:         https://github.com/bbatsov/zenburn-emacs
[Solarized]:       https://github.com/sellout/emacs-color-theme-solarized
[Markdown]:        http://jblevins.org/git/markdown-mode.git
[HTML Folding]:    https://github.com/ataka/html-fold
