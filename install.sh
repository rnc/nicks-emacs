#!/bin/sh
#
# Run this to byte compile the emacs lisp files
#

DIR=$(cd -P -- "$(dirname -- "$0")" && pwd -P)

echo "Compiling General directory..."
emacs --no-site-file --no-init-file -batch -f batch-byte-compile `find $DIR/General -name "*.el"`

[[ "$?" != "0" ]] && "Error compiling General directory!" && exit 1

echo "Compiling emacs_main.el..."
emacs --no-site-file --no-init-file --directory $DIR/General -batch -f batch-byte-compile emacs_main.el

[[ "$?" != "0" ]] && "Error compiling emacs_main!" && exit 1
