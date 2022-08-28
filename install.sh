#!/bin/bash
mkdir ~/.config/emacs 2> /dev/null

cp init.el ~/.config/emacs/
cp org-interface.el ~/.config/emacs/
cp banner.png ~/.config/emacs/

echo "Done"
