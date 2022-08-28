#!/bin/bash

if [[ ! -f "~/.config/emacs" ]]
then
	mkdir "~/.config/emacs"
fi

cp init.el ~/.config/emacs/
cp org-interface.el ~/.config/emacs/
cp banner.png ~/.config/emacs

echo "Done"
