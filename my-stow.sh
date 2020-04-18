#!/bin/bash

# Avoid issues
mkdir -p ~/.config/guix
mkdir -p ~/.emacs.d/
mkdir -p ~/.local/bin/

if [ -f ~/.bashrc ]
then
    mv ~/.bashrc ~/.bashrc.bak
fi

guix environment --ad-hoc stow \
     -- stow -t $HOME . -R -v
