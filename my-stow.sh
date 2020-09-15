#!/bin/bash

# Avoid issues
mkdir -p $XDG_CONFIG_HOME/guix
mkdir -p $XDG_CONFIG_HOME/emacs
mkdir -p ~/.local/bin
mkdir -p ~/mail/.notmuch
mkdir -p ~/mail/gmail/inbox
mkdir -p ~/mail/gmail/sent
mkdir -p ~/mail/drafts

if [ -f ~/.bashrc ]
then
    mv ~/.bashrc ~/.bashrc.bak
fi

guix environment --ad-hoc stow \
     -- stow -t $HOME . -R -v
