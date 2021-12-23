#!/bin/bash

# Avoid issues
XDG_CONFIG_HOME="$HOME/.config/"
mkdir -p $XDG_CONFIG_HOME/guix
mkdir -p $XDG_CONFIG_HOME/emacs
mkdir -p ~/.local/bin
mkdir -p ~/mail/.notmuch
mkdir -p ~/mail/gmail/inbox
mkdir -p ~/mail/gmail/sent
mkdir -p ~/mail/drafts
mkdir -p ~/mail/queue

if [ -f ~/.bashrc ]
then
    mv ~/.bashrc ~/.bashrc.bak
fi

guix shell stow \
     -- stow -t $HOME . -R -v
