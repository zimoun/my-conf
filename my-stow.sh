#!/bin/sh

mydots=/home/simon/work/my-conf

guix environment --ad-hoc stow \
     -- stow -d $mydots -t /home/simon . -R 
