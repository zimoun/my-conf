#!/bin/bash

# Because emacs is sometimes an alias,
# and moreover badly stored in bash_profile!
if [[ -s ~/.bash_profile ]]
then
    shopt -s expand_aliases
    . ~/.bash_profile
fi


emacs -batch \
      --eval "(require 'package)" \
      --eval "(setq package-enable-at-startup nil)" \
      --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
      -f package-initialize \
      -f package-refresh-contents \
      --eval "(add-to-list 'load-path (expand-file-name \"el\" user-emacs-directory))" \
      -l ~/.emacs.d/el/my-fun.el \
      -f my/byte-compile-el-init
