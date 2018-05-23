#!/usr/bin/env bash

# Because emacs is sometimes an alias,
# and moreover badly stored in bash_profile!
if [[ -s ~/.bash_profile ]]
then
    shopt -s expand_aliases
    . ~/.bash_profile
fi


function rm-that () {
    if [ -f $1 ]
    then
        printf "Removing $1..."
        command rm -I $1
        printf "done.\n"
    fi
}

function byte-compile () {
    if [ -f $1 ]
    then
        emacs -batch \
               --eval "(require 'package)" \
               --eval "(setq package-enable-at-startup nil)" \
               -f package-initialize \
               --eval "(add-to-list 'load-path (expand-file-name \"el\" user-emacs-directory))" \
               -f batch-byte-compile $1
    else
        echo "Error: $1 does not exist. Check!"
    fi

}

emacs -batch \
      --eval "(require 'package)" \
      --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
      -f package-initialize -f package-refresh-contents
for file in $(git ls-files |grep -e '\.el')
do
    rm-that ${file%.el}.elc
    byte-compile ${file}
done

echo "Done."
echo "Verify Error and Warning (if any)."
