#!/usr/bin/env bash

RM=rm
EMACS=emacs

function rm-that () {
    if [ -f $1 ]
    then
        printf "Removing $1..."
        $RM -I $1
        printf "done.\n"
    fi
}

function byte-compile () {
    if [ -f $1 ]
    then
        $EMACS -batch \
               --eval "(require 'package)" \
               --eval "(setq package-enable-at-startup nil)" \
               -f package-initialize \
               --eval "(add-to-list 'load-path (expand-file-name \"el\" user-emacs-directory))" \
               -f batch-byte-compile $1
    else
        echo "Error: $1 does not exist. Check!"
    fi

}

for file in $(git ls-files |grep -e '\.el')
do
    rm-that ${file%.el}.elc
    byte-compile ${file}
done

echo "Done."
echo "Verify Error and Warning."
