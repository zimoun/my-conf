#!/usr/bin/env bash

FIND=find
RM=rm
ECHO=echo

function rm-check () {
    if [[ -s $1 ]]
    then
        $ECHO "Remove: $1"
        $RM -r $1
    else
        $ECHO "Already Clean: $1"
    fi
}

function find-here () {
    $FIND . -type f -name $1 -print
}

for what \
    in abbrev_defs \
           auto-save-list \
           history \
           backup \
           elpa \
           eshell \
	       tramp
do
    rm-check $what
done


for what \
    in $(find-here "*.elc")
do
    rm-check $what
done

# twice because bug. Weird?!
for what \
    in $(find-here "*.elc")
do
    rm-check $what
done


echo "Done."
