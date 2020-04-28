#!/bin/bash

GUIX=~/.config/guix/current/bin/guix

GUIX_MYMANIFESTS=$(pwd)/.config/guix/manifests

# WARN: defines here and in ~/.bash_profile (exported there).
GUIX_MYPROFILES=~/.config/guix/profiles

mkdir -p $GUIX_MYPROFILES


$GUIX describe -f channels \
     > $GUIX_MYPROFILES/from-channels.scm

for m in $(ls --color=no -1 $GUIX_MYMANIFESTS)
do
    p=$(basename $m .scm)
    if [ ! -d ${GUIX_MYPROFILES}/${p} ]
    then
	if [ $p != default ]
	then
	    echo "Processing $m ..."
	    mkdir -p ${GUIX_MYPROFILES}/${p}

	    $GUIX package				\
		 -m  ${GUIX_MYMANIFESTS}/${m}		\
		 -p  ${GUIX_MYPROFILES}/${p}/${p}	\
		 --fallback

	    printf "Profile $p done.\n\n"
	fi
    fi
done
$GUIX package -m ${GUIX_MYMANIFESTS}/default.scm --fallback
