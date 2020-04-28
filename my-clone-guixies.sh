#!/bin/bash

GUIX_SRC=~/src/guix/

mkdir -p $GUIX_SRC/wk

HERE=$(pwd)
cd $GUIX_SRC

url=https://git.savannah.gnu.org/git/guix
for project in bootstrappable \
		   data-service       \
		   guix-artwork       \
		   guix-cuirass       \
		   maintenance
do
    echo ""
    if [ -d $project ]
    then
	    cd $project
	    echo `pwd`
	    git checkout master && git fetch
	    cd ..
    else	
	    git clone ${url}/${project}.git
    fi
done

project=guix
echo ""
if [ ! -d $project ]
then
    git clone ${url}.git
else
    echo `pwd`
    echo "Fetch manually!"
fi

cd $HERE
