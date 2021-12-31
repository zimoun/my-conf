#!/bin/sh


who=$1
key=$2

echo "machine: $who"
echo "keyname: $key"

ssh-keygen -t rsa -b 4096 -f ~/.ssh/$key

cat ~/.ssh/${key}.pub \
    | ssh $who \
          'cat >> .ssh/authorized_keys && echo "Key copied."'

ssh $who \
    'chmod 600 .ssh/authorized_keys && echo "Key ok."'
