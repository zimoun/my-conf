#!/bin/sh


DB_DIR=$HOME/.cache
DB_FILE=locate.db

DB=$DB_DIR/$DB_FILE

BIN=/home/simon/.guix-profile/bin

$BIN/updatedb -l 0 -o $DB -U / \
              --prune-bind-mounts=1 \
              --prunefs="9p afs anon_inodefs auto autofs bdev binfmt_misc cgroup cifs coda configfs cpuset cramfs debugfs devpts devtmpfs ecryptfs exofs ftpfs fuse fuse.encfs fuse.sshfs fusectl gfs gfs2 hugetlbfs inotifyfs iso9660 jffs2 lustre mqueue ncpfs nfs nfs4 nfsd pipefs proc ramfs rootfs rpc_pipefs securityfs selinuxfs sfs shfs smbfs sockfs sshfs sysfs tmpfs ubifs udf usbfs vboxsf" \
              --prunepaths="/gnu/store /afs /mnt /net /sfs /tmp /udev /var/cache /var/lib/pacman/local /var/lock /var/run /var/spool /var/tmp" \
              --prunenames=".git .hg .svn .cache Trash .Trash-$(id -u)"
