
export GUIX_MYMANIFESTS=~/.config/guix/manifests
export GUIX_MYPROFILES=~/.config/guix/profiles

for p in $(ls --color=no -1 $GUIX_MYPROFILES)
do
    profile=${GUIX_MYPROFILES}/${p}/${p}
    if [ -d $profile ]
    then
	GUIX_PROFILE=$profile
	. $profile/etc/profile
	# unset $profile
    fi
done

GUIX_PROFILE=$HOME/.guix-profile
. $GUIX_PROFILE/etc/profile

# guix install glibc-locales
## WARN: utf8 is misleading
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale


# from Manual sectiom X.509 Certificates
## https://guix.gnu.org/manual/en/html_node/X_002e509-Certificates.html#X_002e509-Certificates
export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
export GIT_SSL_CAINFO="$SSL_CERT_FILE"
export CURL_CA_BUNDLE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"


# the latest Guix version lives there
# needs to be first in the PATH
GUIX_PROFILE=$HOME/.config/guix/current
. $GUIX_PROFILE/etc/profile


# Ugly workaround
export PATH="$HOME/.config/guix/current/bin${PATH:+:}$PATH"
# ~/.config/current/bin/guix repl works with readline
export GUILE_LOAD_PATH="$HOME/.guix-profile/share/guile/site/3.0${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
# lastest info manual
export INFOPATH="$HOME/.config/guix/current/share/info${INFOPATH:+:}$INFOPATH"


