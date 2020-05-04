#invite de commande user@machine
#PS1="[\u@\h:\w]"
#PS1="\u@\h:\w$ "
PS1="\n\w/\n\u@\h$ "
        #PS1="\w/\n\u@\h$ "

if [ -f $HOME/.dir_colors ] ; then
	eval `/usr/bin/dircolors $HOME/.dir_colors -b`
fi

# # caps lock key turn off
# setxkbmap -option ctrl:nocaps
# xmodmap ~/.Xmodmap

alias e="emacsclient -t"
alias E="emacsclient -n -c"
alias ls="ls --color"
alias ll="ls -l"
alias sl="ls"

alias psg='ps -fe | grep'

alias caml="ledit -x -h ~/.ocaml_history ocaml"

LESS="-r"; export LESS
eval "$(lesspipe)" # to be able to read gzip file ect.

export EDITOR="emacsclient -t"


function cd(){
    builtin cd $1 ; ls -artl
}


alias calk="python -i -c 'from cmath import * ; import sys ; print \"Python\" , sys.version , \"[import cmath]\" '"


## completion : see /etc/bash_completion
##  and command: complete -F ..... read the doc
# Use bash-completion, if available (from README webpage)
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

#suppression du BIP
#setterm --blength
xset b off

# # shitty GnomeKeyring !!
# ## ugly turn off Warning...
# unset GNOME_KEYRING_PID
# unset GNOME_KEYRING_CONTROL


if [ -n "$GUIX_ENVIRONMENT" ]
then
    export PS1="[env]\n\w/\n\u@\h$ "
else
    source ~/.bash_profile
fi


alias mupdf=mupdf-x11
