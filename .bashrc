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

alias em="emacs -e my/theme-default"
alias emx="emacs -nw -e my/theme-default"
alias e="emacsclient -t"
alias E="emacsclient -n -c"
alias m="emacsclient -t -e '(notmuch)'"
alias M="emacsclient -n -c -e '(notmuch)'"

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

# used systemd, e.g., stop fetching email
# systemctl --user stop mbsync.service mbsync.timer
export XDG_RUNTIME_DIR=/run/user/$(id -u)

# is it not normalized?
# used by emacs
export XDG_CONFIG_HOME="$HOME/.config/"

export PATH="$HOME/.local/bin:$PATH"

if [ -n "$GUIX_ENVIRONMENT" ]
then
    export PS1="[env]\n\w/\n\u@\h$ "
else
    source ~/.bash_profile
fi


alias mupdf=mupdf-x11
