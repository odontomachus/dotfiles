#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Turn the **** bell off
set bell-style none

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PAGER=less
export EDITOR='emacs -mm'
export VISUAL='emacs -mm'

export HISTFILESIZE=20000
export HISTSIZE=10000
shopt -s histappend
# Combine multiline commands into one in history
shopt -s cmdhist
# Ignore duplicates, ls without options and builtin commands
HISTCONTROL=ignoreboth
export HISTIGNORE="&:ls:[bf]g:exit"

## Up Arrow: search and complete from previous history
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

alias e='emacs -mm'

# Load rvm as a function
[ -r $HOME/.rvm/scripts/rvm ] && source $HOME/.rvm/scripts/rvm

# Autocomplete virtualenv listings
_venv() {
    ls -d ~/virtualenv/$2* 2> /dev/null | xargs -n 1 basename 2> /dev/null
}

# Activate a virtualenv
venv () {
    test -f ~/virtualenv/$1/bin/activate || ( echo "No such virtualenv" && exit 1 ) || return 1
    source ~/virtualenv/$1/bin/activate
}

complete -C _venv venv

mkvenv () {
        test -f ~/virtualenv/$1/bin/activate && ( echo "Virtualenv already exists" && exit 1 )
	virtualenv ~/virtualenv/"$@"
}

rmvenv () {
    test -d ~/virtualenv/"$1" || ( echo "No such virtualenv" && exit 1 ) || return 1
    N=0;
    echo -n "Are you sure you want to remove virtualenv $1? (y/N) "
    read a
    while [ $N -lt 3 ]; do
	[[ x$a =~ ^x[yY]$ ]] && return $( rm -r ~/virtualenv/"$1" )
	[[ x$a =~ ^x[nN]$ ]] && echo "Cancelling" && return 1
	let N=N+1
	echo -n "Please answer 'y' or 'n'."
	read a
    done;
}

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion
