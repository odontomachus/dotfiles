# .bashrc

shopt -qs histappend
export HISTCONTROL=ignoredups:erasedups:ignorespace

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# The stuff bellow this will only apply to interactive shells
# exit if we're not running an interactive shell
[ -z "$PS1" ] && return

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Turn the **** bell off
set bell-style none

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
	python3 -m venv ~/virtualenv/"$@"
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

alias spwd='/bin/pwd > '$HOME'/.spwd'
alias lpwd='cd "`cat '$HOME'/.spwd`"'
alias va='. .venv/bin/activate'

export EDITOR=emacs
export VISUAL=emacs
#export CDPATH=$CDPATH

touch ~/.sshagent
source ~/.sshagent > /dev/null
ssh-add -l &>/dev/null
if [[ "$?" = 2 ]] ; then
    {
        flock -x -n ~/.sshagent.lock ssh-agent -t 10h > ~/.sshagent 2>/dev/null
    } 3>> ~/.sshagent
fi;
{
    flock -w 1 -s 3
    source ~/.sshagent > /dev/null
} 3< ~/.sshagent


# up down arrow key behavior
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'

export HISTSIZE=10000

export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"

# No accessibility bridge.
export NO_AT_BRIDGE=1

export NVM_DIR="/home/jonathan/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# use stable nodejs
nvm use stable &> /dev/null &
# . $HOME/.asdf/asdf.sh

# . $HOME/.asdf/completions/asdf.bash

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# The next line updates PATH for the Google Cloud SDK.
if [ -f '~/.local/lib/google-cloud-sdk/path.bash.inc' ]; then . '~/.local/lib/google-cloud-sdk/path.bash.inc'; fi

# The next line updates PATH for the Google Cloud SDK.
if [ -d "$HOME/go/bin" ]; then PATH="$PATH:~/go/bin"; fi


# The next line enables shell command completion for gcloud.
if [ -f '/.local/lib/google-cloud-sdk/completion.bash.inc' ]; then . '~/.local/lib/google-cloud-sdk/completion.bash.inc'; fi
alias docker-compose=podman-compose
[ -e $HOME/.config/podman/auth.json ] && export REGISTRY_AUTH_FILE=$HOME/.config/podman/auth.json

alias ip="ip -c"

export _JAVA_OPTIONS="-Djava.io.tmpdir=/var/tmp/java $_JAVA_OPTIONS"
[ -d /opt/android-sdk/platform-tools ] && PATH=$PATH:/opt/android-sdk/platform-tools

[ -e /home/jonathan/.config/broot/launcher/bash/br ] && . /home/jonathan/.config/broot/launcher/bash/br
test -d /mnt/proton/dotnet && export DOTNET_ROOT=/mnt/proton/dotnet || ( test -d $HOME/.dotnet && export DOTNET_ROOT=$HOME/.dotnet )
test -d $DOTNET_ROOT && export PATH=${DOTNET_ROOT}:${DOTNET_ROOT}/tools:$PATH

# created by espup for rust esp programming
[ -f ~/export-esp.sh ] && . ~/export-esp.sh
