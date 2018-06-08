# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# The stuff bellow this will only apply to interactive shells
# exit if we're not running an interactive shell
[ -z "$PS1" ] && return

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
	virtualenv-3 ~/virtualenv/"$@"
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

alias lpr='/usr/bin/lpr -o media=Letter -o page-top=72 -o page-left=72 -o page-right=72 -o page-bottom=72'
alias spwd='/bin/pwd > '$HOME'/.spwd'
alias lpwd='cd "`cat '$HOME'/.spwd`"'
alias memail='echo "" | mutt odontomachus@gmail.com'

export VISUAL=emacs
export JAVA_HOME=/usr/lib/jvm/java-openjdk
export GOPATH=$(go env GOPATH)
export PATH=$GOPATH/bin:$HOME/bin:$HOME/.Android/Sdk/tools:$PATH
#export CDPATH=$CDPATH

alias user_shutdown='dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.ConsoleKit.UPower.Stop'

alias user_suspend='dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend'

# If freshly updates
find ~/.sshagent -mmin -0.01 && sleep 0.3 &>/dev/null
[ -f .sshagent ] && (
    source ~/.sshagent > /dev/null
    # Give a chance for ssh-agent to initialize if freshly updated sshagent file is in place
    find ~/.sshagent -mmin -0.01 && ssh-add -l &>/dev/null || sleep 0.3 &>/dev/null
    ) || touch ~/.sshagent
source ~/.sshagent > /dev/null
ssh-add -l &>/dev/null
if [[ "$?" = 2 ]] ; then
    ssh-agent -t 36000 > ~/.sshagent 2>/dev/null
    source ~/.sshagent > /dev/null
fi;

# up down arrow key behavior
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export HISTSIZE=5000

export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"

# No accessibility bridge.
export NO_AT_BRIDGE=1

NPM_PACKAGES="${HOME}/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

ssh() {
    if [ "$(ps -p $(ps -p $$ -o ppid=) -o comm=)" = "tmux" ]; then
        tmux rename-window "$(echo $* | cut -d . -f 1)"
        command ssh "$@"
        tmux set-window-option automatic-rename "on" 1>/dev/null
    else
        command ssh "$@"
    fi
}
