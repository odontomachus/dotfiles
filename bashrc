# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Turn the **** bell off
set bell-style none

# The stuff bellow this will only apply to interactive shells
# exit if we're not running an interactive shell
[ -z "$PS1" ] && return

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

alias lpr='/usr/bin/lpr -o media=Letter -o page-top=72 -o page-left=72 -o page-right=72 -o page-bottom=72'
alias spwd='/bin/pwd > '$HOME'/.spwd'
alias lpwd='cd "`cat '$HOME'/.spwd`"'
alias memail='echo "" | mutt odontomachus@gmail.com'

export VISUAL=emacs
#export CDPATH=$CDPATH

alias user_shutdown='dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.ConsoleKit.UPower.Stop'

alias user_suspend='dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend'

touch ~/.sshagent
source ~/.sshagent > /dev/null
ssh-add -l &>/dev/null
if [[ "$?" = 2 ]] ; then
    ssh-agent -t 2400 > ~/.sshagent 2>/dev/null
    source ~/.sshagent > /dev/null
fi;
