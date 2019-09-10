#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PROMPT_COMMAND='PS1X=$(p="${PWD#${HOME}}"; [ "${PWD}" != "${p}" ] && printf "~";IFS=/; for q in ${p:1}; do printf /${q:0:1}; done; printf "${q:1}")'
PS1='\[\033[34m\]▶\[\033[00m\]\u@\h ${PS1X} $ '

# Turn the **** bell off
set bell-style none

export GOPATH=$HOME/projects/go
export NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$HOME/.rvm/bin:$HOME/bin:$GOPATH/bin:$HOME/.cargo/bin:$NPM_PACKAGES/bin:$PATH" # Add cargo, RVM & GO to PATH for scripting

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

export VISUAL=emacs
export JAVA_HOME=/usr/lib/jvm/java-openjdk
#export "CLASSPATH=$CLASSPATH:$HOME/projects/vendor/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository"
#export CDPATH=$CDPATH

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

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion
export MAVEN_OPTS=-Xmx2048m
_gopass_bash_autocomplete() {
     local cur opts base
     COMPREPLY=()
     cur="${COMP_WORDS[COMP_CWORD]}"
     opts=$( ${COMP_WORDS[@]:0:$COMP_CWORD} --generate-bash-completion )
     local IFS=$'\n'
     COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
     return 0
 }

complete -F _gopass_bash_autocomplete gopass
source ~/.fzf.bash
