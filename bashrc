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

# Turn the **** bell off
set bell-style none

alias spwd='/bin/pwd > '$HOME'/.spwd'
alias lpwd='cd "`cat '$HOME'/.spwd`"'
alias va='. .venv/bin/activate'
alias ip="ip -c"

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

export HISTSIZE=10000

export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"

# No accessibility bridge.
export NO_AT_BRIDGE=1

[ -e $HOME/.config/podman/auth.json ] && export REGISTRY_AUTH_FILE=$HOME/.config/podman/auth.json

export _JAVA_OPTIONS="-Djava.io.tmpdir=/var/tmp/java $_JAVA_OPTIONS"
export PATH

# created by espup for rust esp programming
[ -f ~/export-esp.sh ] && . ~/export-esp.sh

command -v asdf &>/dev/null && . <(asdf completion bash)

[ -f ~/.work.env ] && . ~/.work.env

export NVM_DIR="$HOME/.nvm"

# Helper function to load the real NVM and completions
_lazy_load_nvm() {
  # Unset placeholder functions to prevent infinite loops
  unset -f nvm node npm npx yarn corepack 2>/dev/null || true
  unfunction nvm node npm npx yarn corepack 2>/dev/null || true # For Zsh compatibility

  # Load NVM
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

  # Load Bash/Zsh Completions
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

# Create placeholder functions for Node-related commands
nvm() { _lazy_load_nvm; nvm "$@"; }
node() { _lazy_load_nvm; node "$@"; }
npm() { _lazy_load_nvm; npm "$@"; }
npx() { _lazy_load_nvm; npx "$@"; }
yarn() { _lazy_load_nvm; yarn "$@"; }
corepack() { _lazy_load_nvm; corepack "$@"; }

