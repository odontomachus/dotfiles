if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH
test -d /opt/android-sdk/platform-tools && /opt/android-sdk/platform-tools:$PATH

test -f "$HOME/.cargo/env" && . "$HOME/.cargo/env"

test -d /mnt/proton/dotnet && export DOTNET_ROOT=/mnt/proton/dotnet || ( test -d $HOME/.dotnet && export DOTNET_ROOT=$HOME/.dotnet )
test -d $DOTNET_ROOT && export PATH=${DOTNET_ROOT}:${DOTNET_ROOT}/tools:$PATH
test -d $DOTNET_ROOT/tools && PATH="$DOTNET_ROOT/tools:$PATH"

# bun
export BUN_INSTALL="$HOME/.bun"
test -d $BUN_INSTALL && export PATH="$BUN_INSTALL/bin:$PATH"

[ -d "${ASDF_DATA_DIR:-$HOME/.asdf}/shims" ] && export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
# The next line updates PATH for the Google Cloud SDK.
if [ -d "$HOME/go/bin" ]; then PATH="$PATH:$HOME/go/bin"; fi
export NVM_DIR="/home/jonathan/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
nvm use stable &> /dev/null &
