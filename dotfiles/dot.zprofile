export PATH="$HOME/bin:$HOME/.local/bin:$HOME/opt/bin:$PATH"

export LANG='en_US.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export BROWSER='brave-bin'
export EDITOR='vim'
export PAGER='less'

export LESS='-c -R -L'
export LV='-c -l'

export WINEARCH="win32"

export MPD_CONF="$HOME/.mpd/mpd.conf"

if [ -d "$HOME/.cabal/bin" ]
then
  PATH="$PATH:$HOME/.cabal/bin"
fi

if [ -d "$HOME/.composer/vendor/bin" ]
then
  PATH="$PATH:$HOME/.composer/vendor/bin"
fi

if [ -d "$HOME/.cargo/bin" ]
then
  PATH="$PATH:$HOME/.cargo/bin"
fi

if [ -d "$HOME/.nvm" ]
then
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
  [ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
fi

if [ -d "$HOME/go" ]
then
  export GOPATH="$HOME/go"
  PATH="$PATH:$GOPATH/bin"
fi

if [ -d "$HOME/android_sdk" ]
then
  export ANDROID_SDK_ROOT="$HOME/android_sdk"
  PATH="$PATH:$ANDROID_SDK_ROOT/cmdline-tools/latest/bin:$ANDROID_SDK_ROOT/platform-tools"
fi

if which ruby >/dev/null && which gem >/dev/null; then
  PATH="$PATH:$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"
fi

if [ -d "$HOME/depot_tools" ]
then
  PATH="$PATH:$HOME/depot_tools"
fi
