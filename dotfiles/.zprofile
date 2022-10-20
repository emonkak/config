export PATH

if [ -d "$HOME/.cabal/bin" ]
then
  PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -d "$HOME/.composer/vendor/bin" ]
then
  PATH="$HOME/.composer/vendor/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]
then
  PATH="$HOME/.cargo/bin:$PATH"
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
  PATH="$GOPATH/bin:$PATH:"
fi

if [ -d "$HOME/android_sdk" ]
then
  export ANDROID_SDK_ROOT="$HOME/android_sdk"
  PATH="$ANDROID_SDK_ROOT/cmdline-tools/latest/bin:$ANDROID_SDK_ROOT/platform-tools:$PATH:"
fi

if which ruby >/dev/null && which gem >/dev/null; then
  PATH="$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

if [ -d "$HOME/depot_tools" ]
then
  PATH="$HOME/depot_tools:$PATH"
fi

PATH="$HOME/bin:$HOME/.local/bin:$HOME/opt/bin:$PATH"

export LANG='en_US.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export BROWSER='brave-bin'
export EDITOR='nvim'
export PAGER='less'

export LESS='-c -R -L'
export LV='-c -l'

export WINEARCH="win32"

export MPD_CONF="$HOME/.mpd/mpd.conf"


# Added by Toolbox App
export PATH="$PATH:/home/emonkak/.local/share/JetBrains/Toolbox/scripts"

