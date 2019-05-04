export PATH="$HOME/bin:$HOME/.composer/vendor/bin:$HOME/.cabal/bin:$HOME/.gem/ruby/2.2.0/bin:$HOME/.local/bin:$GOPATH/bin:$PATH"

export LANG='ja_JP.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export BROWSER='google-chrome-stable'
export EDITOR='vim'
export PAGER='less'

export LESS='-R'
export LV='-c -l'

export WINEARCH="win32"

export MPD_CONF="$HOME/.mpd/mpd.conf"

if [ -d "$HOME/go" ]
then
  export GOPATH="$HOME/go"
  PATH="$PATH:$GOPATH/bin"
fi

if [ -d "$HOME/android" ]
then
  export ANDROID_HOME="$HOME/android"
  PATH="$PATH:$ANDROID_HOME/tools/bin:$ANDROID_HOME/build-tools/26.0.1:$ANDROID_HOME/platform-tools"
fi

if [ -d "$HOME/.nvm" ]
then
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
fi
