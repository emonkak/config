export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

export LANG='ja_JP.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export BROWSER='brave-bin'
export EDITOR='vim'
export PAGER='less'

export LESS='-c -R -L'
export LV='-c -l'

export WINEARCH="win32"

export MPD_CONF="$HOME/.mpd/mpd.conf"

if [ -d "$HOME/.cabal" ]
then
  PATH="$PATH:$HOME/.cabal/bin"
fi

if [ -d "$HOME/.composer" ]
then
  PATH="$PATH:$HOME/.composer/vendor/bin"
fi

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
  [ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
fi

if which ruby >/dev/null && which gem >/dev/null; then
  PATH="$PATH:$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"
fi
