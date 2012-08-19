# My zprofile

export HAXEPATH="$HOME/haxe"
export HAXE_LIBRARY_PATH="$HAXEPATH/std"
export NEKOPATH="$HOME/neko"

export PATH="$HOME/bin:$HOME/.cabal/bin:$HOME/.gem/ruby/1.9.1/bin:$HAXEPATH:$NEKOPATH:$PATH"
export PATH="$HOME/bin:$HOME/.cabal/bin:$HOME/.gem/ruby/1.9.1/bin:$PATH"

export LANG='ja_JP.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export EDITOR='vim'
export PAGER='less'

export LESS='-R'
export LESSOPEN='| /usr/bin/src-hilite-lesspipe.sh %s'
export LV='-c -l'

export HTTP_PROXY='http://localhost:8118/'
export http_proxy=$HTTP_PROXY
export no_proxy='localhost,192.168.0.0/16'
