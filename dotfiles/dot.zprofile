# My zshenv

export HAXEPATH="$HOME/haxe"
export HAXE_LIBRARY_PATH="$HAXEPATH/std"
export NEKOPATH="$HOME/neko"

export LD_LIBRARY_PATH="$HOME/neko"

export PATH="$HOME/bin:$HOME/.cabal/bin:$HOME/.gem/ruby/2.0.0/bin:$HOME/node_modules/.bin:$HAXEPATH:$NEKOPATH:$PATH"

export LANG='ja_JP.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export EDITOR='vim'
export PAGER='lv'

export LESS='-R'
export LV='-c -l'

export HTTP_PROXY='http://localhost:8118/'
export http_proxy=$HTTP_PROXY
export no_proxy='localhost,192.168.0.0/16'

export GIT_MERGE_AUTOEDIT=no

export MPD_CONF="$HOME/.mpd/mpd.conf"

eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
