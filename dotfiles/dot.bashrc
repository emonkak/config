# My bashrc
# Misc.  #{{{1

umask 077  # Default permission
ulimit -c 0  # Don't create core file




# Parameters  #{{{1

HISTSIZE=10000  # History size at runtime
HISTFILESIZE=$HISTSIZE  # History size to save
HISTCONTROL=ignoreboth
IGNOREEOF=10




# PROMPT  #{{{1

_set_up_prompt() {
  local _c_reset='\[\e[0m\]'
  local _c_cyan='\[\e[36m\]'
  local _c_green='\[\e[32m\]'
  local _c_red='\[\e[31m\]'
  local _c_yellow='\[\e[33m\]'

  local _c_user
  case "$USER" in
    root) _c_user="$_c_red" ;;
    *) _c_user="$_c_green" ;;
  esac
  local _c_host
  case "$HOSTNAME" in
    winter) _c_host="$_c_cyan" ;;
    *)
      if [ "$ENV_WORKING" != "$ENV_ACCESS" ]; then
        _c_host="$_c_cyan"
      else
        _c_host="$_c_green"
      fi
      ;;
  esac

  local _prompt_title='\[\e]0;\u@\h \w\007\]'
  local _prompt_host="$_c_user\\u$_c_reset$_c_host@\\h$_c_reset"
  local _prompt_cwd="$_c_yellow\\w$_c_reset"
  local _prompt_main='YUKI.N> '
  if [ -n "$WINDOW" ]; then  # auto-title in GNU screen
    local _prompt_auto='\ek\e\\'
  else
    local _prompt_auto=''
  fi

  PS1="$_prompt_auto$_prompt_title
$_prompt_host $_prompt_cwd
$_prompt_main"
}

_set_up_prompt

unset -f _set_up_prompt




# Alias  #{{{1

alias ls='ls -F --show-control-chars --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'

alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'

alias ..='cd ..'
alias cd='pushd > /dev/null'

alias diff='diff -u'

alias g='git'
alias v='vim'




# __END__  #{{{1
# vim: filetype=sh foldmethod=marker
