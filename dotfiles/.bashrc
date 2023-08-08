# My bashrc
# Misc.  #{{{1

umask 022  # Default permission
ulimit -c 0  # Don't create core file

# Options  #{{{1

HISTSIZE=10000  # History size at runtime
HISTFILESIZE=$HISTSIZE  # History size to save
HISTCONTROL=ignoreboth
IGNOREEOF=10

# Prompt  #{{{1

prompt_setup() {
  local c_reset='\[\e[0m\]'
  local c_cyan='\[\e[36m\]'
  local c_green='\[\e[32m\]'
  local c_red='\[\e[31m\]'
  local c_yellow='\[\e[33m\]'
  local c_gray='\[\e[37m\]'

  local c_user
  case "$USER" in
    root) c_user="$c_red" ;;
    *) c_user="$c_green" ;;
  esac

  local c_host
  if [ -n "$SSH_CONNECTION" ]; then
    c_host="$c_cyan"
  else
    c_host="$c_green"
  fi

  local t_host="$c_user\\u$c_reset$c_host@\\h$c_reset"
  local t_cwd="$c_yellow\\w$c_reset"
  if [ $EUID -le 0 ]; then  # is running with privileges
    local t_main='# '
  else
    local t_main='$ '
  fi

  if [ $SHLVL -gt 1 ]; then  # is nested interactive shell?
    local t_shlvl=" $c_gray($SHLVL)$c_reset"
  else
    local t_shlvl=''
  fi

  PS1="$t_host $t_cwd$t_shlvl
$t_main"
}

prompt_setup

unset -f prompt_setup

# Aliases  #{{{1

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
if which nvim &>/dev/null
then
  alias v='nvim'
else
  alias v='vim'
fi

# __END__  #{{{1
# vim: filetype=sh foldmethod=marker
