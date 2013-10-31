# My bashrc
# Misc.  #{{{1

umask 022  # Default permission
ulimit -c 0  # Don't create core file




# Parameters  #{{{1

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
  local t_main='$PS_DECORATOR> '
  # is nested interactive shell?
  if [ $SHLVL -gt 1 ]; then
    local t_shlvl=" $c_gray($SHLVL)$c_reset"
  else
    local t_shlvl=''
  fi
  local t_vcs="$c_yellow\$(prompt-git-head-name)$c_reset"

  PS1="
$t_host $t_cwd$t_shlvl $t_vcs
$t_main"

  local decorators=('AKARI.A' 'KYOKO.T' 'YUI.F' 'TINATSU.Y')
  PS_DECORATOR="${decorators[$(( $RANDOM % ${#decorators[@]} ))]}"
}

prompt_setup

unset -f prompt_setup




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




# Functions  #{{{1

if which git &>/dev/null; then
  prompt-git-head-name() {
    local git_dir="$(git rev-parse --git-dir 2>/dev/null)"
    if [ -z "$git_dir" ]; then
      return 1
    fi

    local head_name=''
    local additional_info=''
    if [ -d "$git_dir/rebase-apply" ]; then
      if [ -f "$git_dir/rebase-apply/rebasing" ]; then
        additional_info="REBASE"
      elif [ -f "$git_dir/rebase-apply/applying" ]; then
        additional_info="AM"
      else
        additional_info="AM/REBASE"
      fi
      head_name="$(git symbolic-ref HEAD 2>/dev/null)"
    elif [ -f "$git_dir/rebase-merge/interactive" ]; then
      additional_info="REBASE-i"
      head_name="$(< "$git_dir/rebase-merge/head-name")"
    elif [ -d "$git_dir/rebase-merge" ]; then
      additional_info="REBASE-m"
      head_name="$(< "$git_dir/rebase-merge/head-name")"
    elif [ -f "$git_dir/MERGE_HEAD" ]; then
      additional_info="MERGING"
      head_name="$(git symbolic-ref HEAD 2>/dev/null)"
    fi
    if [ -z "$head_name" ]; then
      head_name="$(git branch | sed -e 's/^\* //;t;d')"
      if [ "$head_name" = '(no branch)' ]; then
        # "git branch" doesn't show the correct name of a branch after
        # "git checkout {commitish-and-not-the-head-of-a-branch}",
        # so we have to use another method to get the name of {commitish}.
        head_name="($(
          {
            fgrep 'checkout: moving from ' .git/logs/HEAD |
            sed '$s/^.* to \([^ ]*\)$/\1/;t;d'
          } 2>/dev/null
        ))"
      elif [ "$head_name" = '' ]; then
        head_name='(just initialized; nothing commited)'
      fi
    else
      head_name="${head_name##refs/heads/}"
    fi
    if [ -n "$additional_info" ]; then
      additional_info="|$additional_info"
    fi

    echo "[$head_name$additional_info]"
    return 0
  }
else
  function prompt-git-head-name() {
    echo ''
  }
fi




# __END__  #{{{1
# vim: filetype=sh foldmethod=marker
