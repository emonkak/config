# My zshrc
# Misc.  #{{{1

ulimit -c 0
stty -ixon -ixoff




# Parameters  #{{{1

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000




# Options  #{{{1

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent

setopt auto_list
setopt auto_menu
setopt auto_param_keys
setopt auto_param_slash
setopt list_packed
setopt list_types

setopt brace_ccl
setopt equals
setopt extended_glob
setopt magic_equal_subst
setopt mark_dirs

setopt append_history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_no_store
setopt hist_reduce_blanks
setopt inc_append_history
setopt share_history
unsetopt extended_history

setopt ignore_eof
setopt prompt_subst
unsetopt beep
unsetopt flow_control




# Prompt  #{{{1

function prompt_setup() {
  local user
  if [ "$USER" = 'root' ]; then
    user=$'%{\e[31m%}%n'
  else
    user=$'%{\e[32m%}%n'
  fi

  local host
  if [ -n "$SSH_CONNECTION" ]; then
    host=$'%{\e[36m%}%m'
  else
    host=$'%{\e[32m%}%m'
  fi

  local cwd=$'%{\e[33m%}%~'
  local vcs='$vcs_info_msg_0_'
  local main=$'%{\e[0m%}YUKI.N%(!.#.>) '

  PS1="$user@$host $cwd $vcs
$main"
}

prompt_setup
unset -f prompt_setup




# Title  #{{{1

autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '[%s:%b|%a]'
zstyle ':vcs_info:*' formats '[%s:%b]'
zstyle ':vcs_info:bzr:*' use-simple true

case "$TERM" in
xterm*|rxvt*|screen*)
  precmd() {
    vcs_info
    print -Pn "\e]0;%m@%n:%~\a"
  }
  ;;
esac




# Aliases  #{{{1

alias ls='ls -F --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias diff='colordiff -u'
alias grep='grep -E --line-number --color'
alias lv='lv -c'
alias pstree='pstree -A'

if which emerge &>/dev/null; then
  alias emerge='emerge -a'
fi


alias s='sudo '
if [ "$OSTYPE" = 'cygwin' ]; then
  alias v='vim'
else
  alias v='vim --servername VIM'
fi


alias mount-cifs='sudo mount -t cifs -o defaults,noatime,user,iocharset=utf8,uid=$USER,gid=users,file_mode=0644,dir_mode=0755,username=$USER'
alias untarbz2='tar -vxjf'
alias untargz='tar -vxzf'

if which xsel &>/dev/null; then
  alias pbcopy='xsel --input --clipboard'
  alias pbpaste='xsel --output --clipboard'
fi




# Functions  #{{{1
# mplayer-without-dpms  #{{{2

if which mplayer &>/dev/null && [ -n "$DISPLAY" ]; then
  function mplayer-without-dpms() {
    xset -dpms
    mplayer $@
    xset +dpms
  }
fi




# mkpasswd  #{{{2

if which apg &>/dev/null; then
  function mkpasswd() {
    local -A options
    options[-M]='SNCL'
    options[-n]=1

    while getopts 'l:m:n:rh' OPTION; do
      case $OPTION in
      l)
        options[-m]=$OPTARG
        options[-x]=$OPTARG
        ;;
      m)
        options[-M]=$OPTARG
        ;;
      n)
        options[-n]=$OPTARG
        ;;
      r)
        options[-a]=1
        ;;
      h|?)
        echo "Usage: $0 [-l length] [-m mode] [-n num_of_pass] [-r] [-h]" >&2
        echo "-l length       password length" >&2
        echo "-m mode         new style password modes [SNCL]" >&2
        echo "-n num_of_pass  generate num_of_pass passwords [1]" >&2
        echo "-r              random character password generation" >&2
        echo "-h              print this help screen" >&2
        return 1
        ;;
      esac
    done

    local -a args
    local command='apg'
    for key in ${(k)options}; do
      args+=($key $options[$key])
    done
    $command $args
  }
fi




# Line Editor  #{{{1

bindkey -e

bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

# Home / End
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line

# PageUp / PageDown
bindkey "\e[5~" history-search-backward
bindkey "\e[6~" history-search-forward

# Insert
bindkey "\e[2~" overwrite-mode

# Delete
bindkey "\e[3~" delete-char




# Completion  #{{{1

zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' use-cache true
zstyle ':completion:*' verbose true
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

autoload -U compinit
compinit




# __END__  #{{{1
# vim: expandtab softtabstop=2 shiftwidth=2
# vim: foldmethod=marker
