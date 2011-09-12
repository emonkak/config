# My zshrc
# Misc.  #{{{1

ulimit -c 0
stty stop undef
stty start undef



# Parameters  #{{{1

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000


# Don't add "rm" and "rmdir" to history.
zshaddhistory() {
  local -a args; args=(${(z)1})
  local cmd
  if [[ $args[1] = s(udo|) ]]; then
    cmd=$args[2]
  else
    cmd=$args[1]
  fi
  [[ $cmd != rm(|dir) ]]
}




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




# Title  #{{{1

if [ -n "$WINDOW" ]; then  # is GNU screen
  preexec() {
    local -a cmd; cmd=(${(z)2})
    case "$cmd[1]" in
    sudo)
      cmd=$cmd[2]
      ;;
    fg|%*)
      local -A jt; jt=(${(kv)jobtexts})
      cmd=${(z)${(e):-\$jt$num}}
      ;;
    *)
      cmd=$cmd[1]
      ;;
    esac
    print -Pn "\ek$cmd:t\e\\"
  }
  precmd() {
    print -Pn "\ek$ZSH_NAME\e\\"
  }
elif [[ -n "$TMUX" ]]; then
  precmd() {
    print -Pn "\e]0;%m@%n:%~\a"
    print -n "\a"
  }
elif [[ "$TERM" == (xterm*|rxvt*) ]]; then
  precmd() {
    print -Pn "\e]0;%m@%n:%~\a"
  }
fi




# Prompt  #{{{1

autoload -Uz add-zsh-hook
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '[%s:%b|%a]'
zstyle ':vcs_info:*' formats '[%s:%b]'
zstyle ':vcs_info:bzr:*' use-simple true

add-zsh-hook precmd vcs_info

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

  PS1="
$user@$host $cwd $vcs
$main"
}

prompt_setup
unset -f prompt_setup


accept-line() {
  zle .accept-line && region_highlight=("0 ${#BUFFER} bold")
}
zle -N accept-line




# Aliases  #{{{1

alias ls='ls -F --show-control-chars --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'

alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'

autoload zmv
alias zmv='noglob zmv'

alias diff='colordiff -u'
alias git='noglob git'
alias grep='grep --color -E'
alias lv='lv -c'
alias pstree='pstree -A'
alias sudo='sudo '

alias g='git'
alias s='sudo'
alias v='vim'

alias mount-cifs='sudo mount -t cifs -o defaults,noatime,user,iocharset=utf8,uid=$USER,gid=users,file_mode=0644,dir_mode=0755,username=$USER'

if [ -n "$DISPLAY" ] && which xsel &>/dev/null; then
  alias pbcopy='xsel --input --clipboard'
  alias pbpaste='xsel --output --clipboard'
fi

if [ -n "$DISPLAY" ] && which mplayer &>/dev/null; then
  alias mplayer-webcam='mplayer tv:// -tv driver=v4l2:device=/dev/video0:alsa:adevice=hw.1:forceaudio:immediatemode=0:width=1280:height=720:fps=30'
  alias mencoder-webcam='mencoder tv:// -tv driver=v4l2:device=/dev/video0:alsa:adevice=hw.1:forceaudio:immediatemode=0:width=1280:height=720:fps=30 -ovc x264 -x264encopts bitrate=1000:threads=2 -oac faac -faacopts br=64 -vf scale=640:360,harddup -o'
fi




# Line Editor  #{{{1

bindkey -e

bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward

bindkey "\eh" backward-delete-word  # <A-h>

bindkey "\e[1~" beginning-of-line  # <Home>
bindkey "\e[2~" overwrite-mode  # <Insert>
bindkey "\e[3~" delete-char  # <Delete>
bindkey "\e[4~" end-of-line  # <End>
bindkey "\e[5~" history-search-backward  # <Up>
bindkey "\e[6~" history-search-forward  # <Down>
bindkey "\e[7~" beginning-of-line  # <Home>
bindkey "\e[8~" end-of-line  # <End>
bindkey "\e[F" end-of-line  # <End>
bindkey "\e[H" beginning-of-line  # <Home>


autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " _-./;@"
zstyle ':zle:*' word-style unspecified




# Completion  #{{{1

autoload -U compinit
compinit
zstyle ':completion:*' completer _complete _expand _ignored _oldlist
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' use-cache true
zstyle ':completion:*' verbose true
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin




# Source  #{{{1

source ~/.zsh/zaw/zaw.zsh

bindkey '^Xh' zaw-history
bindkey '^Xc' zaw-dirstack
bindkey '^Xg' zaw-git-files




# __END__  #{{{1
# vim: expandtab softtabstop=2 shiftwidth=2
# vim: foldmethod=marker
