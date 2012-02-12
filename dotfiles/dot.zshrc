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

# Highlight executed command
accept-line() {
  zle .accept-line && region_highlight=("0 ${#BUFFER} bold")
}
zle -N accept-line

function prompt_setup() {
  local c_reset=$'\e[0m'
  local c_cyan=$'\e[36m'
  local c_green=$'\e[32m'
  local c_red=$'\e[31m'
  local c_yellow=$'\e[33m'
  local c_gray=$'\e[37m'

  local c_user
  case "$USER" in
    root)
      c_user="$c_red"
      ;;
    *)
      c_user="$c_green"
      ;;
  esac
  local c_host
  if [ -n "$SSH_CONNECTION" ]; then
    c_host="$c_cyan"
  else
    c_host="$c_green"
  fi

  local t_host="$c_user%n$c_reset$c_host@%m$c_reset"
  local t_cwd="$c_yellow%~$c_reset"
  local t_main='$PS_DECORATOR%(!.#.>) '
  if [ 1 -lt $SHLVL ]; then  # is nested interactive shell?
    local t_shlvl=" $c_gray($SHLVL)$c_reset"
  else
    local t_shlvl=''
  fi
  local t_vcs="$c_yellow\$vcs_info_msg_0_$c_reset"

  PS1="
$t_host $t_cwd$t_shlvl $t_vcs
$t_main"
  PS_DECORATOR='YUKI.N'
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
if which safe-rm &>/dev/null; then
  alias rm='safe-rm -Iv'
else
  alias rm='rm -Iv'
fi

autoload zmv
alias zmv='noglob zmv'

alias git='noglob git'
alias g='git'
alias sudo='sudo '
alias s='sudo'
if which vim &>/dev/null && vim --version | grep -q +X11; then
  alias vim='vim --servername VIM'
fi
alias v='vim'

if which colordiff &>/dev/null; then
  alias diff='colordiff -u'
fi
alias grep='grep --color -E'
alias lv='lv -c'
alias pstree='pstree -A'

alias mount-cifs='sudo mount -t cifs -o defaults,noatime,user,iocharset=utf8,uid=$USER,gid=users,file_mode=0644,dir_mode=0755,username=$USER'

if which xsel &>/dev/null; then
  alias pbcopy='xsel --input --clipboard'
  alias pbpaste='xsel --output --clipboard'
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




# Additional  #{{{1

autoload zargs

if [ -f "$HOME/.zsh/zaw/zaw.zsh" ]; then
  source ~/.zsh/zaw/zaw.zsh
  bindkey '^Xh' zaw-history
  bindkey '^Xc' zaw-dirstack
  bindkey '^Xg' zaw-git-files
fi




# __END__  #{{{1
# vim: expandtab softtabstop=2 shiftwidth=2
# vim: foldmethod=marker
