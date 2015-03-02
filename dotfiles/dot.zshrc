# My zshrc
# Misc.  #{{{1

umask 022  # Default permission
ulimit -c 0  # Don't create core file
stty stop undef
stty start undef



# Parameters  #{{{1

HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000
SAVEHIST=100000


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
setopt glob_complete
setopt magic_equal_subst
setopt mark_dirs
setopt complete_in_word
setopt no_clobber

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




# Autoloads  #{{{1

autoload -Uz add-zsh-hook

# cdr  #{{{2

if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]
then
  autoload -Uz chpwd_recent_dirs cdr

  add-zsh-hook chpwd chpwd_recent_dirs

  mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/shell"

  zstyle ':completion:*:*:cdr:*:*' menu selection
  zstyle ':completion:*' recent-dirs-insert both
  zstyle ':chpwd:*' recent-dirs-max 500
  zstyle ':chpwd:*' recent-dirs-default true
  zstyle ':chpwd:*' recent-dirs-file "${XDG_CACHE_HOME:-$HOME/.cache}/shell/chpwd-recent-dirs"
  zstyle ':chpwd:*' recent-dirs-pushd true
fi




# compinit  #{{{2

autoload -Uz compinit

compinit

zstyle ':completion:*' completer _complete _expand _ignored _match _oldlist _prefix
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' use-cache true
zstyle ':completion:*' verbose true
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin




# select-word-style  #{{{2

autoload -Uz select-word-style

select-word-style default

zstyle ':zle:*' word-chars " _-./;@"
zstyle ':zle:*' word-style unspecified




# vcs_info  #{{{2

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' check-for-changes false
zstyle ':vcs_info:git:*' unstagedstr '!'
zstyle ':vcs_info:*' actionformats '[%s:%b%u|%a]'
zstyle ':vcs_info:*' formats '[%s:%b%u]'
zstyle ':vcs_info:bzr:*' use-simple true

add-zsh-hook precmd vcs_info




# zargs  #{{{2

autoload -Uz zargs




# zmv  #{{{2

autoload -Uz zmv
alias zmv='noglob zmv'




# zsh-syntax-highlighting  #{{{2

if [ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
  source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi




# Hooks  #{{{1

if [ -n "$WINDOW" ]  # is GNU screen
then
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
elif [[ -n "$TMUX" ]]
then
  precmd() {
    print -Pn "\e]0;%m@%n:%~\a"
    tmux show-environment | while read line
    do
      if [ $line[1] = '-' ]; then
        unset ${line#-*}
      else
        eval ${line// /\\ }
      fi
    done
  }
elif [[ "$TERM" == (xterm*|rxvt*) ]]
then
  precmd() {
    print -Pn "\e]0;%m@%n:%~\a"
  }
fi

# Highlight executed command
accept-line() {
  zle .accept-line && region_highlight=("0 ${#BUFFER} bold")
}
zle -N accept-line

# Adds node_modules/.bin to the PATH
npm_chpwd_hook() {
    if [ -n "${PRENPMPATH+x}" ]
    then
        PATH=$PRENPMPATH
        unset PRENPMPATH
    fi
    if [ -d node_modules/.bin ]
    then
        PRENPMPATH=$PATH
        PATH=$PWD/node_modules/.bin:$PATH
    fi
}

add-zsh-hook preexec npm_chpwd_hook




# Prompt  #{{{1

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
  local c_host="\$([ -n \"\$SSH_CONNECTION\" ] && echo \"$c_cyan\" || echo \"$c_green\")"

  local t_host="$c_user%n$c_host@%m$c_reset"
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

if ls --color=never --directory / >/dev/null 2>&1; then
  alias ls='ls -Fh --color=auto --show-control-chars'
else
  alias ls='ls -FGh'
fi
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
alias ln='ln -iv'

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
alias grep='grep --binary-files=without-match --color -E'
alias lv='lv -c'
alias pstree='pstree -A'

if which xsel &>/dev/null; then
  alias pbcopy='xsel --input --clipboard'
  alias pbpaste='xsel --output --clipboard'
fi

alias -s rb='ruby'
alias -s py='python'
alias -s hs='runhaskell'
alias -s php='php -f'
alias -s {gz,tar,zip,rar,7z}='aunpack'




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

if which peco &> /dev/null
then
  function peco-history() {
    local tac
    if which tac > /dev/null
    then
        tac='tac'
    else
        tac='tail -r'
    fi

    BUFFER=$(history -n 1 | eval $tac | peco --query "$LBUFFER")
    CURSOR=$#BUFFER

    zle clear-screen
  }

  function peco-cdr() {
    local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)

    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi

    zle clear-screen
  }

  zle -N peco-cdr
  zle -N peco-history

  bindkey '^Xc' peco-cdr
  bindkey '^Xh' peco-history
fi




# __END__  #{{{1
# vim: expandtab softtabstop=2 shiftwidth=2
# vim: foldmethod=marker
