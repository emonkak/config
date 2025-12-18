# Prelude  {{{1

if not status is-interactive
  exit
end

# Set default permissions
umask 022

# Do not create core dumps
ulimit -c 0

# Disable Ctrl-S and Ctrl-Q keys
stty stop undef
stty start undef

# Exports  {{{1

set --global --export LANG 'en_US.UTF-8'
set --global --export LC_MESSAGES 'C'
set --global --export LC_TIME 'C'

if type --query nvim
  set --global --export EDITOR 'nvim'
else if type --query vim
  set --global --export EDITOR 'vim'
end

if type --query less
  set --global --export PAGER 'less'
  set --global --export LESS '-c -R -L'
end

if type --query fzf
  set --global --export FZF_DEFAULT_OPTS '--reverse --color=base16'
end

for browser in brave-browser-stable microsoft-edge-stable google-chrome-stable
  if type --query $browser
    set --global --export BROWSER $browser
    set --global --export CHROME_PATH (type -p $browser)
    break
  end
end

if status --is-login
  fish_add_path ~/.cabal/bin ~/.cargo/bin ~/.composer/vendor/bin ~/.ghcup/bin

  if test -d ~/go 
    set --global --export GOPATH ~/go
    fish_add_path "$GOPATH/bin"
  end

  if test -d ~/.local/share/pnpm 
    set --global --export PNPM_HOME ~/.local/share/pnpm
    fish_add_path "$PNPM_HOME"
  end

  if type --query brew
    for bin_dir in (brew --prefix)/opt/*/libexec/gnubin
      fish_add_path "$bin_dir"
    end
  end

  fish_add_path -m ~/bin ~/.local/bin ~/opt/bin 
end

# Options  {{{1

# Disable welcome message
set --unexport fish_greeting

function fish_prompt
  set --local user_color
  set --local host_color
  set --local prompt_character
  set --local last_status $status

  if test $EUID -eq 0 
    set user_color (set_color red)
    set prompt_character '#'
  else
    set user_color (set_color green)
    set prompt_character '$'
  end

  if test -n "$SSH_CONNECTION"
    set host_color (set_color cyan)
  else
    set host_color (set_color white)
  end

  set --local level $SHLVL

  if test -n "$TMUX"
    set level (math $level - 1)
  end

  if test $level -gt 1 
    set prompt_character (string repeat -n $level "$prompt_character")
  end

  set --local cwd_prompt ' ' (prompt_pwd --full-length-dirs=8)
  set --local vcs_prompt  (fish_vcs_prompt)
  set --local status_prompt

  if test $last_status -ne 0
    set status_prompt " [$last_status]"
  end

  echo -n -s \
    $user_color $USER \
    $host_color @ $hostname \
    (set_color yellow) $cwd_prompt \
    (set_color blue) $vcs_prompt \
    (set_color red) $status_prompt \n \
    (set_color normal) $prompt_character ' '
end

function fish_right_prompt
  echo -n -s (set_color white) (date '+%T')
end

# Abbreviations  {{{1

# Single key
abbr --add - 'cd -'
abbr --add c command
abbr --add g git
abbr --add s sudo
abbr --add t tmux
if type --query nvim
  abbr --add v nvim
else if type --query vim
  abbr --add v vim
else
  abbr --add v vi
end

abbr --add la 'ls -a'
abbr --add ll 'ls -l'
abbr --add lla 'ls -la'

# Aliases  {{{1

# Coreutils
alias cp 'cp --interactive --verbose'
alias ln 'ln --interactive --verbose'
if type --query eza
  alias ls 'eza --classify --group --time-style=long-iso'
else if type -q vdir # It is only available in GNU coreutils.
  alias ls 'ls --classify --human-readable --color=auto --time-style=long-iso --show-control-chars'
else
  alias ls 'ls -FGh'
end
alias mv 'mv --interactive --verbose'
if type --query safe-rm
  alias rm 'safe-rm --interactive=once --verbose'
else
  alias rm 'rm --interactive=once --verbose'
end
alias rmdir 'rmdir --verbose'

if type --query ffmpeg
  alias ffmpeg 'ffmpeg -hide_banner'
  alias ffprobe 'ffprobe -hide_banner'
end

if not type --query pbcopy && type --query xclip
  alias pbcopy 'xclip -i -selection clipboard'
  alias pbpaste 'xclip -o -selection clipboard'
end

# Plugins  {{{1
# fzf   {{{2

if type --query fzf_configure_bindings
  fzf_configure_bindings \
    --directory=\cx\cd \
    --git_log=\cx\cl \
    --git_status=\cx\cs \
    --history=\cx\ch \
    --processes=\cx\cp \
    --variables=\cx\cv
end

# __END__ {{{1
# vim: expandtab softtabstop=2 shiftwidth=2
# vim: foldmethod=marker
