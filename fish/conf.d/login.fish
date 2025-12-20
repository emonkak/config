if not status is-login
  exit
end

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
