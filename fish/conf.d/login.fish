if not status is-login
  exit
end

set --global --export LANG 'C.UTF-8'
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
  set --global --export FZF_DEFAULT_OPTS '--color=base16 --reverse'
end

if test -S "$XDG_RUNTIME_DIR/mpd/socket"
  set --global --export MPD_HOST "$XDG_RUNTIME_DIR/mpd/socket"
end

if test -S "$XDG_RUNTIME_DIR/ssh-agent.sock"
  set --global --export SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.sock"
end

if test -f "$XDG_RUNTIME_DIR/supervise-ssh-agent.pid"
  set --global --export SSH_AGENT_PID (read < "$XDG_RUNTIME_DIR/supervise-ssh-agent.pid")
end

fish_add_path ~/.cabal/bin ~/.cargo/bin ~/.composer/vendor/bin ~/.ghcup/bin

if test -d ~/.bun
  set --export BUN_INSTALL ~/.bun
  fish_add_path "$BUN_INSTALL/bin"
end

if test -d ~/.local/share/pnpm 
  set --global --export PNPM_HOME ~/.local/share/pnpm
  fish_add_path "$PNPM_HOME"
end

if test -d ~/go 
  set --global --export GOPATH ~/go
  fish_add_path "$GOPATH/bin"
end

if type --query brew
  for bin_dir in (brew --prefix)/opt/*/libexec/gnubin
    fish_add_path "$bin_dir"
  end
end

fish_add_path --move ~/bin ~/.local/bin ~/opt/bin 
