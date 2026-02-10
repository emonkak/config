DOTFILE_SOURCES=\
  dotfiles/.gtkrc-2.0 \
  dotfiles/.inputrc \
  dotfiles/.my.cnf \
  dotfiles/.ssh \
  dotfiles/.xinitrc \
  dotfiles/.Xresources
DOTFILE_TARGETS=$(patsubst dotfiles/.%, $(HOME)/.%, $(DOTFILE_SOURCES))

CONFIG_SOURCES=\
  alacritty \
  cabal \
  fish/conf.d \
  fish/config.fish \
  fish/functions \
  fontconfig \
  ghc \
  git \
  libskk \
  mpv \
  ncmpcpp \
  nvim \
  picom \
  polybar \
  tig \
  tmux \
  vifm \
  vim \
  xkb \
  xmonad
CONFIG_TARGETS=$(patsubst %, $(HOME)/.config/%, $(CONFIG_SOURCES))

DATA_SOURCES=\
  skk
DATA_TARGETS=$(patsubst %, $(HOME)/.local/share/%, $(DATA_SOURCES))

ALL_TARGETS=\
  $(CONFIG_TARGETS) \
  $(DATA_TARGETS) \
  $(DOTFILE_TARGETS)

all: $(ALL_TARGETS)

$(HOME)/.%: dotfiles/.%
	ln -s $(abspath $<) $@

$(HOME)/.config/%: %
	ln -s $(abspath $<) $@

$(HOME)/.local/share/%: %
	ln -s $(abspath $<) $@

clean:
	unlink -f $(ALL_TARGETS)

.PHONY: all clean
