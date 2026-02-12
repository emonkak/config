DOTFILES_SOURCES=\
  dotfiles/.gtkrc-2.0 \
  dotfiles/.inputrc \
  dotfiles/.my.cnf \
  dotfiles/.ssh \
  dotfiles/.xinitrc \
  dotfiles/.Xresources
DOTFILES_TARGETS=$(patsubst dotfiles/.%, $(HOME)/.%, $(DOTFILES_SOURCES))

CONFIG_SOURCES=\
  alacritty \
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
  $(DOTFILES_TARGETS) \
  $(CONFIG_TARGETS) \
  $(DATA_TARGETS) \
  $(HOME)/bin

all: $(ALL_TARGETS)

define GenerateLinkRule # (target, source)
$(1): $(2)
	ln -s $$(abspath $$<) $$@

endef

$(eval $(call GenerateLinkRule,$(HOME)/.%,dotfiles/.%))
$(eval $(call GenerateLinkRule,$(HOME)/.config/%,%))
$(eval $(call GenerateLinkRule,$(HOME)/.local/share/%,%))
$(eval $(call GenerateLinkRule,$(HOME)/%,%))

clean:
	@for target in $(ALL_TARGETS); \
	do \
	  if [ -L "$$target" ]; \
	  then \
	    rm -v "$$target"; \
	  fi \
	done

.PHONY: all clean
