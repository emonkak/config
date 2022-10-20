# Makefile to update config.
# Generic  #{{{1

all: update
.PHONY: \
  all \
  update




# Group definitions  #{{{1

ALL_GROUPS=\
  DOTS \
  GENTOO \
  VIM \

GROUP_DOTS_FILES=\
  dotfiles/.Xresources \
  dotfiles/.bashrc \
  dotfiles/.config/alacritty/alacritty.yml \
  dotfiles/.config/fontconfig/fonts.conf \
  dotfiles/.config/git/config \
  dotfiles/.config/git/ignore \
  dotfiles/.config/tmux/tmux.conf \
  dotfiles/.ghc/ghci.conf \
  dotfiles/.inputrc \
  dotfiles/.mfiler3/mfiler3.sa \
  dotfiles/.uim \
  dotfiles/.xinitrc \
  dotfiles/.xkb/keymap/my_keymap \
  dotfiles/.xkb/symbols/my_symbols \
  dotfiles/.zprofile \
  dotfiles/.zshrc
GROUP_DOTS_RULE=$(patsubst dotfiles/%, $(HOME)/%, $(1))

GROUP_GENTOO_FILES=\
  gentoo/etc/X11/xorg.conf \
  gentoo/etc/fstab \
  gentoo/etc/portage/make.conf \
  gentoo/etc/portage/package.accept_keywords \
  gentoo/etc/portage/package.mask \
  gentoo/etc/portage/package.unmask \
  gentoo/etc/portage/package.use \
  gentoo/etc/portage/profile/package.provided \
  gentoo/usr/src/linux/.config \
  gentoo/var/lib/portage/world \
  $(patsubst %, \
    gentoo/%, \
    $(shell find /etc/portage/patches -type f))
GROUP_GENTOO_RULE=$(patsubst gentoo/%, /%, $(1))

GROUP_VIM_FILES=\
  vim/bundle/Makefile \
  vim/vimrc \
  $(patsubst $(HOME)/.vim/%, \
    vim/%, \
    $(shell find \
      $(wildcard $(patsubst %,$(HOME)/.vim/%,\
        after autoload colors compiler ftplugin indent plugin syntax)) -type f))
GROUP_VIM_RULE=$(patsubst vim/%, $(HOME)/.vim/%, $(1))




# update  #{{{1
# Rules for `update' are generated by eval.

RemoveCurrentDirectory=$(filter-out ./,$(1))
RemoveDuplicates=$(sort $(1))
CallRule=$(call RemoveCurrentDirectory,$(call GROUP_$(1)_RULE,$(2)))

define GenerateRulesToUpdateFile  # (src, dest)
update: $(2)
$(2): $(1)
	@cp '$$<' '$$@'
	@echo 'update:' '$$@'

endef

define GenerateRulesToUpdateDirectory  # (dest)
update: $(1)
$(1):
	@mkdir -p '$$@'

endef

define GenerateRulesFromGroups  # (groups = (group_name*))
$(foreach directory, \
  $(call RemoveDuplicates, \
    $(foreach group, \
      $(1), \
      $(dir $(GROUP_$(group)_FILES)))), \
  $(call GenerateRulesToUpdateDirectory,$(directory)))
$(foreach group, \
  $(1), \
  $(foreach file, \
    $(GROUP_$(group)_FILES), \
    $(call GenerateRulesToUpdateFile,$(call CallRule,$(group),$(file)),$(file))))
endef

$(eval $(call GenerateRulesFromGroups,$(ALL_GROUPS)))




# __END__  #{{{1
# vim: foldmethod=marker
