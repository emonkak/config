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
  LINUX \
  VIM \

GROUP_DOTS_FILES=\
  dotfiles/dot.Xresources \
  dotfiles/dot.bashrc \
  dotfiles/dot.config/fontconfig/fonts.conf \
  dotfiles/dot.ghci \
  dotfiles/dot.inputrc \
  dotfiles/dot.mfiler3/mfiler3.sa \
  dotfiles/dot.screenrc \
  dotfiles/dot.tmux.conf \
  dotfiles/dot.uim \
  dotfiles/dot.xkb/keymap/my_keymap \
  dotfiles/dot.xkb/symbols/my_symbols \
  dotfiles/dot.xinitrc \
  dotfiles/dot.xmonad/xmonad.hs \
  dotfiles/dot.zprofile \
  dotfiles/dot.zshrc
GROUP_DOTS_RULE=$(patsubst dotfiles/dot.%, $(HOME)/.%, $(1))

GROUP_GENTOO_FILES=\
  gentoo/etc/X11/xorg.conf \
  gentoo/etc/fstab \
  gentoo/etc/portage/make.conf \
  gentoo/etc/portage/package.accept_keywords \
  gentoo/etc/portage/package.mask \
  gentoo/etc/portage/package.unmask \
  gentoo/etc/portage/package.use \
  gentoo/etc/portage/profile/package.provided \
  gentoo/var/lib/portage/world
GROUP_GENTOO_RULE=$(patsubst gentoo/%, /%, $(1))

GROUP_LINUX_FILES=\
  linux/dot.config
GROUP_LINUX_RULE=$(patsubst linux/dot.%, /usr/src/linux/.%, $(1))

GROUP_VIM_FILES=$(patsubst $(HOME)/.%, vim/dot.%, $(shell find $(HOME)/.vim/{after,autoload,compiler,dict,indent,syntax} -type f)) \
  vim/dot.vim/colors/basic256.vim \
  vim/dot.vimrc
GROUP_VIM_RULE=$(patsubst vim/dot.%, $(HOME)/.%, $(1))




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
