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
  OPERA \
  VIM

GROUP_DOTS_FILES=\
  .Xmodmap \
  .Xresources \
  .conky.lua \
  .conkyrc \
  .fonts.conf \
  .ghci \
  .mfiler3/mfiler3.sa \
  .screenrc \
  .tmux.conf \
  .vimrc \
  .xinitrc \
  .xmobarrc \
  .xmonad/xmonad.hs \
  .zprofile \
  .zshrc
GROUP_DOTS_RULE=$(patsubst .%, $(HOME)/.%, $(1))

GROUP_GENTOO_FILES=\
  gentoo/X11/xorg.conf \
  gentoo/portage/package.keywords \
  gentoo/portage/package.mask \
  gentoo/portage/package.unmask \
  gentoo/portage/package.use \
  gentoo/portage/profile/package.provided \
  gentoo/fstab \
  gentoo/make.conf
GROUP_GENTOO_RULE=$(patsubst gentoo/%, /etc/%, $(1))

GROUP_LINUX_FILES=\
  linux/.config
GROUP_LINUX_RULE=$(patsubst linux/%, /usr/src/linux/%, $(1))

GROUP_OPERA_FILES=\
  .opera/keyboard/my-keyboard.ini \
  .opera/menu/my-menu.ini \
  .opera/mouse/my-mouse.ini \
  .opera/styles/google.css \
  .opera/styles/ldr.css \
  .opera/styles/user.css \
  .opera/search.ini
GROUP_OPERA_RULE=$(patsubst .opera/%, $(HOME)/.opera/%, $(1))

GROUP_VIM_FILES=\
  .vim/after/plugin/metarw/http.vim \
  .vim/autoload/ku/colorscheme.vim \
  .vim/autoload/ku/register.vim \
  .vim/autoload/metarw/http.vim \
  .vim/autoload/metarw/sudo.vim \
  .vim/autoload/operator/comment.vim \
  .vim/autoload/ref/hayoo.vim \
  .vim/colors/basic256.vim \
  .vim/compiler/ghc.vim \
  .vim/compiler/javac.vim \
  .vim/compiler/tex.vim \
  .vim/plugin/operator/comment.vim \
  .vim/syntax/dmesg.vim \
  .vim/syntax/int-ghci.vim \
  .vim/syntax/int-gosh.vim \
  .vim/syntax/ref-hoogle.vim
GROUP_VIM_RULE=$(patsubst .vim/%, $(HOME)/.vim/%, $(1))




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
