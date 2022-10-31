# Makefile to manage vim packages
# Variables  {{{1

PACKAGE_URLS=\
  https://github.com/Julian/vim-textobj-variable-segment.git \
  https://github.com/Shougo/vimproc.git \
  https://github.com/b4b4r07/vim-shellutils.git \
  https://github.com/blueyed/vim-auto-programming \
  https://github.com/dhruvasagar/vim-table-mode.git \
  https://github.com/emonkak/vim-accelerate.git \
  https://github.com/emonkak/vim-ku.git \
  https://github.com/emonkak/vim-metarw-gist.git \
  https://github.com/emonkak/vim-operator-comment.git \
  https://github.com/emonkak/vim-operator-sort.git \
  https://github.com/emonkak/vim-surround.git \
  https://github.com/hail2u/vim-css3-syntax.git \
  https://github.com/junegunn/vim-easy-align.git \
  https://github.com/jwalton512/vim-blade.git \
  https://github.com/kana/vim-altercmd.git \
  https://github.com/kana/vim-arpeggio.git \
  https://github.com/kana/vim-exjumplist.git \
  https://github.com/kana/vim-gf-user.git \
  https://github.com/kana/vim-grex.git \
  https://github.com/kana/vim-metarw-git.git \
  https://github.com/kana/vim-metarw.git \
  https://github.com/kana/vim-niceblock.git \
  https://github.com/kana/vim-operator-replace.git \
  https://github.com/kana/vim-operator-user.git \
  https://github.com/kana/vim-scratch.git \
  https://github.com/kana/vim-skeleton.git \
  https://github.com/kana/vim-smartinput.git \
  https://github.com/kana/vim-smartword.git \
  https://github.com/kana/vim-submode.git \
  https://github.com/kana/vim-textobj-entire.git \
  https://github.com/kana/vim-textobj-fold.git \
  https://github.com/kana/vim-textobj-indent.git \
  https://github.com/kana/vim-textobj-jabraces.git \
  https://github.com/kana/vim-textobj-lastpat.git \
  https://github.com/kana/vim-textobj-line.git \
  https://github.com/kana/vim-textobj-syntax.git \
  https://github.com/kana/vim-textobj-user.git \
  https://github.com/koron/codic-vim.git \
  https://github.com/lambdalisue/suda.vim.git \
  https://github.com/mattn/webapi-vim.git \
  https://github.com/prabirshrestha/vim-lsp.git \
  https://github.com/rust-lang/rust.vim.git \
  https://github.com/thinca/vim-qfreplace.git \
  https://github.com/thinca/vim-quickrun.git \
  https://github.com/thinca/vim-ref.git \
  https://github.com/thinca/vim-textobj-between.git \
  https://github.com/tyru/operator-camelize.vim.git

PACKAGES=$(foreach url,$(PACKAGE_URLS),$(call PackageNameFromUrl,$(url)))

vim_auto_programming_BRANCH=neovim

ifndef MAKEFLAGS
MAKEFLAGS=--output-sync=target --jobs 4
endif




# Functions  {{{1

PackageNameFromUrl=$(notdir $(patsubst %.git,%,$(1)))
PackageUrlFromName=$($(call NormalizePackageName,$(1))_URL)
PackageBranchFromName=$($(call NormalizePackageName,$(1))_BRANCH)
NormalizePackageName=$(subst .,_,$(subst -,_,$(1)))

$(foreach url, \
  $(PACKAGE_URLS), \
  $(eval \
    $(call NormalizePackageName,$(call PackageNameFromUrl,$(url)))_URL=$(url)))

EMPTY=
SPACE=$(EMPTY) $(EMPTY)




# Rules  {{{1

define GenerateRule  # (name, url, branch)
install: $(1)

$(1):
ifneq ($(3),)
	@echo 'Installing' '$(1)@$(3)' '...'
	@git clone --branch '$(3)' '$(2)' '$(1)'
else
	@echo 'Installing' '$(1)' '...'
	@git clone '$(2)' '$(1)'
endif

update: $(1) update/$(1)

update/$(1):
	@echo 'Updating' $(1) '...'
	@git --git-dir '$(1)/.git' --work-tree '$(1)' pull --rebase

.PHONY: install update update/$(1)

endef

$(eval \
  $(foreach name, \
    $(PACKAGES), \
    $(call GenerateRule,$(name),$(call PackageUrlFromName,$(name)),$(call PackageBranchFromName,$(name)))))

all: install

list:
	@echo -e $(subst $(SPACE),\n,$(sort $(PACKAGES)))

orphans:
	@echo -e $(subst $(SPACE),\n,$(filter-out $(PACKAGES) $(MAKEFILE_LIST),$(wildcard *)))

.PHONY: all list orphans




# __END__  {{{1
# vim: foldmethod=marker
