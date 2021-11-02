#!/bin/bash

set -euo pipefail

if ! which brew &> /dev/null
then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

if [ ! -f /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/cooviewer.rb ]
then
  tee /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/cooviewer.rb &> /dev/null <<EOL
cask 'cooviewer' do
  version '1.2b25'
  sha256 'b085124c540ce6e7cf2f8dbf1c2a0dc7ae47133d9afbc73357ac98ea168cf09e'

  url "https://github.com/coo-ona/cooViewer/releases/download/#{version}/cooViewer.zip"
  name 'cooViewer'
  homepage 'https://coo-ona.github.io/cooViewer/'

  app 'cooViewer/cooViewer.app'
end
EOL
fi

brew install --cask \
  altserver \
  aquaskk \
  bettertouchtool \
  brave-browser \
  cooviewer \
  iterm2 \
  karabiner-elements \
  menumeters \
  resilio-sync \
  the-unarchiver \
  transmission \

brew install \
  coreutils \
  diffutils \
  findutils \
  gawk \
  git \
  git-delta \
  gnu-sed \
  gnu-tar \
  grep \
  gzip \
  tig \
  tmux \
  vim \
  zsh \

defaults write -g ApplePressAndHoldEnabled -bool false
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2
