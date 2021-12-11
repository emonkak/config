#!/bin/bash

set -euo pipefail

if ! which brew &> /dev/null
then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

if [ ! -f /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/cooviewer.rb ]
then
  cat <<EOF > /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/cooviewer.rb
cask 'cooviewer' do
  version '1.2b25'
  sha256 'b085124c540ce6e7cf2f8dbf1c2a0dc7ae47133d9afbc73357ac98ea168cf09e'

  url "https://github.com/coo-ona/cooViewer/releases/download/#{version}/cooViewer.zip"
  name 'cooViewer'
  homepage 'https://coo-ona.github.io/cooViewer/'

  app 'cooViewer/cooViewer.app'
end
EOF
fi

if [ ! -f /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/hakumai.rb ]
then
  cat <<EOF > /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/hakumai.rb
cask 'hakumai' do
  version '2.2.0'
  sha256 '6df26a31b9220302fbe796cec8890055396f02cffcce05181262792358498b7e'

  url "https://hakumai.s3.amazonaws.com/Hakumai.2.2.0.zip"
  name 'Hakumai'
  homepage 'https://honishi.github.io/Hakumai/'

  app 'Hakumai.app'
end
EOF
fi

brew install --cask \
  altserver \
  appcleaner \
  aquaskk \
  bettertouchtool \
  brave-browser \
  checkra1n \
  clipy \
  cooviewer \
  hakumai \
  impactor \
  iterm2 \
  karabiner-elements \
  licecap \
  menumeters \
  resilio-sync \
  the-unarchiver \
  transmission \
  vlc \

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
  make \
  tig \
  tmux \
  vim \
  zsh \

defaults write -g ApplePressAndHoldEnabled -bool false
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2
