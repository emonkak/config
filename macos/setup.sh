#!/bin/bash

set -euo pipefail

if ! which brew &> /dev/null
then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

for f in terminfo/*.terminfo
do
  tic "$f"
done

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

cat <<EOF > /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask/Casks/hakumai.rb
cask 'hakumai' do
  version '2.11.0'
  sha256 '9d4206cfafb4be5f93d217420cba24e87d70dceaefe58b2edb4046c99f368932'

  url "https://hakumai.s3.amazonaws.com/Hakumai.2.11.0.zip"
  name 'Hakumai'
  homepage 'https://honishi.github.io/Hakumai/'

  app 'Hakumai.app'
end
EOF

brew install --cask \
  appcleaner \
  aquaskk \
  brave-browser \
  checkra1n \
  clipy \
  cooviewer \
  hakumai \
  iterm2 \
  karabiner-elements \
  licecap \
  macvim \
  microsoft-edge \
  mpv \
  resilio-sync \
  stats \
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
  massren \
  neovim \
  ripgrep \
  tig \
  tmux \
  tree \
  zsh \

defaults write -g ApplePressAndHoldEnabled -bool false
defaults write -g AppleShowAllExtensions -bool true
defaults write -g AppleShowScrollBars -string 'WhenScrolling'
defaults write -g NSAutomaticWindowAnimationsEnabled -bool false
defaults write -g QLPanelAnimationDuration -float 0

defaults write com.apple.CrashReporter DialogType -string "none"    
defaults write com.apple.LaunchServices LSQuarantine -bool false 
defaults write com.apple.Preview NSQuitAlwaysKeepsWindows -bool false
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool TRUE
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.finder _FXSortFoldersFirst -bool true
defaults write com.apple.screencapture disable-shadow -boolean true

# Keyboard
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2

# Trackpad
defaults write -g com.apple.trackpad.forceClick -bool true
defaults write -g com.apple.trackpad.threeFingerTapGesture -bool false
defaults write com.apple.AppleMultitouchTrackpad ActuateDetents -int 1
defaults write com.apple.AppleMultitouchTrackpad Clicking -int 1
defaults write com.apple.AppleMultitouchTrackpad ForceSuppressed -int 1
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerTapGesture -int 0
