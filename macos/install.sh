#!/bin/bash

set -euo pipefail

mklink() {
  mkdir --parents "${2%/*}"
  ln --force --symbolic --no-target-directory --verbose $(realpath "${1}") "${2}"
}

install-brew() {
  echo Install brew...

  if ! which brew &> /dev/null
  then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

install-brew-formulas() {
  echo Install brew formulas...

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
    zsh
}

install-brew-casks() {
  echo Install brew casks...

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
  version '3.8.0'
  sha256 "4e9c911a02cb45f8c92d8a05d7cc7f943283eea491c05b8071536b4a915b2346"

  url "https://hakumai.s3.amazonaws.com/Hakumai.#{version}.zip"
  name 'Hakumai'
  homepage 'https://honishi.github.io/Hakumai/'

  app 'Hakumai.app'
end
EOF

  brew install --cask \
    appcleaner \
    aquaskk \
    checkra1n \
    clipy \
    cooviewer \
    hakumai \
    karabiner-elements \
    licecap \
    macvim \
    microsoft-edge \
    mpv \
    qlvideo \
    raycast \
    resilio-sync \
    stats \
    the-unarchiver \
    transmission \
    vlc
}

install-terminfos() {
  echo Install terminfos...

  tic /dev/stdin <<EOF
tmux-256color|tmux with 256 colors and italic,
	ritm=\E[23m,
	rmso=\E[27m,
	sgr=\E[0%?%p6%t;1%;%?%p1%t;7%;%?%p2%t;4%;%?%p3%t;7%;%?%p4%t;5%;m%?%p9%t\016%e\017%;,
	sitm=\E[3m,
	smso=\E[7m,
	use=screen-256color,
EOF

  tic /dev/stdin <<EOF
xterm-256color|xterm with 256 colors and italic,
	sitm=\E[3m,
	ritm=\E[23m,
	use=xterm-256color,
EOF
}

install-launch-daemons() {
  echo Install launch daemons...

  sudo install -m644 LaunchDaemons/setup-locale.plist /Library/LaunchDaemons/setup-locale.plist
  sudo install -m755 LaunchDaemons/setup-locale.sh /Library/LaunchDaemons/setup-locale.sh

  sudo launchctl bootout system /Library/LaunchDaemons/setup-locale.plist &> /dev/null || true
  sudo launchctl bootstrap system /Library/LaunchDaemons/setup-locale.plist || true
}

install-configs() {
  echo Install configs...

  mklink "AquaSKK" "${HOME}/Library/Application Support/AquaSKK"
  mklink "Hammerspoon" "${HOME}/.hammerspoon"
  mklink "Karabiner" "${HOME}/.config/karabiner"
}

configure-user-defaults() {
  echo Configure user defaults...

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

  # Enable subpixel rendering
  defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO

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
}

disable-spotlight() {
  sudo mdutil -a -i off
}

install-brew
install-brew-formulas
install-brew-casks
install-terminfos
install-launch-daemons
install-configs
configure-user-defaults
disable-spotlight
