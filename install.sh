#!/usr/bin/env bash

set -o errexit -o nounset

if [[ "${TRACE-0}" == "1" ]]
then
  set -o xtrace
fi

mklink() {
  mkdir --parents "${2%/*}"
  ln --force --symbolic --no-target-directory --verbose $(realpath "${1}") "${2}"
}

mkdir --parents ~/.config ~/.local/share

mklink "vim" "${HOME}/.config/nvim"

mklink "skk" "${HOME}/.local/share/skk"

for path in alacritty cabal fontconfig git libskk mpv ncmpcpp picom polybar tig tmux vifm vim xmonad
do
  mklink "${path}" "${HOME}/.config/${path}"
done

for path in dotfiles/.[^.]*
do
  mklink "${path}" "${HOME}/${path#*/}"
done
