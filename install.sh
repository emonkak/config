#!/usr/bin/env bash

set -o errexit -o nounset

if [[ "${TRACE-0}" == "1" ]]
then
  set -o xtrace
fi

mklink() {
  ln -Tfvs $(realpath "${1}") "${2}"
}

mkdir -p ~/.config ~/.local/share

mklink "vim" "${HOME}/.config/nvim"

mklink "skk" "${HOME}/.local/share/skk"

for path in alacritty fontconfig git ncmpcpp polybar tig tmux vifm vim xmonad
do
  mklink "${path}" "${HOME}/.config/${path}"
done

for path in dotfiles/.[^.]*
do
  mklink "${path}" "${HOME}/${path##*/}"
done
