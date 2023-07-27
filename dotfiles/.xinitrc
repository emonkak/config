#!/bin/sh

export BROWSER='microsoft-edge-stable'
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=adwaita-dark

export INFINALITY_FT="ultimate5"

export XDG_CACHE_HOME="/tmp/.${USER}/cache"
[ ! -d "${XDG_CACHE_HOME}" ] && install -dm0700 "${XDG_CACHE_HOME}"

eval `ssh-agent`

[ -f "${HOME}/.Xresources" ] && xrdb -merge "${HOME}/.Xresources"

[ -f "${HOME}/.xkb/keymap/my_keymap" ] && xkbcomp -w 0 -I"${HOME}/.xkb" "${HOME}/.xkb/keymap/my_keymap" $DISPLAY &> /dev/null

xcape -e '#133=Escape;#134=Zenkaku_Hankaku'

xinput --set-ptr-feedback "ThinkPS/2 Kensington ThinkingMouse" 4 2 1

export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
export XMODIFIERS='@im=uim'
uim-xim &

picom &

geektray &

dunst &

hsetroot -solid '#3f576e'

session="${1##*/}"

case "${session}" in
  Xsession|'')
    exec dbus-launch --exit-with-session "${HOME}/.cabal/bin/xmonad"
    ;;
  *)
    exec dbus-launch --exit-with-session "${session}"
    ;;
esac
