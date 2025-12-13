#!/bin/sh

export LANG='en_US.UTF-8'
export LC_MESSAGES='C'
export LC_TIME='C'

export BROWSER='microsoft-edge-stable'
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=adwaita-dark

export INFINALITY_FT="ultimate5"

export XDG_CACHE_HOME="/tmp/.${USER}/cache"
[ ! -d "${XDG_CACHE_HOME}" ] && install -dm0700 "${XDG_CACHE_HOME}"

eval "$(ssh-agent)"

eval "$(dbus-launch --sh-syntax --exit-with-session)"

[ -f "${HOME}/.Xresources" ] && xrdb -merge "${HOME}/.Xresources"

[ -f "${HOME}/.xkb/keymap/my_keymap" ] && xkbcomp -w 0 -I"${HOME}/.xkb" "${HOME}/.xkb/keymap/my_keymap" $DISPLAY &> /dev/null

xcape -e '#133=Escape;#134=Zenkaku_Hankaku'

xinput --set-ptr-feedback "ThinkPS/2 Kensington ThinkingMouse" 4 2 1

export GTK_IM_MODULE='fcitx5'
export QT_IM_MODULE='fcitx5'
export XMODIFIERS='@im=fcitx5'

fcitx5 -d

pulseaudio -D

picom &

geektray &

dunst &

hsetroot -solid '#3f576e'

if [ ! -d "${HOME}/.local/state/tigervnc" ]
then
  mkdir "${HOME}/.local/state/tigervnc"
fi

# Remap ALT_L -> Super_L, ALT_R -> Super_R
x0vncserver \
  -PasswordFile ~/.config/tigervnc/passwd \
  -RemapKeys '0xffe9->0xffeb,0xffea->0xffec' \
  >> "${HOME}/.local/state/tigervnc/server.log" &

session="${1##*/}"

case "${session}" in
  Xsession|'')
    exec "${HOME}/.local/bin/xmonad"
    ;;
  *)
    exec "${session}"
    ;;
esac
