#!/usr/bin/env bash

for browser in brave-browser-stable microsoft-edge-stable google-chrome-stable
do
  if type -P "${browser}" > /dev/null
  then
    export BROWSER="${browser}"
    export CHROME_PATH="$(type -P $browser)"
    break
  fi
done

export INFINALITY_FT='ultimate5'

export QT_QPA_PLATFORMTHEME='qt5ct'
export QT_STYLE_OVERRIDE='adwaita-dark'

dbus_socket="${XDG_RUNTIME_DIR}/bus"
xresources="${HOME}/.Xresources"
xkb_keymap="${HOME}/.xkb/keymap/my_keymap"

if test -S "${dbus_socket}"
then
  export DBUS_SESSION_BUS_ADDRESS="unix:path=${dbus_socket}"
fi

if test -f "${xresources}"
then
  xrdb -merge "${xresources}"
fi

if test -f "${xkb_keymap}"
then
  xkbcomp -w 0 -I"${HOME}/.xkb" "${xkb_keymap}" ${DISPLAY} &> /dev/null
fi

xcape -e '#133=Escape;#134=Zenkaku_Hankaku'

export GTK_IM_MODULE='fcitx5'
export QT_IM_MODULE='fcitx5'
export XMODIFIERS='@im=fcitx5'

fcitx5 -d

picom &

dunst &

${HOME}/.local/bin/geektray &

# Remap ALT_L -> Super_L, ALT_R -> Super_R
x0vncserver \
  -PasswordFile "${HOME}/.config/tigervnc/passwd" \
  -RemapKeys '0xffe9->0xffeb,0xffea->0xffec' &

hsetroot -solid '#3f576e'

if test "${1##*/}" = 'Xsession'
then
  session="${HOME}/.local/bin/xmonad"
else
  session="${1}"
fi

exec fish --login --command 'exec $argv' "${session}"
