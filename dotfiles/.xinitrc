#!/bin/sh

for browser in brave-browser-stable microsoft-edge-stable google-chrome-stable
do
  if type -P ${browser} > /dev/null
  then
    export BROWSER=${browser}
    export CHROME_PATH="$(type -P $browser)"
    break
  fi
done

export INFINALITY_FT="ultimate5"

export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=adwaita-dark

eval "$(ssh-agent)"

eval "$(dbus-launch --sh-syntax --exit-with-session)"

XRESOURCES="${HOME}/.Xresources"

if test -f "${XRESOURCES}"
then
  xrdb -merge "${XRESOURCES}"
fi

XKB_KEYMAP="${HOME}/.xkb/keymap/my_keymap"

if test -f "${XKB_KEYMAP}"
then
  xkbcomp -w 0 -I"${HOME}/.xkb" "${XKB_KEYMAP}" ${DISPLAY} > /dev/null 2>&1
fi

xcape -e '#133=Escape;#134=Zenkaku_Hankaku'

/usr/bin/pulseaudio -D

export GTK_IM_MODULE='fcitx5'
export QT_IM_MODULE='fcitx5'
export XMODIFIERS='@im=fcitx5'

/usr/bin/fcitx5 -d

/usr/bin/picom &

${HOME}/.local/bin/geektray &

hsetroot -solid '#3f576e'

# Remap ALT_L -> Super_L, ALT_R -> Super_R
/usr/bin/x0vncserver \
  -PasswordFile "${HOME}/.config/tigervnc/passwd" \
  -RemapKeys '0xffe9->0xffeb,0xffea->0xffec' &

if test "${1##*/}" = Xsession
then
  session="${HOME}/.local/bin/xmonad"
else
  session="${1}"
fi

exec fish --login --command 'exec $argv' "${session}"
