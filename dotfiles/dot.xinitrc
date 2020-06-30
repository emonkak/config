#!/bin/sh

userresources="${HOME}/.Xresources"
usermodmap="${HOME}/.Xmodmap"

[ -f "${userresources}" ] && xrdb -merge "${userresources}"
[ -f "${usermodmap}" ] && xmodmap "${usermodmap}"

xsetroot -solid '#363f45'

xinput --set-ptr-feedback "ThinkPS/2 Kensington ThinkingMouse" 4 2 1

xcape -e '#133=Escape;#134=Zenkaku_Hankaku'

eval `ssh-agent`

export BROWSER='google-chrome-stable'

export XDG_CACHE_HOME="/tmp/.${USER}/cache"
[ ! -d "${XDG_CACHE_HOME}" ] && install -dm0700 "${XDG_CACHE_HOME}"

export XMODIFIERS='@im=uim'
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &

dunst &

case ${1##*/} in
  fluxbox)
    exec dbus-launch --exit-with-session startfluxbox
    ;;
  *)
    exec dbus-launch --exit-with-session xmonad
    ;;
esac
