#!/bin/sh

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"

xset r rate 250 30

export BROWSER='google-chrome'

export XMODIFIERS='@im=uim'
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &

FG='#dcdccc'
BG='#222222'
FONT='-artwiz-glisp-medium-r-normal--*-*-*-*-*-*-*-*'
conky | dzen2 -x 1320 -y 0 -w 600 -h 14 -ta r -fg "$FG" -bg "$BG" -fn "$FONT" -dock -e '' &

exec dbus-launch --exit-with-session xmonad
