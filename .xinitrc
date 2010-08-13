#!/bin/sh

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"

xset r rate 250 30

export XMODIFIERS='@im=uim'
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &

FG='#cccccc'
BG='#000000'
FONT='-artwiz-snap-normal-r-normal--10-*-*-*-*-*-*-*'
conky | dzen2 -x 680 -y 0 -w 600 -h 18 -ta r -fg "$FG" -bg "$BG" -fn "$FONT" &

dbus-launch --sh-syntax --exit-with-session xmonad
