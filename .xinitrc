#!/bin/sh

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"

xset r rate 250 30
xcompmgr -c &

export BROWSER='google-chrome'

export XMODIFIERS='@im=uim'
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &

mpd

FG='#e2e2e2'
BG='#171717'
FONT='-mplus-fxd-medium-r-normal--10-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--10-*-*-*-*-*-*-*'
conky | dzen2 -x 960 -y 0 -w 960 -h 12 -ta r -fg "$FG" -bg "$BG" -fn "$FONT" -dock -e '' &

exec dbus-launch --exit-with-session xmonad
