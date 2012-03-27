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

dropbox &

mpd

FG='#e2e2e2'
BG='#171717'
FONT='-nil-profont-medium-r-normal--11-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--10-*-*-*-*-*-*-*'
conky | dzen2 -x 960 -y 0 -w 960 -h 14 -ta r -fg "$FG" -bg "$BG" -fn "$FONT" -dock -e '' &

exec dbus-launch --exit-with-session xmonad
