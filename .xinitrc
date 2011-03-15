#!/bin/sh

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"

xset r rate 250 30

export BROWSER="google-chrome"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

export SAL_USE_VCLPLUGIN=gen

export XMODIFIERS="@im=uim"
export GTK_IM_MODULE="uim"
export QT_IM_MODULE="uim"
uim-xim &

FG="#dcdccc"
BG="#222222"
FONT="-artwiz-gelly-medium-r-normal--10-*-*-*-*-*-*-*"
conky | dzen2 -x 1320 -y 0 -w 600 -h 14 -ta r -fg "$FG" -bg "$BG" -fn "$FONT" -dock -e "" &

trayer --edge bottom --align right --widthtype request --heighttype pixel --SetDockType true --SetPartialStrut true --transparent true --height 18 --tint 0x000000 &

exec dbus-launch --sh-syntax --exit-with-session xmonad
