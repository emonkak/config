#!/bin/sh

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
[ -f $userresources ] && xrdb -merge "$userresources"
[ -f $usermodmap ] && xmodmap "$usermodmap"

export XMODIFIERS='@im=uim'
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &

exec xmonad
