#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
if [ -f $sysresources ]; then
    xrdb -merge $userresources
fi
if [ -f $usermodmap ]; then
    xmodmap $usermodmap
fi

export XMODIFIERS='@im=uim'
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &


exec xmonad
