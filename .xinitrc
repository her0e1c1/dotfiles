#!/usr/local/bin/bash

setxkbmap -rules xorg -model jp106 -layout jp

#input methods
export LANG=ja_JP.UTF-8
#export LC_CTYPE=ja_JP.UTF-8
export XMODIFIERS='@im=SCIM'
#export GDM_LANG=ja_JP.UTF-8
#export 
#export GTK_IM_MODULE=scim
#echo '*inputMethod : SCIM' | xrdb -merge
scim -d &
#xmodmap ~/.xmodmap
#xrdb .Xresources
#gui
fluxbox

