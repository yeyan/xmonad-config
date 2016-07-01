#/bin/bash

xrdb -merge ~/.Xresources &

exec xmonad
