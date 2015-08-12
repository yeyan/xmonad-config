#!/bin/bash

cp ~/.Xresources .
cp -r ~/.xmonad .
cp ~/.xmobarrc .

find .xmonad -not \( -iname '*hs' -o -iname '*sh' \) -type f -exec rm '{}' \;
