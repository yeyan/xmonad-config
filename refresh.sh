#!/bin/bash

cp ~/.Xresources .
cp -r ~/.xmonad .

find .xmonad -not \( -iname '*hs' -o -iname '*sh' \) -type f -exec rm '{}' \;
