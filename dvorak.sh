#!/bin/sh

setxkbmap -variant dvp
xmodmap ~/.keyswap
echo "dvorak" > $HOME/.keyboard_layout
