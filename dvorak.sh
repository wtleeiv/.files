#!/bin/sh

setxkbmap -variant dvp
xmodmap ~/.keyswap
xcape -e 'Hyper_L=Home'
echo "dvorak" > $HOME/.keyboard_layout
