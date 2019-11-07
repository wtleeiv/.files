#!/bin/sh

setxkbmap us
killall xcape
echo "qwerty" > $HOME/.keyboard_layout
