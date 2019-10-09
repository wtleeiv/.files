#!/bin/sh

layout=`cat $HOME/.keyboard_layout`

case $layout in
    dvorak)
	bash $HOME/bin/qwerty.sh
	;;
    qwerty)
	bash $HOME/bin/dvorak.sh
	;;
esac
