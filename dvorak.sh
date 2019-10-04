#!/bin/sh

setxkbmap -variant dvp
# Map an unused modifier's keysym to the spacebar's keycode and make
# it a control modifier. It needs to be an existing key so that emacs
# won't spazz out when you press it. Hyper_L is a good candidate.
space_modifier="Hyper_L"
xmodmap -e "keycode 65 = $space_modifier"
xmodmap -e "remove mod4 = $space_modifier" # hyper_l is mod4 by default
xmodmap -e "add mod3 = $space_modifier"
# Map space to an unused keycode (to keep it around for xcape to use).
xmodmap -e "keycode any = space"
# Finally use xcape to cause the space bar to generate a space when tapped.
xcape -e "$space_modifier=space"

xmodmap ~/.keyswap
