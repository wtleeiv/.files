setxkbmap -variant dvp
xmodmap ~/.keyswap
if [ -e ~/.Xresources ]; then
    xrdb -merge ~/.Xresources
fi
exec i3
