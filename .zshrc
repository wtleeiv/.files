# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep extendedglob nomatch
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/ty/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

alias ls="ls --color"
alias grep="grep --color"
alias less="less -R"

alias clip="xclip -sel clip"
alias x="chmod +x"
alias aoeu="setxkbmap us"
alias asdf="setxkbmap dvorak"
