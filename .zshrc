# Lines configured by zsh-newuser-install
setopt autocd
unsetopt beep extendedglob notify
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/wtleeiv/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export PS1="%)o( "
export EDITOR="vim"
export VISUAL="vim"
export TERM="rxvt-unicode"
export PATH="/home/wtleeiv/.miniconda3/bin:$PATH"

alias ls="ls --color"
alias grep="grep --color"

alias http="python -m http.server"
