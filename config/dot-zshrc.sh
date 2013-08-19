# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
export PATH=$PATH

# Move around faster than cd
. ~/z.sh

# Can be set to "random"
ZSH_THEME="robbyrussell"

# aliases
alias g="git"
alias gf="g flow"
alias ga="g add ."
alias gp="g push"
alias gclone="g clone --recursive"
alias gpl="g pull --recurse-submodules"

alias myip="curl -s http://checkrealip.com/ | grep \"Current IP Address\" ; ifconfig | grep \"inet addr\""
alias myip2="curl ifconfig.me"
alias ping="mtr"
alias emc="emacsclient -c"

alias ack="ack-grep"
alias ack-coffee="ack --coffee"
alias ack-d="ack --d"

lt() { ls -ltrsa "$@" | tail ; }
psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto ; }
fname() { find . -iname "*$@*" ; }

plugins=(git git-flow screen battery)

source $ZSH/oh-my-zsh.sh
