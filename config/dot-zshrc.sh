# Move around faster than cd
. ~/z.sh

# oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
export PATH=$PATH:~/.cabal/bin
plugins=(git git-flow screen battery)
ZSH_THEME="../../ekyo"
source $ZSH/oh-my-zsh.sh

# copy a file from a remote host to a local one
fetchr() { scp -P $2 $1:$3 $3 }

### Git Aliases

alias g="git"
alias gf="g flow"
gcommands(){ cat ~/.zshrc | grep 'alias g' | sed -e 's/alias //g' | grep "$@" ; }


## Whitespace Checking

# Warn about whitespace errors
# Usage: gcheck
alias gcheck="gdn --check"


## Git Create

# Cloning a repository
# Usage: gclone
alias gclone="g clone --recursive"

# Initializing a folder as a git repository
# Usage: gfinit
alias gfinit="git flow init -d"

# Creating a new repository as a git repository
# Usage: gfcreate <name>
gfcreate() { mkcd "$@" && git flow init -d ; }


## Git Local Changes

# Show changed files
# Usage: gss
alias gss="g status"

# Show changes to tracked files
# Usage: gd
alias gd="g diff"
# Usage: gdn
alias gdn="g --no-pager diff"

# Stage all changes
# Usage: ga <file>
# Usage: ga .
alias ga="g add"

# Stage partial changes
# Usage: gap <file>
alias gap="ga -p"


## Git Commit

# Commit all changes
# Usage: gc
alias gc="g commit -ae"
# Usage: gcm <message>
alias gcm="g commit -am"

# Commit all changes with whitespace checking
# Usage: gcsafe
alias gcsafe="gcheck && gc"

# Commit only staged changes
# Usage: gcs
alias gcs="g commit -e"
# Usage: gcsm
alias gcsm="g commit -m"

# Change last commit
# Usage: gcamend
alias gcamend="g commit --ammend"


## Git Log/History

# Show all commits, starting with newest
# Usage: gl
# Usage: gl -p <file>
alias gl="g log"

# See who changed what/when for a file
# Usage: glb <file>
alias glb="g blame"


## Git Branches

# List all branches
# Usage: gb
alias gb="g branch"
# Usage: gba
alias gba="gb -a"

# Switch current branch
# Usage: gbc <branch>
alias gbc="g checkout"
# Usage: gbcm
alias gbcm="g checkout master"
# Usage: gbcd
alias gbcd="g checkout develop"

# Create a new branch off of another one
# Usage: gbn <new-branch> [<base-branch>]
alias gbn="gbc -b"

# Delete a local branch
# Usage: gbdel <branch>
alias gbdel="gb -d"


## Git Tags

# Create a tag for current commit
# Usage: gtag <tag>
alias gtag="g tag"

# Publish tags
# Usage: gtagp
alias gtagp="gp --tags"


## Git Remotes

# List remotes
# Usage: grl
alias grl="g remote -v"

# Show information about a remote
# Usage: grs <remote>
alias grs="g remote show"

# Add a remote
# Usage: gra <remote> <url>
alias gra="g remote add"

# Download changes from a remote without integrating
# Usage: grf <remote>
alias grf="g fetch"

# Download changes from a remote and merge
# Usage: gpl [<remote> <branch>]
alias gpl="g pull --recurse-submodules"

# Publish local changes to a remote
# Usage: gp [<remote> <branch>]
alias gp="g push"

# Delete a remote branch
# grbdel <remote> <branch>
grbdel() { gp "$1" :"$2" ; }


## Git Merge

# Merge into current branch
# Usage: gm <branch>
alias gm="g merge --no-ff"

# Resolve merge conflicts
# Usage: gmtool
alias gmtool="g mergetool"

# Merge into another branch
# Usage: gmr <from> <to>
gmr() { gbc "$2" && gm "$1" ; }


## Git Undo

# Discard all local changes
# Usage: gua
alias gua="g reset --hard HEAD"

# Discard local changes for a specific file
# Usage: gu <file>
alias gu="gbc HEAD"

# Revert a commit by producing a new commit with contrary changes
# Usage: guc <commit>
alias guc="g revert"

# Reset to a previous commit and discards all changes since
# Usage: gurdel <commit>
alias gurdel="g reset --hard"

# Reset to a previous commit and unstage all changes since
# Usage: guru <commit>
alias guru="g reset"

# Reset to a previous commit and preserve uncommited local changes
# Usage: gurk <commit>
alias gurk="g reset --keep"


## Git Flow Feature

# List branches
# Usage: gffl
alias gffl="gf feature"

# Start a feature (Where <base> is a commit on develop)
# Usage: gff <name> [<base>]
alias gff="git flow feature start"

# Finish a feature
# Usage: gfff <name>
alias gfff="git flow feature finish"

# Push a feature
# Usage: gffp <name>
alias gffp="git flow feature publish"

# Pull a feature
# Usage: gffpl <remote> <name>
alias gffpl="git flow feature pull"


## Git Flow Release

# List branches
# Usage: gfrl
alias gfrl="gf release"

# start a release (Where <base> is a commit on develop)
# Usage: gfr <release> [<name>]
alias gfr="git flow release start"

# finish a release
# Usage: gfrf <release>
alias gfrf="git flow release finish"


## Git Flow Hotfix

# List branches
# Usage: gfhl
alias gfhl="gf hotfix"

# start a hotfix (Where <base> is a commit on master)
# Usage: gfh <release> [<base>]
alias gfh="git flow hotfix start"

# Finish a hotfix
# Usage: gfhf <release>
alias gfhf="git flow hotfix finish"




### Ack Aliases
alias ack="ack-grep"
alias ack-coffee="ack --coffee"
alias ack-d="ack --d"

### Emacs Aliases
alias em="emacs"
alias emnw="em -nw"
alias emc="emacsclient -c"

#### Generic Aliases
alias myip="curl -s http://checkrealip.com/ | grep \"Current IP Address\" ; ifconfig | grep \"inet addr\""
alias myip2="curl ifconfig.me"
alias ping="mtr"
alias update="sudo apt-get update -y"
alias upgrade="sudo apt-get upgrade -uy"
alias updagrade="update ; upgrade"
alias apt-install="sudo apt-get install -y"
alias serve="python -m SimpleHttpServer"
alias serve3="python -m http.server"

### Generic Functions
lt() { ls -ltrsa "$@" | tail ; }
psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto ; }
fname() { find . -iname "*$@*" ; }
mkcd() { mkdir "$@" && wait && cd "$@" ; }
explain(){ curl -s $(echo "http://explainshell.com/explain/$1?args=${@:2}" | sed -e 's/ /+/g') |
    sed -n '/<pre/,/<\/pre>/p' | sed -s 's/<[^>]*>//g' | sed -e 's/^ *//g;s/ *$//g' | grep '.' | cat }
transfer() { tar zcf - "$2" | ssh "$1" "tar xvzf -" }
