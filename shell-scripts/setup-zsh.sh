# Install Zsh with Oh My Zsh
cd ~
sudo apt-get install -y zsh git
[ ! -d .oh-my-zsh ] && \
    git clone https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh
if [ -e .zshrc ]; then
    cp .zshrc .zshrc.back
    rm .zshrc
fi
cp .oh-my-zsh/templates/zshrc.zsh-template .zshrc
echo 'export PATH=$PATH' >> .zshrc
sudo chsh -s /usr/bin/zsh
chsh -s /usr/bin/zsh
echo 'PROMPT='"'"'
%{$fg_bold[green]%}%p%{$fg[cyan]%}%c %{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%}
%{$fg_bold[red]%}➜  %{$reset_color%}'"'"'

ZSH_THEME_GIT_PROMPT_PREFIX="git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
' > .oh-my-zsh/themes/robbyrussell.zsh-theme

echo 'alias g="git"' >> .zshrc
echo 'alias goscript="cd ~/Dropbox/ekyo/shell-scripts"' >> .zshrc
echo 'alias goekyo="cd ~/Dropbox/ekyo-private/server"' >> .zshrc
echo 'alias godrop="cd ~/Dropbox/"' >> .zshrc
echo 'alias myip="wget http://automation.whatismyip.com/n09230945.asp -O - -q ; echo"' >> .zshrc
echo 'alias ping="mtr"' >> .zshrc
echo 'alias vim="emacs"' >> .zshrc
