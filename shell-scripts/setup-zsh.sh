# Install Zsh with Oh My Zsh, integrates with git-flow and z

cd ~
sudo apt-get install -y zsh git git-flow
curl -L https://raw.github.com/rupa/z/master/z.sh > ~/z.sh

# Fetch oh-my-zsh if not already there, apply minor edits
if [ ! -d .oh-my-zsh ]; then
    git clone https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh
fi

# Fetch my zsh config, backup previous one
if [ -e .zshrc ]; then
    cp .zshrc .zshrc.back
    rm .zshrc
fi

curl -L https://raw.github.com/ekyo/ekyo/master/config/dot-zshrc.sh > ~/.zshrc

curl -L https://raw.github.com/ekyo/ekyo/master/config/ekyo.zsh-theme > ~/.oh-my-zsh/themes/ekyo.zsh-theme

# Sets zsh as default prompt
sudo chsh -s /usr/bin/zsh
chsh -s /usr/bin/zsh

echo '
if [[ $INSIDE_EMACS != "" ]]; then
    stty -echo
fi
' >> ~/.bashrc

# ack Settings to recognize D and Coffee languages
echo "--type-set
coffee=.coffee
--type-set
d=.d" > ~/.ackrc
