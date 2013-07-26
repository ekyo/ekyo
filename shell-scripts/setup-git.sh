# Configure Git
sudo apt-get install -y git meld
read -p "Name for git? " gituser
read -p "Email for git? " email
git config --global user.name $gituser
git config --global user.email $email
git config --global diff.algorithm patience
