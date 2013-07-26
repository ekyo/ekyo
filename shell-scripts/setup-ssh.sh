cd ~
sudo apt-get install pssh

# install mosh
sudo apt-get install -y python-software-properties
sudo add-apt-repository ppa:keithw/mosh
sudo apt-get update
sudo apt-get install -y mosh

# setup ssh keys
mkdir .ssh
cd .ssh
touch authorized_keys2
chmod 600 authorized_keys2

curl https://github.com/ekyo.keys >> authorized_keys2
