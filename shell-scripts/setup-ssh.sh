cd ~
mkdir .ssh
sudo apt-get install pssh

cd .ssh
touch authorized_keys2
chmod 600 authorized_keys2

curl https://github.com/ekyo.keys >> authorized_keys2
