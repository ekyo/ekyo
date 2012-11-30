# Install NodeJs
cd ~
sudo make install npm
wget -N http://nodejs.org/dist/v0.8.9/node-latest.tar.gz
tar xzvf node-latest.tar.gz && cd `ls -rd node-v*`
./configure
sudo make install
cd ..
