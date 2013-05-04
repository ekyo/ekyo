# Install NodeJs
cd ~
sudo apt-get install npm -y
wget -N http://nodejs.org/dist/v0.10.5/node-v0.10.5.tar.gz
tar xzvf node-v0.10.5.tar.gz && cd node-v0.10.5
./configure
sudo make install
npm install -g coffeelint
n
cd ..
