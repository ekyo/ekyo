sudo apt-get update -y && sudo apt-get upgrade -y

sudo apt-get install -y haskell-platform build-essential xorg-dev libudev-dev libts-dev libgl1-mesa-dev libglu1-mesa-dev libasound2-dev libpulse-dev libopenal-dev libogg-dev libvorbis-dev libaudiofile-dev libpng12-dev libfreetype6-dev libusb-dev libdbus-1-dev zlib1g-dev libdirectfb-dev libcairo2-dev libpango1.0-dev

cabal update
cabal install gtk2hs-buildtools

echo "PATH=$HOME/.cabal/bin:$PATH" >> ~/.bashrc
source ~/.bashrc

wget https://www.libsdl.org/release/SDL2-2.0.3.tar.gz
tar -zxvf SDL2-2.0.3.tar.gz
rm SDL2-2.0.3.tar.gz
cd SDL2-2.0.3/
./configure 

sudo apt-get install checkinstall
sudo checkinstall --nodoc -y

sudo ldconfig

cabal install helm
