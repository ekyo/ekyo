# Install emacs
sudo apt-get install -y \
    texinfo texlive \
    libxpm-dev libjpeg-dev libgif-dev libtiff4-dev libgtk2.0-dev \
    libncurses-dev libncurses5-dev \
    aspell ack wmctrl fonts-inconsolata

sudo add-apt-repository ppa:cassou/emacs
sudo apt-get update
sudo apt-get install -y emacs24 emacs24-el emacs24-common-non-dfsg emacs-goodies-el

# Prelude Extension
curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

echo "Emacs Installation Will Complete Upon Executing Emacs"
echo "Run it once for prelude, then copy that awesome config"
