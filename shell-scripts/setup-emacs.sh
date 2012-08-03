# Install emacs
sudo apt-get install -y \
    texinfo texlive \
    libxpm-dev libjpeg-dev libgif-dev libtiff4-dev libgtk2.0-dev \
    libncurses-dev libncurses5-dev \
    aspell ack wmctrl
wget http://ftp.gnu.org/pub/gnu/emacs/emacs-24.1.tar.gz
tar -xzf emacs-24.1.tar.gz
cd emacs-24.1
./configure
make
sudo make install
gsettings set com.canonical.Unity.Launcher favorites "$(gsettings get com.canonical.Unity.Launcher favorites | sed "s/, *'emacs' *//g" | sed "s/'emacs' *, *//g" | sed -e "s/]$/, 'emacs']/")"
cd ..
rm -r emacs-24.1
rm emacs-24.1.tar.gz
# Prelude Extension
curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

echo "Emacs Installation Will Complete Upon Executing Emacs"
echo "The Following Packages Must Still Be Installed Manually:"
echo "  - thesaurus"
echo "  - auto-complete"
