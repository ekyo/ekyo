cd ~
git clone http://git.beanwood.com/git-crypt.git
cd git-crypt
make
sudo mv git-crypt /usr/local/bin

# To generate a key
#git-crypt keygen ~/gitcrypt-key

# To use the key
#cd ~/ekyo
#git-crypt init ~/gitcrypt-key

# files to encrypt go in .gitattributes
# secretfile1 filter=git-crypt diff=git-crypt
# secretfile2 filter=git-crypt diff=git-crypt
# *.secret filter=git-crypt diff=git-crypt
