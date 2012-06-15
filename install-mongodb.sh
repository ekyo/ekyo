# Install MongoDB
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
echo deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen \
    > /etc/apt/sources.list.d/10gen.list
apt-get -y update
apt-get -y upgrade
apt-get install -y mongodb-10gen 