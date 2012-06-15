# Configure Git
read -p "Name for git?" gituser
read -p "Email for git?" email
git config --global user.name $gituser
git config --global user.email $email
git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=3600'

# Update System and Install configuration-less Programs
apt-get -y dist-upgrade
apt-get -y update
apt-get -y upgrade
apt-get install -y \
    ubuntu-desktop \
    gnome-shell gnome-tweak-tool \
    xfce4-appfinder xfce4-panel thunar xfce4-session xfce4-settings xfdesktop xfwm4 \
    pidgin chromium-browser

# Install MongoDB
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
echo deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen \
    > /etc/apt/sources.list.d/10gen.list
apt-get -y update
apt-get -y upgrade
apt-get install -y mongodb-10gen 

# Install Redis
cd ~
wget http://redis.googlecode.com/files/redis-2.4.14.tar.gz
tar xzf redis-2.4.14.tar.gz
cd redis-2.4.14
make
[ ! -f "/usr/local/bin/redis-server" ] && redis-server /usr/local/bin/
[ ! -f "/usr/local/bin/redis-cli" ] && cp redis-cli /usr/local/bin/
[ ! -d "/etc/redis" ] && mkdir /etc/redis
[ ! -d "/var/redis" ] && mkdir /var/redis
[ ! -d "/var/redis/6379" ] && mkdir /var/redis/6379
[ ! -f "/etc/init.d/redis_6379" ] && \
    cat utils/redis_init_script | \
    sed 's,REDIS_PORT,6379,g' \
    > /etc/init.d/redis_6379
[ ! -f "/etc/redis/6379.conf" ] && \
    cat utils/redis.conf.tpl | \
    sed 's,daemonize no,daemonize yes,g' \
    sed 's,$PIDFILE,/var/run/redis_6379.pid,g' \
    sed 's,$REDIS_PORT,6379,g' \
    sed 's,loglevel verbose,loglevel debug,g' \
    sed 's,$REDIS_LOG_FILE,/var/log/redis_6379.log,g' \
    sed 's,$REDIS_DATA_DIR,/var/redis/6379,g' \
    > /etc/redis/6379.conf
update-rc.d redis_6379 defaults

# Install DMD
echo "http://d-apt.googlecode.com/files /" >> /etc/apt/sources.list
apt-get update && apt-get -y --allow-unauthenticated install d-apt-keyring && apt-get update
apt-get install -y \
    dmd \
    libgtkd-dev libgtkd-doc \
    libplot2kill-dev libplot2kill-doc \
    libtango-dev libtango-doc \
    libdcollections-dev libdcollections-doc \
    liborange-dmd-dev liborange-dmd-doc \
    libspiritd-dev libspiritd-doc \
    libdstats-dev libdstats-doc \
    libmsgpack-dmd-dev libmsgpack-dmd-doc \
    libderelict-dev \
    libgl3n-dev libgl3n-doc \
    vibe vibe-doc

# Install Zsh with Oh My Zsh
cd ~
apt-get install -y zsh
[ ! -d .oh-my-zsh ] && git clone https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh
if [ -e .zshrc ] then
    cp .zshrc .zshrc.back
    rm .zshrc
fi
cp .oh-my-zsh/templates/zshrc.zsh-template .zshrc
echo "export PATH=$PATH" >> .zshrc
chsh -s 'which zsh'

# Install emacs
cd ~
apt-get install -y emacs
[ ! -d .dude-dot-files ] && \
    git clone --recursive git://github.com/ddude/dude-dot-files.git .dude-dot-files
[ ! -f .emacs ] && \
    ln -s .dude-dot-files/.emacs .emacs
[ ! -d .site-lisp ] && \
    ln -s .dude-dot-files/.site-lisp .site-lisp
[ ! -f .bashrc ] && \
    ln -s .dude-dot-files/.bashrc .bashrc
[ ! -f .bash_profile ] && \
    ln -s .dude-dot-files/.bash_profile .bash_profile