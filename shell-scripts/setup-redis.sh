# Install Redis
cd ~
wget http://redis.googlecode.com/files/redis-2.6.7.tar.gz
tar xzf redis-2.6.7.tar.gz
cd redis-2.6.7
make
sudo make install
[ ! -f "/usr/local/bin/redis-server" ] && sudo cp redis-server /usr/local/bin/
[ ! -f "/usr/local/bin/redis-cli" ] && sudo cp redis-cli /usr/local/bin/
[ ! -d "/etc/redis" ] && sudo mkdir /etc/redis
[ ! -d "/var/redis" ] && sudo mkdir /var/redis
[ ! -d "/var/redis/6379" ] && sudo mkdir /var/redis/6379
[ ! -f "/etc/init.d/redis_6379" ] && \
    sudo sh -c "cat utils/redis_init_script | \
    sed 's,REDIS_PORT,6379,g' \
    > /etc/init.d/redis_6379"
[ ! -f "/etc/redis/6379.conf" ] && \
    sudo sh -c "cat utils/redis.conf.tpl | \
    sed -e 's,daemonize no,daemonize yes,g' \
        -e 's,$PIDFILE,/var/run/redis_6379.pid,g' \
        -e 's,$REDIS_PORT,6379,g' \
        -e 's,loglevel verbose,loglevel debug,g' \
        -e 's,$REDIS_LOG_FILE,/var/log/redis_6379.log,g' \
        -e 's,$REDIS_DATA_DIR,/var/redis/6379,g' \
    > /etc/redis/6379.conf"
sudo update-rc.d redis_6379 defaults
cd ..
