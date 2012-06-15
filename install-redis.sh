# Install Redis
cd ~
wget http://download.redis.io/redis-stable.tar.gz
tar xzf redis-stable.tar.gz
cd redis-stable
make
[ ! -f "/usr/local/bin/redis-server" ] && \
    cp redis-server /usr/local/bin/
[ ! -f "/usr/local/bin/redis-cli" ] && \
    cp redis-cli /usr/local/bin/
[ ! -d "/etc/redis" ] && \
    mkdir /etc/redis
[ ! -d "/var/redis" ] && \
    mkdir /var/redis
[ ! -d "/var/redis/6379" ] && \
    mkdir /var/redis/6379
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