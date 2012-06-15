# Install DMD
echo "deb http://d-apt.googlecode.com/files /" >> /etc/apt/sources.list
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