Configure your ubuntu-server 12.04 LTS x64
------------
1. Download the install/configuration script `wget https://raw.github.com/ekyo/ekyo/master/install-ubuntu.sh`
2. Run the script`sh install-ubuntu.sh`
  It will ask your root password and then will try configuring git for you, it will ask you
    - your name, enter it like this: `"FirstName LastName"`
    - your email: `"youremail@anydomain.com"`
3. Restart the system now `sudo shutdown -r now`

After restart, log in using your account and install remaining drivers (or VirtualBox additions if your ubuntu is a guest)
