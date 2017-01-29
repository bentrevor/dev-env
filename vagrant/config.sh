#!/usr/bin/env sh

git clone -b vagrant git@github.com:bentrevor/env.git /home/vagrant/env
chown -R vagrant:vagrant /home/vagrant/env
sh /home/vagrant/env/configure.sh

sudo apt-get install -y automake pkg-config libpcre3-dev zlib1g-dev liblzma-dev
git clone git@github.com:ggreer/the_silver_searcher.git
sh /home/vagrant/the_silver_searcher/build.sh
sudo make install -C /home/vagrant/the_silver_searcher
