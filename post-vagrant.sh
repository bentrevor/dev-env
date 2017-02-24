#!/usr/bin/env bash

git clone git@github.com:bentrevor/env.git /home/vagrant/env
chown -R vagrant:vagrant /home/vagrant/env
source /home/vagrant/env/configure.sh

sudo apt-get install -y automake pkg-config libpcre3-dev zlib1g-dev liblzma-dev
git clone git@github.com:ggreer/the_silver_searcher.git
cd the_silver_searcher
./build.sh
sudo make install
