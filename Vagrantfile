# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.require_version ">= 1.7.4"

Vagrant.configure("2") do |config|

  config.vm.box = 'ubuntu/trusty64'
  config.vm.box_check_update = false
  config.vm.network :private_network, ip: '192.168.56.56'
  config.ssh.forward_agent = true

  provision_environment(config, 'eevee', {
                          host_port: 4040,
                          memory: 4096,
                          cpus: 1,
                        })

  apt_packages = %w(
git
emacs24-nox
tree
acpi
silversearcher-ag
zsh
).join(' ')

  config.vm.provision "~~ install (#{apt_packages})  ~~", type: 'shell', inline: <<-SHELL
apt-get update
apt-get install #{apt_packages} -y
SHELL

  config.vm.provision '~~ get env ~~', type: 'shell', inline: <<-SHELL
touch ~/.ssh/config
echo "Host github.com\n  StrictHostKeyChecking no" >> ~/.ssh/config

git clone -b vagrant git@github.com:bentrevor/env.git /home/vagrant/env
chown -R vagrant:vagrant /home/vagrant/env
source /home/vagrant/env/configure.sh
SHELL
end

def provision_environment(config, env_name, opts)
  host_port = opts[:host_port]
  memory = opts[:memory]
  cpus = opts[:cpus]

  config.vm.define env_name, autostart: true do |e|
    e.vm.network 'forwarded_port', host: host_port, guest: 8080, autocorrect: true
    e.vm.provider :virtualbox do |vb|
      vb.memory = memory
      vb.cpus = cpus
    end
  end
end
