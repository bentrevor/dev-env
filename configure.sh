#!/usr/bin/env bash

function symlink() {
    target_path=$1
    link_path=$2

    ln -s $target_path $link_path
    chown -R vagrant:vagrant $link_path
}

cp -r /home/vagrant/env/emacs /home/vagrant/.emacs.d
cp -r /home/vagrant/env/zsh /home/vagrant/.zsh
chown -R vagrant:vagrant /home/vagrant/.zsh
chown -R vagrant:vagrant /home/vagrant/.emacs.d

symlink /home/vagrant/env/zsh/zshrc /home/vagrant/.zshrc
# chsh -s $(which zsh) vagrant

symlink /home/vagrant/env/dotfiles/vimrc /home/vagrant/.vimrc
symlink /home/vagrant/env/dotfiles/tmux.conf /home/vagrant/.tmux.conf
symlink /home/vagrant/env/dotfiles/gitconfig /home/vagrant/.gitconfig

echo "still need to run 'chsh -s $(which zsh) vagrant'"
