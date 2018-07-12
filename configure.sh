#!/usr/bin/env bash

function symlink() {
    target_path=$1
    link_path=$2

    ln -s $target_path $link_path
    chown -R vagrant:vagrant $link_path
}

symlink /home/vagrant/env/emacs /home/vagrant/.emacs.d

symlink /home/vagrant/env/zsh /home/vagrant/.zsh
symlink /home/vagrant/.zsh/zshrc /home/vagrant/.zshrc
symlink /home/vagrant/.zsh/zshenv /home/vagrant/.zshenv

symlink /home/vagrant/env/dotfiles/vimrc /home/vagrant/.vimrc
symlink /home/vagrant/env/dotfiles/tmux.conf /home/vagrant/.tmux.conf
symlink /home/vagrant/env/dotfiles/gitconfig /home/vagrant/.gitconfig

# doesn't work :(
# chsh -s $(which zsh) vagrant
echo "still need to run 'chsh -s $(which zsh) vagrant'"
