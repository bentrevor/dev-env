#!/usr/bin/env bash

echo 'need to set a username'
return 1

#username=vagrant

function symlink() {
    target_path=$1
    link_path=$2

    ln -s $target_path $link_path
    chown -R $username:$username $link_path
}

symlink /home/$username/env/emacs /home/$username/.emacs.d

symlink /home/$username/env/zsh /home/$username/.zsh
symlink /home/$username/.zsh/zshrc /home/$username/.zshrc
symlink /home/$username/.zsh/zshenv /home/$username/.zshenv

symlink /home/$username/env/dotfiles/vimrc /home/$username/.vimrc
symlink /home/$username/env/dotfiles/tmux.conf /home/$username/.tmux.conf
symlink /home/$username/env/dotfiles/gitconfig /home/$username/.gitconfig

# doesn't work :(
# chsh -s $(which zsh) $username
echo "still need to run 'chsh -s $(which zsh) $username'"
