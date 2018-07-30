#!/usr/bin/env bash

function symlink() {
    target_path=$1
    link_path=$2

    ln -s $target_path $link_path
    chown -R vagrant:vagrant $link_path
    echo "$link_path -> $target_path" 
}

function maybe_show_existing_config() {
	[[ -e "$1" ]] && echo "$1 already exists"
}

function configs_already_present() {
        [[ -e /home/vagrant/.emacs.d ]] ||

                [[ -e /home/vagrant/.gemrc ]] ||
                [[ -e /home/vagrant/.gitconfig ]] ||
                [[ -e /home/vagrant/.rspec ]] ||
                [[ -e /home/vagrant/.tmux.conf ]] ||
                [[ -e /home/vagrant/.vimrc ]] ||

                [[ -e /home/vagrant/.zsh ]] ||
                [[ -e /home/vagrant/.zshrc ]] ||
                [[ -e /home/vagrant/.zshenv ]]
}

function show_existing_configs() {
	maybe_show_existing_config /home/vagrant/.emacs.d

	maybe_show_existing_config /home/vagrant/.gemrc
	maybe_show_existing_config /home/vagrant/.gitconfig
	maybe_show_existing_config /home/vagrant/.rspec
	maybe_show_existing_config /home/vagrant/.tmux.conf
	maybe_show_existing_config /home/vagrant/.vimrc

	maybe_show_existing_config /home/vagrant/.zsh
	maybe_show_existing_config /home/vagrant/.zshrc
	maybe_show_existing_config /home/vagrant/.zshenv
}

function main() {
	if configs_already_present; then
		show_existing_configs
	else
		symlink /home/vagrant/env/emacs /home/vagrant/.emacs.d

		symlink /home/vagrant/env/dotfiles/gemrc /home/vagrant/.gemrc
		symlink /home/vagrant/env/dotfiles/gitconfig /home/vagrant/.gitconfig
		symlink /home/vagrant/env/dotfiles/rspec /home/vagrant/.rspec
		symlink /home/vagrant/env/dotfiles/tmux.conf /home/vagrant/.tmux.conf
		symlink /home/vagrant/env/dotfiles/vimrc /home/vagrant/.vimrc

		symlink /home/vagrant/env/zsh /home/vagrant/.zsh
		symlink /home/vagrant/.zsh/zshrc /home/vagrant/.zshrc
		symlink /home/vagrant/.zsh/zshenv /home/vagrant/.zshenv

		# doesn't work :(
		# chsh -s $(which zsh) vagrant
		echo "\nstill need to run 'chsh -s $(which zsh) vagrant'"
	fi
}

main
