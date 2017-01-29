def chown(target)
  `chown -R vagrant:vagrant #{target}`
end

def cp(dir, target)
  `cp -r /home/vagrant/env/#{dir} /home/vagrant/#{target}`
  chown(target)
end

def symlink(target, link)
  `ln -s /home/vagrant/env/#{target} /home/vagrant/#{link}`
  chown(link)
end


cp 'emacs', '.emacs.d'
cp 'zsh', '.zsh'
symlink 'zsh/zshrc', '.zshrc'
symlink 'dotfiles/vimrc', '.vimrc'
symlink 'dotfiles/tmux.conf', '.tmux.conf'
symlink 'dotfiles/gitconfig', '.gitconfig'

`chsh -s $(which zsh) vagrant`

