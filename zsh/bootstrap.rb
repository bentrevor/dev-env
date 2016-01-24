#!/usr/bin/env ruby

require 'fileutils'
require 'open-uri'

FileUtils.cd File.dirname(__FILE__)

def install_syntax_highlighting
  url = "git://github.com/zsh-users/zsh-syntax-highlighting.git"
  dir = url.split('/').last.sub(/\.git$/, '')

  if File.exists?(dir)
    puts "  Pulling from #{url} into #{dir}"
    `cd #{dir} && git reset --hard && git pull && cd ..`
  else
    puts "  Unpacking #{url} into #{dir}"
    `git clone #{url} #{dir}`
  end
end

def link_zshrc_and_zshenv
  `ln -s ~/.zsh/zshrc ~/.zshrc`
  `ln -s ~/.zsh/zshenv ~/.zshenv`
end

link_zshrc_and_zshenv
install_syntax_highlighting git_bundles
