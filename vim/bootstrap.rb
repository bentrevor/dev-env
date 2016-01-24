#!/usr/bin/env ruby

git_bundles = [
#   "git://github.com/guns/vim-clojure-static.git",
#   "git://github.com/jnwhiteh/vim-golang.git",
#   "git://github.com/vim-ruby/vim-ruby.git",
#   "git://github.com/kchmck/vim-coffee-script.git",
#   "git://github.com/bentrevor/rainbow_parentheses.vim.git",
#   "git://github.com/vim-scripts/paredit.vim.git",
#   'git@github.com:wincent/Command-T.git',

#   "git://github.com/tomtom/tlib_vim.git",
#   "git://github.com/MarcWeber/vim-addon-mw-utils.git",
#   "git://github.com/garbas/vim-snipmate.git",
#   "git://github.com/honza/vim-snippets.git",

  "git://github.com/bling/vim-airline.git",
#   "git://github.com/pangloss/vim-javascript",
  "git://github.com/scrooloose/nerdtree.git",
#   "git://github.com/tpope/vim-fireplace.git",
  "git://github.com/tpope/vim-pathogen.git"
]

require 'fileutils'
require 'open-uri'

bundles_dir = File.join(File.dirname(__FILE__), "bundle")

FileUtils.mkdir_p(bundles_dir)
FileUtils.cd(bundles_dir)

def install_plugins(bundles)
  git_bundles = bundles
  git_bundles.each do |url|
    dir = url.split('/').last.sub(/\.git$/, '')
    if File.exists?(dir)
      if !ENV["SKIP_UPDATING_VIM_BUNDLES"]
        puts "  Pulling from #{url} into #{dir}"
        `cd #{dir} && git reset --hard && git pull && cd ..`
      end
    else
      puts "  Unpacking #{url} into #{dir}"
      `git clone #{url} #{dir}`
    end
  end
end

def link_vimrc
  `ln -s ~/.vim/.vimrc ~/.vimrc`
end

def copy_syncolor
  `cp ~/.vim/syncolor.vim /usr/share/vim/vim73/syntax/syncolor.vim`
end

link_vimrc
copy_syncolor
install_plugins git_bundles
