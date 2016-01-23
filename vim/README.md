# vimfiles

This is my vim config. There are many like it but this one is <del>mine</del> [Colin's](https://github.com/trptcolin/vimfiles), only better.

## Getting going

The idea is:
- clone this to your home directory, renaming it `.vim`
- run `./update_bundles`

## Gotchas

`update_bundles` is a Ruby script, so make sure you have Ruby installed.

Be sure to install any needed extensions, and that your Vim is compiled with
the right flags. Example: Command-T requires `+ruby`, and requires compiling
native extensions with the system ruby (`rvm use system`)- see their README for
more.


