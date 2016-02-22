This was originally forked from https://github.com/mbadolato/iTerm2-Color-Schemes

#iTerm Color Schemes#

- [Intro](#intro)
- [Installation Instructions](#installation-instructions)
- [Credits](#credits)
- [Mac OS Terminal color schemes](#terminal-color-schemes)
- [Previewing color schemes](#previewing-color-schemes)

##Intro##
This is a set of color schemes for iTerm (aka iTerm2). Screenshots in the [screenshots](screenshots/) directory and in `screenshots.markdown`.

##Installation Instructions##
To install:

* Launch iTerm 2. Get the latest version at <a href="http://www.iterm2.com">iterm2.com</a>
* Type CMD+i (⌘+i)
* Navigate to Colors tab
* Click on Load Presets
* Click on Import
* Select the .itermcolors file(s) of the scheme(s) you'd like to use
* Click on Load Presets and choose a color scheme

## Credits
See `credits.txt`.

##Terminal color schemes##
Just double click on selected theme in `terminal` directory

##Previewing color schemes##

[preview.rb](tools/preview.rb) is a simple script that allows you to preview
the color schemes without having to import them. It parses .itermcolors files
and applies the colors to the current session using [iTerm's proprietary
escape codes](https://iterm2.com/documentation-escape-codes.html). As noted in
the linked page, it doesn't run on tmux or screen.

```sh
# Apply AdventureTime scheme to the current session
tools/preview.rb schemes/AdventureTime.itermcolors

# Apply the schemes in turn.
# - Press any key to advance; hit CTRL-C or ESC to stop
tools/preview.rb schemes/*
```


