function dull_red()     { echo "$(tput setaf 1)$1$(tput sgr0)" }
function dull_green()   { echo "$(tput setaf 2)$1$(tput sgr0)" }
function dull_yellow()  { echo "$(tput setaf 3)$1$(tput sgr0)" }
function dull_blue()    { echo "$(tput setaf 4)$1$(tput sgr0)" }
function dull_magenta() { echo "$(tput setaf 5)$1$(tput sgr0)" }
function dull_cyan()    { echo "$(tput setaf 6)$1$(tput sgr0)" }
function dull_white()   { echo "$(tput setaf 7)$1$(tput sgr0)" }
function dull_black()   { echo "$(tput setaf 8)$1$(tput sgr0)" }
function hot_red()      { echo "$(tput setaf 9)$1$(tput sgr0)" }
function hot_green()    { echo "$(tput setaf 10)$1$(tput sgr0)" }
function hot_yellow()   { echo "$(tput setaf 11)$1$(tput sgr0)" }
function hot_blue()     { echo "$(tput setaf 12)$1$(tput sgr0)" }
function hot_magenta()  { echo "$(tput setaf 13)$1$(tput sgr0)" }
function hot_cyan()     { echo "$(tput setaf 14)$1$(tput sgr0)" }
function hot_white()    { echo "$(tput setaf 15)$1$(tput sgr0)" }
function hot_black()    { echo "$(tput setaf 16)$1$(tput sgr0)" }

# for debugging
function show-colors() {
    echo "dull_red:     \t $(dull_red     'jump in the urinal')"
    echo "dull_green:   \t $(dull_green   'and stand on your head')"
    echo "dull_yellow:  \t $(dull_yellow  'im the one thats alive')"
    echo "dull_blue:    \t $(dull_blue    'youre all dead')"
    echo "dull_magenta: \t $(dull_magenta 'lean over the bowl')"
    echo "dull_cyan:    \t $(dull_cyan    'and then take a dive')"
    echo "dull_white:   \t $(dull_white   'all of you are dead')"
    echo "dull_black:   \t $(dull_black   'i am alive')\n"
    echo "hot_red:      \t $(hot_red      'jump in the urinal')"
    echo "hot_green:    \t $(hot_green    'and stand on your head')"
    echo "hot_yellow:   \t $(hot_yellow   'im the one thats alive')"
    echo "hot_blue:     \t $(hot_blue     'youre all dead')"
    echo "hot_magenta:  \t $(hot_magenta  'lean over the bowl')"
    echo "hot_cyan:     \t $(hot_cyan     'and then take a dive')"
    echo "hot_white:    \t $(hot_white    'all of you are dead')"
    echo "hot_black:    \t $(hot_black    'i am alive')\n"
}

function show-bold-colors() {
    echo "red:      \t $(tput bold)$(hot_red      'jump in the urinal')"
    echo "green:    \t $(tput bold)$(hot_green    'and stand on your head')"
    echo "yellow:   \t $(tput bold)$(hot_yellow   'im the one thats alive')"
    echo "blue:     \t $(tput bold)$(hot_blue     'youre all dead')"
    echo "magenta:  \t $(tput bold)$(hot_magenta  'lean over the bowl')"
    echo "cyan:     \t $(tput bold)$(hot_cyan     'and then take a dive')"
    echo "white:    \t $(tput bold)$(hot_white    'all of you are dead')"
    echo "black:    \t $(tput bold)$(hot_black    'i am alive')\n"
}
