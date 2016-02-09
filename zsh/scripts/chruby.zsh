# chruby.sh
CHRUBY_VERSION="0.3.9"
RUBIES=()

for dir in "$PREFIX/opt/rubies" "$HOME/.rubies"; do
    [[ -d "$dir" && -n "$(ls -A "$dir")" ]] && RUBIES+=("$dir"/*)
done
unset dir

function chruby_reset() {
    [[ -z "$RUBY_ROOT" ]] && return

    PATH=":$PATH:"; PATH="${PATH//:$RUBY_ROOT\/bin:/:}"
    [[ -n "$GEM_ROOT" ]] && PATH="${PATH//:$GEM_ROOT\/bin:/:}"

    if (( UID != 0 )); then
        [[ -n "$GEM_HOME" ]] && PATH="${PATH//:$GEM_HOME\/bin:/:}"

        GEM_PATH=":$GEM_PATH:"
        [[ -n "$GEM_HOME" ]] && GEM_PATH="${GEM_PATH//:$GEM_HOME:/:}"
        [[ -n "$GEM_ROOT" ]] && GEM_PATH="${GEM_PATH//:$GEM_ROOT:/:}"
        GEM_PATH="${GEM_PATH#:}"; GEM_PATH="${GEM_PATH%:}"

        unset GEM_HOME
        [[ -z "$GEM_PATH" ]] && unset GEM_PATH
    fi

    PATH="${PATH#:}"; PATH="${PATH%:}"
    unset RUBY_ROOT RUBY_ENGINE RUBY_VERSION RUBYOPT GEM_ROOT
    hash -r
}

function chruby_use() {
    if [[ ! -x "$1/bin/ruby" ]]; then
        echo "chruby: $1/bin/ruby not executable" >&2
        return 1
    fi

    [[ -n "$RUBY_ROOT" ]] && chruby_reset

    export RUBY_ROOT="$1"
    export RUBYOPT="$2"
    export PATH="$RUBY_ROOT/bin:$PATH"

    eval "$(RUBYGEMS_GEMDEPS="" "$RUBY_ROOT/bin/ruby" - <<EOF
puts "export RUBY_ENGINE=#{Object.const_defined?(:RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'};"
puts "export RUBY_VERSION=#{RUBY_VERSION};"
begin; require 'rubygems'; puts "export GEM_ROOT=#{Gem.default_dir.inspect};"; rescue LoadError; end
EOF
)"
    echo "using $(dull_red $(basename $1))\n"
    export PATH="${GEM_ROOT:+$GEM_ROOT/bin:}$PATH"

    if (( UID != 0 )); then
        export GEM_HOME="$HOME/.gem/$RUBY_ENGINE/$RUBY_VERSION"
        export GEM_PATH="$GEM_HOME${GEM_ROOT:+:$GEM_ROOT}${GEM_PATH:+:$GEM_PATH}"
        export PATH="$GEM_HOME/bin:$PATH"
    fi

    hash -r
}

function chruby()
{
    case "$1" in
        -h|--help)
            echo "usage: chruby [RUBY|VERSION|system] [RUBYOPT...]"
            ;;
        -V|--version)
            echo "chruby: $CHRUBY_VERSION"
            ;;
        on)
            CHRUBY_AUTOSWITCH=true
            ;;
        off)
            CHRUBY_AUTOSWITCH=false
            ;;
        env)
            echo ''
            echo "$(dull_red PATH:)"
            echo $PATH | tr ':' '\n'
            echo ''
            echo "$(dull_red GEM_HOME) \t=>\t$GEM_HOME"
            echo "$(dull_red GEM_ROOT) \t=>\t$GEM_ROOT"
            echo "$(dull_red GEM_PATH) \t=>\t$GEM_PATH"
            echo ''
            echo "$(dull_red RUBY_ENGINE) \t=>\t$RUBY_ENGINE"
            echo "$(dull_red RUBY_ROOT) \t=>\t$RUBY_ROOT"
            ;;
        "")
            local dir ruby
            for dir in "${RUBIES[@]}"; do
                dir="${dir%%/}"; ruby="${dir##*/}"
                if [[ "$dir" == "$RUBY_ROOT" ]]; then
                    echo " * ${ruby} ${RUBYOPT}"
                else
                    echo "  ${ruby}"
                fi

            done
            ;;
        system) chruby_reset ;;
        *)
            local dir ruby match
            for dir in "${RUBIES[@]}"; do
                dir="${dir%%/}"; ruby="${dir##*/}"
                case "$ruby" in
                    "$1")match="$dir" && break ;;
                    *"$1"*)match="$dir" ;;
                esac
            done

            if [[ -z "$match" ]]; then
                local no_match="chruby could not find match for $(dull_red $1.)"
                local still_using="still using $(dull_green "$(chruby | grep '^ \* ' | sed 's/ \* //')")"
                echo "\n  $(hot_red $no_match)\n  ($still_using)\n" >&2
                return 1
            fi

            shift
            chruby_use "$match" "$*"
            ;;
    esac
}

# auto.sh
unset RUBY_AUTO_VERSION
export CHRUBY_AUTOSWITCH=true

function chruby_auto() {
    local dir="$PWD/" version

    until [[ -z "$dir" ]]; do
        dir="${dir%/*}"

        if { read -r version <"$dir/.ruby-version"; } 2>/dev/null || [[ -n "$version" ]]; then
            version="${version%%[[:space:]]}"

            if [[ "$version" == "$RUBY_AUTO_VERSION" ]]; then return
            else
                RUBY_AUTO_VERSION="$version"
                echo "\$ chruby $(dull_red $version)"
                chruby "$version"
                return $?
            fi
        fi
    done

    if [[ -n "$RUBY_AUTO_VERSION" ]]; then
        chruby_reset
        unset RUBY_AUTO_VERSION
    fi
}

function chpwd() {
    [[ $CHRUBY_AUTOSWITCH == true ]] && chruby_auto
}
