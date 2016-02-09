
if on_linux; then
    prompt_branch_color='cyan'
    prompt_path_color='black'
else
    prompt_branch_color='yellow'
    prompt_path_color='red'
fi

# first line of `git status` is either `On branch xxx` or `HEAD detached at xxx`
function nth_word_of_gs()       { git status | head -1 | awk "{print \$$1}" }
function detached_head()        { [[ $(nth_word_of_gs 2) == 'detached' ]] }
function detached_head_status() { nth_word_of_gs 4 }
function attached_head_status() { nth_word_of_gs 3 }

function current_dir()    { echo "[%{$fg_bold[$prompt_path_color]%}%~%{$reset_color%}]" }
function current_branch() {
    if [[ -a .git/refs/heads ]] || [[ -a ../.git/refs/heads ]] || [[ -a ../../.git/refs/heads ]]; then
        if detached_head; then
            branch=$(detached_head_status)
        else
            branch=$(attached_head_status)
        fi
        echo "[%{$fg_bold[$prompt_branch_color]%}$branch%{$reset_color%}]"
    else
        echo ""
    fi
}

export PS1='$(current_branch)$(current_dir) '
