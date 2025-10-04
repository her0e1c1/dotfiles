echo "Loading .profile at $(hostname) ... "

#==============================================================================
# ENVIRONMENT VARIABLES
#==============================================================================

# Core shell environment
export PS1="\u@\w\n$ "
export LANG=en_US.UTF-8
export PAGER=less
export CLICOLOR=1
export LSCOLORS=DxGxcxdxCxegedabagacad
export FZF_DEFAULT_OPTS="--reverse --height 40%"

# Editor configuration
if [ -n "$(which nvim)" ]; then
    export EDITOR=nvim
else
    export EDITOR=vi
fi

# Custom environment variables
export MYDIRS_HISTORY=~/.mydirs
export RECENT_FILES=~/.recent_files

# Docker container PATH handling
if [ -n "$PATH_ADDITIONAL" ]; then
    IFS=':' read -ra ADD_PATHS <<< "$PATH_ADDITIONAL"
    for p in "${ADD_PATHS[@]}"; do
        if [ -n "$p" ] && [ -d "$p" ]; then
            PATH="$p:$PATH"
        fi
    done
    export PATH
fi

#==============================================================================
# BASH-SPECIFIC CONFIGURATION
#==============================================================================

if echo "$SHELL" | grep -q bash; then
    export HISTCONTROL=ignoreboth:erasedups
    export HISTIGNORE="fg*:bg*:history:cd*:rm*"
    export HISTSIZE=100000

    shopt -u histappend
    
    share_history() {
        history -a
        history -c
        history -r
    }

    bash_post_command_hook() {
        local ecode=$?
        local e='$'
        if [ $ecode -ne 0 ]; then
            e="$(red '$')"
        fi
        update_files "$MYDIRS_HISTORY" "$(pwd)"
        local branch=""
        local origin=""
        if git status 2>/dev/null 1>/dev/null; then
            branch=$(green "$(git_current_branch)")
            origin=$(git config --get remote.origin.url)
        fi
        export PS1="\u@\w [$branch:$origin]\n[\H]$ "
        share_history
    }
    PROMPT_COMMAND="bash_post_command_hook"
fi

#==============================================================================
# UTILITY FUNCTIONS
#==============================================================================

# Basic utilities
debug() { set -x; "$@"; set +x; }
exists() { test -e "$(which "$1")"; }
watch() { while true; do clear; "$@"; sleep 1; done; }
color() { perl -E 'print qq/\x1b[38;5;${_}mC$_ / for 0..255; say'; }

# Path utilities
abspath() {
    if [ $# -eq 1 ]; then
        python3 -c "import os, sys; print(os.path.abspath(sys.argv[1]))" "$1"
    else
        echo "Usage: abspath <path>"
        return 1
    fi
}

# Color utilities
RED='\033[0;31m'
GREEN='\033[0;32m'
NOCOLOR='\033[0m'
red() { echo -e "${RED}$1${NOCOLOR}"; }
green() { echo -e "${GREEN}$1${NOCOLOR}"; }

# Number base conversion
d2h() { printf '%x\n' "$1"; }
h2d() { echo "ibase=16; $*" | bc; }
esc() { perl -plE "s#'#'\\''# "; }

#==============================================================================
# FILE AND DIRECTORY OPERATIONS
#==============================================================================

# Enhanced cd with ls and git status
cdls() {
    if [ $# -eq 0 ]; then
        \cd && ls
    else
        local d
        d=$(abspath "$1")
        if [ -d "$d" ]; then
            \cd "$d" && ls -G
        else
            echo "NO DIR: $d"
            return 1
        fi
    fi
    if git status 2>/dev/null 1>/dev/null; then
        git status
    fi
    pwd
    exists direnv && _direnv_hook
}

# File extraction utility
extract() {
    case "$1" in
        *.tar.gz|*.tgz) tar xzvf "$1";;
        *.tar.xz) tar Jxvf "$1";;
        *.zip) unzip "$1";;
        *.lzh) lha e "$1";;
        *.tar.bz2|*.tbz) tar xjvf "$1";;
        *.tar.Z) tar zxvf "$1";;
        *.gz) gzip -dc "$1";;
        *.bz2) bzip2 -dc "$1";;
        *.Z) uncompress "$1";;
        *.tar) tar xvf "$1";;
        *.arj) unarj "$1";;
        *) echo "Unknown archive format: $1";;
    esac
}

# File history management
update_files() {
    if [ $# -ne 2 ]; then
        echo 'Usage: update_files <filepath> <word>'
        echo 'Add <word> to the first line of <filepath>'
        return 1
    fi
    if [ ! -f "$1" ]; then
        touch "$1"
    fi
    cat << 'EOS' | python3 - "$@"
import sys
filepath, word = sys.argv[1:3]
word += "\n"

with open(filepath) as f:
    lines = f.readlines()
    for i, l in enumerate(lines):
        if l == word:
            lines.pop(i)
            lines = [l] + lines
            break
    else:
       lines = [word] + lines

with open(filepath, 'w') as f:
    f.writelines(lines)
EOS
}

#==============================================================================
# EDITOR FUNCTIONS
#==============================================================================

open_file() {
    local file="$1"
    if ! [ -f "$file" ]; then
        echo "$file doesn't exist"
        return 1
    fi
    update_files "$RECENT_FILES" "$(abspath "$file")"
    if [ $# -eq 2 ]; then
        $EDITOR "$file" "+$2"
    else
        $EDITOR "$file"
    fi
}

#==============================================================================
# FZF INTEGRATION FUNCTIONS
#==============================================================================

fzf_select_history() {
    local cmd
    cmd=$(history |
        tail -r |
        perl -plE 's#^\s*\d+\s*##' |
        perl -nlE 'say if length $_ >= 5' |
        perl -M"List::MoreUtils qw(uniq)" -E '@a=uniq <STDIN>; say @a' |
        fzf --prompt "$(pwd) > ")
    $cmd
}

fzf_select_docker_shell() {
    local name
    name=$(docker ps --format "{{.Names}}" | fzf --prompt "$(pwd) > ")
    if [ -n "$name" ]; then
        docker exec --detach-keys ctrl-q,q -it "$name" sh
    fi
}

fzf_select_recent_files() {
    if [ $# -eq 1 ]; then
        if [ ! -f "$1" ]; then
            touch "$1"
        fi
        open_file "$1"
        return
    fi
    cat "$RECENT_FILES" | perl -nlE 'say if -f' | fzf --prompt "$(pwd) > " | {
        read -r file
        if [ -f "$file" ]; then
            open_file "$file"
        fi
    }
}

fzf_find_word() {
    local w=""
    if [ $# -eq 0 ]; then
        read -p "Enter search word: " -r w
    else
        w="$1"
        shift
    fi
    local l
    l=$(grep -Inr "$w" . | fzf --prompt "$(pwd) > ")
    [ -z "$l" ] && return 1
    # grep format: filepath:line number: matching string
    local line file
    line=$(echo "$l" | perl -nlE 'say $1 if /:(\d+):/')
    file=$(echo "$l" | perl -nlE 'say $1 if /^(.*?):/')
    open_file "$file" "$line"
}

fzf_select_dir() {
    if [ ! -f "$MYDIRS_HISTORY" ]; then
        touch "$MYDIRS_HISTORY"
    fi
    if [ $# -eq 0 ]; then
        local d
        d=$(cat "$MYDIRS_HISTORY" | fzf --prompt "$(pwd) > ")
        if [ -d "$d" ]; then
            cdls "$d"
        else
            echo "Invalid directory: $d"
            return 1
        fi
    else
        cdls "$1"
    fi
}

#==============================================================================
# DOCKER FUNCTIONS
#==============================================================================

docker_purge() {
    read -p "Are you sure? [y]" -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        docker container prune -f
        docker volume prune -f
        docker system prune -f
        docker image prune -f
    fi
}

docker_compose_down() {
    docker compose ls --quiet | xargs -I{} docker compose -p {} down -v
}

docker_remove_all() { 
    docker rm -f "$(docker ps -aq)"
    docker network rm "$(docker network ls -q)"
}

docker_remove_volume() { 
    docker volume rm "$(docker volume ls -q)"
}

docker_remove_images() { 
    docker rmi "$(docker images | perl -anlE 'say "$F[2]" if $F[0] =~ /<none>/')"
}

#==============================================================================
# TRAEFIK FUNCTIONS
#==============================================================================

traefik_start() {
    local port="${1:-8888}"
    if ! docker info >/dev/null 2>&1; then
        echo "You need to start docker first to start traefik"
        return 1
    fi
    local lockfile="/tmp/traefik_start.lock"
    if mkdir "$lockfile" >/dev/null 2>&1; then
        if ! docker ps -a --format "{{.Names}}" | grep -x traefik >/dev/null 2>&1; then
            echo "Start traefik at port $port ..."
            docker run -d -it --rm --name traefik \
                -v /var/run/docker.sock:/var/run/docker.sock \
                -p "$port":80 traefik:v2.10 \
                --api.insecure=true \
                --providers.docker=true \
                --providers.docker.network=bridge \
                --providers.docker.exposedbydefault=false \
                --entrypoints.web.address=:80
        fi
        rmdir "$lockfile"
    else
        echo "Cannot start traefik. Remove '$lockfile' directory manually"
        return 1
    fi
}

traefik_end() {
    docker rm --force traefik
}

#==============================================================================
# GIT FUNCTIONS
#==============================================================================

git_current_branch() { 
    git rev-parse --abbrev-ref HEAD
}

git_submodule_update() { 
    git submodule update --recursive --remote
}

#==============================================================================
# NETWORK AND SYSTEM UTILITIES
#==============================================================================

port_kill() { 
    local port="$1"
    lsof -t -i "tcp:$port" | xargs kill -9
}

port_used() {
    lsof -i -P -n | grep LISTEN
}

ip_global() {
    curl http://wtfismyip.com/text
}

ip_info() {
    curl "ipinfo.io/$1"
}

#==============================================================================
# APPLICATION LAUNCHERS
#==============================================================================

open_vscode() { 
    local a="${1:-.}"
    VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args "$a"
}

open_intellij() { 
    /Applications/IntelliJ\ IDEA\ CE.app/Contents/MacOS/idea "$(pwd)"
}

#==============================================================================
# WEB AND SERVER UTILITIES
#==============================================================================

html_serve() {
    local cwd="${1:-.}"
    local port="${2:-8121}"
    (
        cd "$cwd" || exit
        python3 -m http.server "$port"
    )
}

#==============================================================================
# KUBERNETES UTILITIES
#==============================================================================

kubectl_kustomize() {
    local cwd="${1:-.}"
    kubectl kustomize --enable-helm "$cwd" | kubectl apply -f -
}

#==============================================================================
# PROXY AND NETWORKING
#==============================================================================

mac_socks5() {
    if [ $# -eq 0 ]; then
        echo 'Usage: mac_socks5 <ssh_name> [port] [device_name]'
        echo 'ssh_name: proxy server specified in ~/.ssh/config'
        return 1
    fi
    local target="$1"
    local port="${2:-9999}"
    local name="${3:-Wi-Fi}"
    
    if [ -z "$(networksetup -listnetworkserviceorder | grep "$name")" ]; then
        echo "Cannot find $name from networksetup"
        return 1
    fi
    
    echo "(SOCKS5) ssh $target -vND localhost:$port on $name"
    sudo networksetup -setsocksfirewallproxy "$name" localhost "$port"
    ssh "$target" -vND "localhost:$port"
    sudo networksetup -setsocksfirewallproxystate "$name" off
    echo "DONE"
}

#==============================================================================
# TEXT PROCESSING UTILITIES
#==============================================================================

replace() {
    local y=false
    while getopts y OPT; do
        case $OPT in
            y) y=true;;
            *) echo "Usage: replace [-y] FILE_PATTERN PERL_SUBSTITUTIONS"; return 1;;
        esac
    done
    shift $((OPTIND - 1))
    
    if [ $# -lt 2 ]; then
        echo 'Usage: replace [-y] FILE_PATTERN PERL_SUBSTITUTIONS'
        return 1
    fi
    
    local pattern="$1"
    shift
    
    find . | grep -E "$pattern" | {
        read -r filepath
        if [ $# -eq 1 ] && [ "$y" = false ]; then
            perl -nlE "$1 and say qq/\$ARGV: \$_/" "$filepath"
            return 0
        fi
        echo "========== Replacing $filepath =========="
        local t="$filepath"
        if ! $y; then
            t=$(mktemp)
            cp "$filepath" "$t"
        fi
        for s in "$@"; do
            perl -i -plE "$s" "$t"
        done
        if ! $y; then 
            cat "$t"
        fi
    }
}

#==============================================================================
# URL ENCODING ALIASES
#==============================================================================

alias urlencode='python3 -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python3 -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias timestamp='python3 -c "import sys, datetime as d; print(d.datetime.utcfromtimestamp(float(sys.argv[1])))"'
alias jsonload='python3 -c "import json,sys; a=json.loads(sys.stdin.read()); print(a)"'

#==============================================================================
# ALIASES
#==============================================================================

# Editor aliases
alias vim='fzf_select_recent_files'
alias vi="nvim"
alias v='nvim'

# Basic command aliases
alias m='make'
alias g="git"
alias ll='ls -alF'
alias ls='ls -aCF'
alias sl=ls
alias l=ls
alias r="stty sane"

# Kubernetes aliases
alias k="kubectl"
alias ka="kubectl apply -f"
alias kk="kubectl_kustomize"

# Docker aliases
alias d="docker"
alias dc="docker compose"
alias dcr="docker compose run --remove-orphans --rm"
alias dcu="docker compose up"
alias dcw="docker compose up --remove-orphans --force-recreate --watch watch"
alias dcd="docker compose down --remove-orphans --volumes"

# Project-specific aliases
alias me="docker compose -f docker-compose.me.yml"
alias mesh="docker compose -f docker-compose.me.yml run --remove-orphans sh"
alias ti="tmuxinator"
alias vs="open_vscode"

# Utility aliases
alias f="fzf_find_word"
alias cd="fzf_select_dir"

#==============================================================================
# KEY BINDINGS
#==============================================================================

bind -x '"\ew": fzf_select_docker_shell'
bind -x '"\eo": fzf_select_recent_files'
bind -x '"\ed": fzf_select_dir'
bind -x '"\eg": "cdls .."'
bind -x '"\C-r": fzf_select_history'
bind '"\C-xr": reverse-search-history'
bind '"\ei": edit-and-execute-command'

#==============================================================================
# EXTERNAL TOOL INITIALIZATION
#==============================================================================

which brew >/dev/null && eval "$(brew shellenv)"
which direnv >/dev/null && eval "$(direnv hook bash)"
which nodenv >/dev/null && eval "$(nodenv init -)"

echo "done!"