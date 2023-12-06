echo "LOADING ... `hostname`"

### EXPORT

export PS1="\u@\w\n$ "

export PATH="$HOME/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"  # for mac m1
export PATH=$HOME/.nodebrew/current/bin:$PATH
export PATH="/Applications/Docker.app/Contents/Resources/bin/:$PATH"

export LANG=en_US.UTF-8
export PAGER=less
export TERM=xterm-256color  # for zenburn-emacs
export CLICOLOR=1  # lsに色づけ
export LSCOLORS=DxGxcxdxCxegedabagacad
export VSCODE_HOME="$HOME/Library/Application Support/Code/User/"
export KARABINER_ASSETS="$HOME/.config/karabiner/assets/complex_modifications/"
export KARABINER_CONFIG="$HOME/.config/karabiner"

export MYDIRS_HISTORY=~/.mydirs
export RECENT_FILES=~/.recent_files

if [ -d "/usr/local/opt/go/libexec" ]; then
    export GOROOT=/usr/local/opt/go/libexec
elif [ -d "/opt/homebrew/opt/go/libexec" ]; then
    export GOROOT="/opt/homebrew/opt/go/libexec"
elif [ -d "$(brew --prefix golang)/libexec" ]; then
    export GOROOT="$(brew --prefix golang)/libexec"
fi

if [ -n "$(which vim)" ]; then
    export EDITOR=vim
else
    export EDITOR=vi
fi

if [ -d "$HOME/go/bin" ]; then
    export PATH="$PATH:$HOME/go/bin"
fi

### macOS

if [[ $OSTYPE == "darwin"* ]]; then
    ssh-add --apple-load-keychain  # ssh-add --apple-use-keychain ~/.ssh/[your-private-key]
fi

### INSTALL IF NEEDED

install_dotfiles() {
    cd ~
    [ ! -d ~/dotfiles ] && git clone https://github.com/her0e1c1/dotfiles.git;
    cd ~/dotfiles
    for file in `ls -1`; do
        if [ -f $file ]; then
            local p="$HOME/$name"
            ln -sf ~/dotfiles/$file ~/$file
        fi
    done
    if [ -d "$KARABINER_ASSETS" ]; then
        for i in `ls -1 ~/dotfiles/karabiner/assets/*.json`; do
            ln -sf $i "$KARABINER_ASSETS"
            echo "karabiner assets $i installed"
        done
    fi
    if [ -d "$KARABINER_CONFIG" ]; then
        cp ~/dotfiles/karabiner/karabiner.json "$KARABINER_CONFIG"
        echo "karabiner.json copied"
    fi
    if [ -d "$VSCODE_HOME" ]; then
        ln -sf ~/dotfiles/.vscode/settings.json "$VSCODE_HOME"
        ln -sf ~/dotfiles/.vscode/keybindings.json "$VSCODE_HOME"
        echo "vscode setting files installed"
    fi
    echo "source ~/.profile" >> ~/.bashrc
}

### UTILS

debug () { set -x; $@; set +x; }
exists () { test -e "$(which $1)"; }
repeat () { local n=$1; shift; for i in `seq $n`; do $@ ;done; }
watch () { while true; do clear; $@; sleep 1; done;}
chomp () { perl -pE "chomp \$_"; }
color(){ perl -E 'print qq/\x1b[38;5;${_}mC$_ / for 0..255; say'; }
abspath() {
    if [ $# -eq 0 ]; then
        python3 -c "import os, sys; print(os.path.abspath(sys.stdin.read()))";
    else
        python3 -c "import os; print(os.path.abspath('$1'))";
    fi
}

RED='\033[0;31m'
GREEN='\033[0;32m'
NOCOLOR='\033[0m'
red() { echo -e "${RED}$1${NOCOLOR}"; }
green() { echo -e "${GREEN}$1${NOCOLOR}"; }

d2h () { printf '%x\n' $1; }
h2d(){ echo "ibase=16; $@"|bc; }  # capitize
esc () { perl -plE "s#'#'\\''# "; }

port_used() {
    lsof -i -P -n | grep LISTEN
}

alias urlencode='python3 -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python3 -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias timestamp='python3 -c "import sys, datetime as d; print(d.datetime.utcfromtimestamp(float(sys.argv[1])))"'
alias jsonload='python3 -c "import json,sys; a=json.loads(sys.stdin.read()); print(a)"'

cdls(){
    if [ ${#1} -eq 0 ]; then
        \cd && ls
    else
        local d=`abspath $1`
        if [ -d $d ]; then
            \cd "$d" && ls -G
            # update_files $MYDIRS_HISTORY $d
        else
            echo "NO DIR: $d"
        fi
    fi
    if git status 2>/dev/null 1>/dev/null; then
        git status
    fi
    pwd
    exists direnv && _direnv_hook
}

# 圧縮ファイルを名前だけで展開
extract() {
  case $1 in
    *.tar.gz|*.tgz) tar xzvf $1;;
    *.tar.xz) tar Jxvf $1;;
    *.zip) unzip $1;;
    *.lzh) lha e $1;;
    *.tar.bz2|*.tbz) tar xjvf $1;;
    *.tar.Z) tar zxvf $1;;
    *.gz) gzip -dc $1;;
    *.bz2) bzip2 -dc $1;;
    *.Z) uncompress $1;;
    *.tar) tar xvf $1;;
    *.arj) unarj $1;;
  esac
}

### FOR BASH

if echo $SHELL | grep -q bash; then
    export HISTCONTROL=ignoreboth:erasedups
    export HISTIGNORE="fg*:bg*:history:cd*:rm*"  #よく使うコマンドは履歴保存対象から外す。
    export HISTSIZE=100000  #ヒストリのサイズを増やす

    # 履歴の共有
    shopt -u histappend
    function share_history {
        history -a
        history -c
        history -r
    }

    bash_post_command_hook() {
        local ecode=$?
        local e='$'
        # [ $ecode -eq 0 ] || e=`red \$`
        update_files $MYDIRS_HISTORY `pwd`
        local branch=""
        local origin=""
        if git status 2>/dev/null 1>/dev/null; then
            branch=$(green `git_current_branch`)
            origin=`git config --get remote.origin.url`
        fi
        export PS1="\u@\w [$branch:$origin]\n$e "
        share_history
    }
    PROMPT_COMMAND="bash_post_command_hook"
fi

## EDITOR

open_file() {
    local file=$1;
    if ! [ -f $file ]; then
       echo "$file doesn't exist"
       return 1
    fi
    update_files $RECENT_FILES `abspath $file`
    if [ $# -eq 2 ]; then
        $EDITOR $file +$2
    else
        $EDITOR $file
    fi

}

### PECO

peco_git_branch() {
    local b=$(git branch -a | perl -plE 's#^\* ##'| perl -plE 's#.*?(origin|upstream)/##' | peco --prompt `git_current_branch`)
    git checkout $b
}

peco_select_history() {
    local cmd=$(history |
    tail -r |
    perl -plE 's#^\s*\d+\s*##' |
    perl -nlE 'say if length $_ >= 5' |
    perl -M"List::MoreUtils qw(uniq)" -E '@a=uniq <STDIN>; say @a' |
    peco --prompt `pwd`)
    echo $cmd
    [ -x `which pbcopy` ] && echo $cmd | pbcopy
}

peco_select_docker_shell() {
    declare l=$(docker ps --format "{{.Names}}" | peco --prompt `pwd`)
    if [ -z "$l" ]; then
        return  # do nothing
    else
        docker exec --detach-keys ctrl-q,q -it $l sh
    fi
}

peco_select_recent_files() {
    if [ $# -eq 1 ]; then
        if [ ! -f $1 ]; then
            touch $1
        fi
        open_file $1
        return
    fi
    # share emacs recent files
    declare l=$(cat $RECENT_FILES | perl -nlE 'say if -f' | peco --prompt `pwd`)
    if [ -z "$l" ]; then
        return  # do nothing
    elif [ -f $l ]; then
        cd `dirname $l`
        open_file $l
    elif [ -d $l ]; then
        cdls $l
    fi
}

peco_select_find() {
    local tmp=/tmp/peco
    [ -f $tmp ] && rm $tmp
    ls -1 | ignore_files >> $tmp
    find . -maxdepth 4 | ignore_files >> $tmp
    local l=$(cat $tmp | peco --prompt `pwd` | abspath)
    echo $l
    if [ -z $l ] ;then
        return # do nothing
    elif [ -f $l ]; then
        cd `dirname $l`
        open_file $l
    else
        cdls $l
    fi
}

peco_ssh () {
    local q=$1
    cat ~/.ssh/config | perl -nlE '/^Host (.*)/ and say $1 if !/github/i' | grep "$1" | peco --select-1
}

peco_replace() {
    local pattern=$1; shift
    local file=$1; shift
    if [ -z $file ]; then
        file=$(ls -1a | peco --prompt `pwd`)
    fi
    perl -plE "$pattern" $file
}

peco_grep_word() {
    local w=""
    if [ $# -eq 0 ];then
        read -p "Enter search word: " w
    else
        w=$1; shift
    fi
    local l=$(grep -Inr "$w" . | peco --prompt `pwd`)
    [ -z "$l" ] && return;
    # grep format: filepath:line number: matching string
    local line=$(echo $l | perl -nlE 'say $1 if /:(\d+):/')
    local file=$(echo $l | perl -nlE 'say $1 if /^(.*?):/')
    open_file $file $line
}

peco_select_dir () {
    if [ ! -f $MYDIRS_HISTORY ]; then
        touch $MYDIRS_HISTORY
    fi
    if [ $# -eq 0 ]; then
        local d=`cat $MYDIRS_HISTORY | peco --prompt $(pwd)`
        if [ -d "$d" -a -n "$d" ]; then
            cdls $d
        fi
    else
        local d=`python3 -c "import os; print(os.path.abspath('$1'))"`
        cdls $d
    fi

}

peco_docker_commit() {
    docker ps | tail +2 | peco | perl -anlE '$cmd = "docker commit $F[-1] $F[1]"; say $cmd; system $cmd'
}

peco_stash_list() {
    git stash list | peco | perl -alnE '$s = substr($F[0], 0, -1); $cmd = "git stash apply $s"; system $cmd'
}

### DOCKER

docker_purge() {
    read -p "Are you sure? [y]" -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        docker container prune -f
        docker volume prune -f
        docker system prune -f
    else
        echo "DO NOTHING"
    fi
}

docker_kill()        { docker rm -f `docker ps -aq`; docker network rm `docker network ls -q`; }
docker_rename()      { docker tag $1 $2; docker rmi $1; }
docker_remove_volume () { docker volume rm `docker volume ls -q`; }
docker_remove_images() { docker rmi `docker images | perl -anlE 'say "$F[2]" if $F[0] =~ /<none>/'`; }
docker_compose_all() { docker-compose `perl -E 'say map {" -f \$_"} reverse <docker-compose*.yml>'` $@; }
docker_find_process_name () { docker ps -a --format "{{.Names}}" | grep $1 > /dev/null; }
docker_process_alive () { docker ps --format "{{.Names}}" | perl -E "exit !(grep {\$_=~ /^$1\$/} <STDIN>);"; }
docker_working() { docker inspect $1 | python3 -c 'import sys, json; print(json.loads(sys.stdin.read())[0]["Mounts"][0]["Source"])';}
docker_alias() { docker tag $1 $2; }
docker_export() { docker export $1 | tar tf -; }
images_rstudio() { docker run --name rstudio -v `pwd`:/w -w /wn --rm -it -p 8787:8787 rocker/hadleyverse; }
docker_push () {
    local image=$1; shift
    docker tag $image $DOCKER_ID_USER/$image
    docker push $DOCKER_ID_USER/$image
}

emacs() {
    if docker_find_process_name emacs; then
        docker rm -f emacs
    fi
    touch ~/.recentf
    docker run -itd --name emacs \
	  -w "$HOME" \
	  -v "$HOME:$HOME" \
	  -v "$HOME/dotfiles/.emacs:/root/.emacs" \
	  -e TERM=xterm-256color \
	  -e LC_CTYPE=UTF-8 \
	  her0e1c1/emacs sh -c "emacs --daemon && bash -l"
      # -v ~/.recentf:/root/.emacs.d/recentf \
}

emacsclient () {
    local q=""
    if [ $# -eq 0 ]; then
        local p=`pwd`
    else
        local p=`perl -E "use Cwd 'abs_path'; say abs_path('$1')"`
        if [ $# -eq 2 ]; then
            q="+$2:0"
        fi
    fi
    if [ -e $p ]; then
        update_files $RECENT_FILES $p
        docker exec -it emacs script -q -c "'/bin/bash' -c 'emacsclient $q -t $p'" /dev/null
    else
        echo "$p does not exist"
    fi
}

de() {
    local rflag=false
    while getopts rh OPT; do
        case $OPT in
            r) rflag=true;;
            h) docker-exec-help; return 0;;
        esac
    done
    shift $((OPTIND - 1))
    if $rflag; then
        docker-volume-remove $@
    elif [ $# -eq 0 ]; then
        docker ps
    else
        local name=$1; shift
        if docker ps -a --format "{{.Names}}##{{.Status}}"| grep "$name##Exited"; then
            echo "Start ..."
            docker start $name
        fi
        if [ $# -ne 0 ]; then
            # $@はかなり特殊な変数(配列っぽい動きする。そのため他の変数に代入できないっぽい)
            docker exec -it --detach-keys ctrl-q,q $name $@
        else
            # docker exec -it --detach-keys ctrl-q,q $name sh -c "cd `pwd`; exec /bin/bash --init-file /etc/profile"
            docker exec -it --detach-keys ctrl-q,q $name bash
        fi
    fi
}

dr() {
    local eflag=false
    while getopts e OPT; do
        case $OPT in
            e) eflag=true;;
        esac
    done
    shift $((OPTIND - 1))
    if [ $# -eq 0 ]; then
        docker images
    else
        local name=$1; shift
        local cmd=/bin/bash
        local env=""
        if $eflag; then
            env=`perl -E 'say map {chomp; "-e $_ "} qx/env/'`
        fi
        [ $# -ne 0 ] && cmd=$@
        touch ~/.profile  # ensure the file exists
        # --add-host=docker:$HOSTIP
        sh -c "docker run --rm -p 9999 $env -v ~/.profile:/root/.profile -v /Users:/Users -w `pwd` --detach-keys ctrl-q,q -it $name $cmd"
    fi
}

read_env() {
    local envfile=$1
    local prefix=$2
    if [ -z $envfile ]; then
        envfile=`ls -1 ~/.env/${prefix}*.env | peco --select-1`
    fi
    if [ ! -f $envfile ]; then
        echo "NO FILE $envfile" >&2;
        return 1
    fi
    local tmp=`mktemp`
    cat $envfile | perl -plE 's/^/export /' > $tmp
    source $tmp
    rm $tmp
    return 0
}

port_forwarding() {
    local envfile=$1
    if [ -z $envfile ]; then
        envfile=`ls -1 ~/.env/ssh*.env | peco --select-1`
    fi
    if [ ! -f $envfile ]; then
        echo "NO FILE $envfile" >&2;
        return 1
    fi
    source $envfile
    if check_port_used $SSH_PORT_FROM; then
        echo "PORT IS USED: $SSH_PORT_FROM" >&2;
        return 1
    fi
    local cmd="ssh $SSH_NAME -gNL $SSH_PORT_FROM:$SSH_HOST_TO:$SSH_PORT_TO"
    echo $cmd
    eval $cmd
}

# Add default docker options
docker_run() {
    local image=$1; shift
    local name=`perl -E '$ARGV[0] =~ /(.*):/; say $1' $image`
    docker_process_alive $name && docker rm -f $name
    touch ~/.profile  # ensure the file exists
    # --add-host=docker:$HOSTIP
    docker run "$@" --name $name -it --rm \
           -p 9999 \
           -v ~/.profile:/etc/profile -v /Users:/Users \
           --detach-keys ctrl-q,q \
           $image
}

ubuntu() {
    docker run -it --rm -v `pwd`:`pwd` -w `pwd` ubuntu bash
}

pyfmt() {
    local dir=${1:-.}
    local cwd=`pwd`
    local cmd0="clear; docker run -it --rm -v $cwd:/w -w /w her0e1c1/dev:py pycodestyle --max-line-length=120 $dir"
    local cmd1="watchmedo shell-command -W --recursive --pattern \"*.py;\" --command \"$cmd0\" $dir"
    local cmd2="clear; docker run -it --rm -v $cwd:/w -w /w her0e1c1/dev:py pylint --errors-only $dir"
    local cmd3="watchmedo shell-command -W --recursive --pattern \"*.py;\" --command \"$cmd2\" $dir"
    tmux split-window -p 20 "tmux split-window -hp 100 '$cmd0; $cmd1'; tmux split-window -hp 50 '$cmd2; $cmd3'"
}

gr () { find . -type f -exec grep -nH -e $1 {} \;; }

update_files () {
    if [ ! -f $1 ]; then
        touch $1
    fi
    local s=$(cat <<EOS
import sys

if len(sys.argv) == 2:
    exit(1)

filepath = '''$1'''
word = '''$2\n'''

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
)
    python3 -c "$s" $@
}

### GIT

git_current_branch() { git rev-parse --abbrev-ref HEAD; }

git_submodule_update() { git submodule update --recursive --remote; }

docker_edit_file() {
    local name="$1"; shift
    local fpath="$1"; shift
    if docker exec -it $name test -f $fpath; then
        local tmp=`mktemp`
        local w=`docker exec $name pwd`
        docker cp $name:$fpath $tmp
        vim $tmp
        docker cp $tmp $name:$fpath

    else
        docker exec -it $name ls -1aFG $fpath
    fi
}

### OTHERS (no more used)

# docker run のタイミングでsyncもできるようにするか(指定したディレクトリを監視するみたいな)
# または、cp cpを2回繰り返す! (または docker-sync name /path ./host_side)
# host側のイベントを取りにいけない...
docker_sync () {
    if [ $# -eq 1 ]; then
        docker exec -it $1 /bin/bash
        return 0
    fi

    local name=$1; shift
    local src=$1; shift
    local sync=".docker-sync/$name/`basename $src`"
    local trim=`perl -E '\$_=\$ARGV[0]; s#/*\$## and say' $src`
    local working=`docker_working $name`

    echo "cd $working"
    \cd $working

    # コンテナに必ずしもrsyncがインストールされているとは限らないが必須
    if ! docker exec $name which rsync; then
        echo "Install rsync on $name"
        docker exec $name apt-get update -y;
        if ! docker exec $name apt-get install -y rsync; then
            return 1
        fi
    fi

    if ! docker exec $name test -d $sync; then
        local d=`dirname $sync`
        docker exec $name sh -c "[ ! -d $d ] && mkdir -p $d"

        # echo "cp $src $sync on host"
        # docker exec $name cp -r $src $sync

        echo "rsync $sync on host"
        # docker exec $name rsync -avz --exclude '*.git*' $trim/ $sync
        docker exec $name rsync -avz $trim/ $sync
    fi
    if docker exec $name test -d $sync; then
        if [ -d "$sync" ]; then
            echo "start sync ... on $working"
            echo watchmedo shell-command -R "$sync" -c "docker exec $name rsync -avz --exclude '*.git*' $sync/ $trim" $@
            watchmedo shell-command -R "$sync" -c "docker exec $name rsync -avz --exclude '*.git*' $sync/ $trim" $@
        else
            echo "You can't sync on `pwd`. Go to $sync on host"
        fi
    else
        echo "$sync dir is not found on docker."
    fi
}

kill_port () { local port=$1; lsof -t -i tcp:$port | xargs kill -9; }

open_vscode () { local a=${1:-.}; VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args $a; }
intellij () { /Applications/IntelliJ\ IDEA\ CE.app/Contents/MacOS/idea `pwd`; }

ssl_expired() { openssl s_client -connect $1:443 < /dev/null | openssl x509 -text |grep Not; }
ssl_crt() { openssl req -nodes -newkey rsa:2048 -keyout myserver.key -out server.csr; }

ip_global() {
    curl http://wtfismyip.com/text
}

ip_info() {
    curl ipinfo.io/$1
}

web_score_loading() {
    if [ $# -eq 0 ]; then
        echo "NEED url";
		return
    fi
	local url="https://www.googleapis.com/pagespeedonline/v5/runPagespeed?url=$1"
    curl "$url" | jq .lighthouseResult.categories.performance.score
}

mac_socks5() {
    if [ $# -eq 0 ]; then
        echo 'USAGE: cmd $ssh_name [$port] [$device_name]'
        echo '$ssh_name: must be proxy server specified in ~/.ssh/config'
        return 1
    fi
    local target=$1
    local port=${2:-9999}
    local name=${3:-Wi-Fi}
    if [ -z "$target" ]; then
        echo "Need one argument for ssh"
        return 1
    fi
    if [ -z "`networksetup -listnetworkserviceorder | grep $name`" ]; then
        echo "Can not find $name from networksetup"
        return 1
    fi
    echo "(SOCKS5) ssh $target -vND localhost:$port on $name"
    sudo networksetup -setsocksfirewallproxy "$name" localhost "$port"
    ssh "$target" -vND "localhost:$port"
    sudo networksetup -setsocksfirewallproxystate "$name" off
    echo "DONE"
}

### ALIAS

alias vim='peco_select_recent_files'
alias vi='vim'
alias v='vim'
alias docker_down="docker-compose down  --remove-orphans"
alias g="git"
alias ll='ls -alF'
alias ls='ls -aCF'
alias sl=ls
alias l=ls
alias sudo="sudo "  # sudo時にアリアス有効
alias f="peco_select_recent_files"
alias w="peco_grep_word"
alias d="peco_select_dir"
alias cd="peco_select_dir"
alias r="stty sane"
alias me="docker-compose -f docker-compose.me.yml"
alias mesh="docker-compose -f docker-compose.me.yml run sh"
alias ti="tmuxinator"
alias vs="open_vscode"

# TODO: use docker compose
alias dc="docker-compose"
alias dcr="docker-compose run"
alias dcu="docker-compose up"

### BINDS

bind -x '"\eb": peco_git_branch'
bind -x '"\es": ssh_peco'
bind -x '"\eS": peco_stash_list'
bind -x '"\ew": peco_select_docker_shell'
bind -x '"\ef": peco_select_find'
bind -x '"\eo": peco_select_recent_files'
bind -x '"\ed": peco_select_dir'
bind -x '"\eC": peco_docker_commit'
bind -x '"\eu": "cdls .."'
bind -x '"\C-r": peco_select_history'
bind    '"\C-xr": reverse-search-history'
bind '"\ei": edit-and-execute-command'

which direnv && eval "$(direnv hook bash)"
which nodenv && eval "$(nodenv init -)"

if [ -d "$(brew --prefix)/Caskroom/google-cloud-sdk/latest/google-cloud-sdk" ]; then
  source "$(brew --prefix)/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc"
  source "$(brew --prefix)/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc"
fi

echo "DONE"
