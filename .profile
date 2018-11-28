
# You can install files by this command
# $ curl https://raw.githubusercontent.com/her0e1c1/dotfiles/master/.profile -o ~/.profile && . ~/.profile
# install_dotfiles

# set -x

### TODO: - L, T (pbcopy)
### TODO: 本番環境のPANEに色付け

echo "LOADING ... `hostname`"

### EXPORT

# \h hostname
# \t \T 24 12 time
# \w \W currend dir
# \u user name
export PS1="\u@\w\n$ "
export PATH="/Applications/Docker.app/Contents/Resources/bin/:$PATH"
export PATH=$PATH:./node_modules/.bin
# export LC_ALL=C
# export LC_ALL=en_US.UTF-8
export VSCODE_SETTINGS="$HOME/Library/Application Support/Code/User/settings.json"
export GOPATH=~/go
export GOBIN=~/go/bin
export PATH="$PATH:$GOPATH/bin:$GOBIN"
export PAGER=less
export TERM=xterm-256color  # for zenburn-emacs
export CLICOLOR=1  # lsに色づけ
export LSCOLORS=DxGxcxdxCxegedabagacad
export DOCKER_ID_USER=her0e1c1

export MYDIRS_HISTORY=~/.mydirs
export MY_ENV=~/.my.env
export MYCMDS_HISTORY=~/.mycmds
export RECENT_FILES=~/.recent_files

# android for mac
export ANDROID_HOME=${HOME}/Library/Android/sdk
export PATH=${PATH}:${ANDROID_HOME}/tools
export PATH=${PATH}:${ANDROID_HOME}/platform-tools

if uname | grep "Darwin"; then
    export GOROOT=/usr/local/opt/go/libexec
fi

if [ -n "$(which vim)" ]; then
    export EDITOR=vim
else
    export EDITOR=vi
fi

if [ -f $MY_ENV ]; then
    . $MY_ENV
fi
   
### INSTALL IF NEEDED

setup_mac () {
    install_dotfile
    install_brew
}

# INSTALL

install_brew () {
    if ! which brew 2>&1 1>/dev/null; then
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    brew install tmux go peco ansible
    brew cask install docker
}

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
    # only for mac
    ln -sf ~/dotfiles/.vscode/settings.json "$VSCODE_SETTINGS" 
}

### UTILS

debug () { set -x; $@; set +x; }
exists () { test -e "$(which $1)"; }
repeat () { local n=$1; shift; for i in `seq $n`; do $@ ;done; }
watch () { while true; do clear; $@; sleep 1; done;}
chomp () { perl -pE "chomp \$_"; }
color(){ perl -E 'print qq/\x1b[38;5;${_}mC$_ / for 0..255; say'; }
RED='\033[0;31m'
GREEN='\033[0;32m'
NOCOLOR='\033[0m'
red() { echo -e "${RED}$1${NOCOLOR}"; }
green() { echo -e "${GREEN}$1${NOCOLOR}"; }

d2h () { printf '%x\n' $1; }
h2d(){ echo "ibase=16; $@"|bc; }  # capitize
esc () { perl -plE "s#'#'\\''# "; }

check_port_used() {
    lsof -i -P -n | grep LISTEN | grep ":$1" > /dev/null;
}

ignore_files() {
    perl -nlE 'say if ! m#\.(pyc)$#'
    perl -nlE 'say if ! m#\.git#'
}

alias urlencode='python -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias timestamp='python -c "import sys, datetime as d; print(d.datetime.fromtimestamp(float(sys.argv[1])))"'

# alias crypt='python -c "import crypt; print crypt.crypt(, \"$1$SomeSalt$\")"'
# python -c 'import crypt; print crypt.crypt("PASSWD", "$1$SomeSalt$")'

cdls(){
	if [ ${#1} -eq 0 ]; then
	    \cd && ls
	else
        local d=`python -c "import os; print(os.path.abspath('$1'))"`
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
}

#圧縮ファイルを名前だけで展開
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

### BINDS

# stty -a
# bind -p
# INPUTRC=`mktemp`
# cat <<EOS >> $INPUTRC
# set bell-style none
# set meta-flag on
# set input-meta on
# set convert-meta off
# set output-meta on
# "\e[1~": beginning-of-line
# 
# set convert-meta off
# set blink-matching-paren on
# set editing-mode vi
# 
# set keymap vi-command
# # these are for vi-command mode
# "\C-i": undo
# 
# set keymap vi-insert
# # these are for vi-insert mode
# "\C-p": previous-history
# "\C-n": next-history
# "\C-l": clear-screen
# "\C-g": vi-movement-mode
# 
# EOS
# bind -f $INPUTRC
# [ ! -f ~/.inputrc ] && cp $INPUTRC ~/.inputrc

### FOR BASH
if echo $SHELL | grep -q bash; then
    export HISTCONTROL=ignorespace  # 空白から始めたコマンドを無視
    export HISTCONTROL=ignoredups  # 重複履歴を無視
    export HISTCONTROL=ignoreboth  # 両方
    # 履歴の共有
    function share_history {  # 以下の内容を関数として定義
        history -a  # .bash_historyに前回コマンドを1行追記
        # history -c  # 端末ローカルの履歴を一旦消去
        history -r  # .bash_historyから履歴を読み込み直す
    }
    PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
    shopt -u histappend   # .bash_history追記モードは不要なのでOFFに
    #よく使うコマンドは履歴保存対象から外す。
    export HISTIGNORE="fg*:bg*:history:cd*"
    #ヒストリのサイズを増やす
    export HISTSIZE=100000
    # bash_pre_command_hook() {  # 2回呼ばれる...
    #     ;
    # }
    # trap "bash_pre_command_hook" DEBUG
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
    }
    PROMPT_COMMAND="bash_post_command_hook"
fi

## EDITOR

open_file() {
    local file=$1; shift
    if ! [ -f $file ]; then
       echo "$file doesn't exist"
       return 1
    fi
    update_files $RECENT_FILES $file
    if docker_find_process_name emacs; then
        emacsclient $file
    else
        $EDITOR $file 
    fi
}

### PECO

peco_prompt() {
    echo `pwd`
    # if git status 2>/dev/null 1>/dev/null; then
    #     echo `git_current_branch`
    # fi
}

peco_git_branch() {
    local b=$(git branch -a | perl -plE 's#^\* ##'| perl -plE 's#.*?(origin|upstream)/##' | peco --prompt `git_current_branch`)
    git checkout $b
}

peco_select_history() {
    declare cmd=$(
        history |
        perl -plE 's#^\s*\d+\s*##' |
        perl -nlE 'say if length $_ >= 4' |
        perl -M"List::MoreUtils qw(uniq)" -E '@a=uniq <STDIN>; say @a' |
        uniq |
        peco --prompt `pwd`
    )
    eval "$cmd"
    # READLINE_LINE="$l"  # bash ver >= 4
    # READLINE_POINT=${#l}
    # if [ `uname` = "Darwin" ]; then
    #     # this doesn't work on current sierra tmux
    #     echo "${READLINE_LINE}" | chomp | pbcopy
    # fi
}

peco_select_docker_shell() {
    declare l=$(docker ps --format "{{.Names}}" | peco --prompt `pwd`)
    if [ -z "$l" ]; then
        return  # do nothing
    else
        docker exec -it $l bash
    fi
}

peco_select_recent_files() {
    if [ $# -eq 1 ]; then
        open_file $1
        return
    fi
    # share emacs recent files
    declare l=$(cat $RECENT_FILES | peco --prompt `pwd`)
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
    find . -maxdepth 3 | ignore_files >> $tmp
    local l=$(cat $tmp | peco --prompt `pwd`)
    if [ -z $l ] ;then
        return # do nothing
    elif [ -f $l ]; then
        open_file $l
    else
        cdls $l
    fi 
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
    emacsclient $file $line
}

peco_select_dir () {
    if [ ! -f $MYDIRS_HISTORY ]; then
        touch $MYDIRS_HISTORY
    fi
    if [ $# -eq 0 ]; then
        local a=`peco_prompt`
        echo $a
        local d=`cat $MYDIRS_HISTORY | peco --prompt $(peco_prompt)`
        if [ -d $d ]; then
            cdls $d
            # update_files $MYDIRS_HISTORY $d
        fi
    else
        local d=`python -c "import os; print(os.path.abspath('$1'))"`
        # update_files $MYDIRS_HISTORY $d
        cdls $d
    fi

}


anything() {
    # ACTION: SOMETHING のフォーマットでなんでもやるインターフェイスをつくる
    # TODO: grep |peco | open file
    # 現在のディレクトリ(ls, find)
    # 最近のfile/dir/cmmand
    # お気に入りのcommand/dir/file
    ls
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
docker_working() { docker inspect $1 | python -c 'import sys, json; print(json.loads(sys.stdin.read())[0]["Mounts"][0]["Source"])';}
docker_alias() { docker tag $1 $2; }
docker_export() { docker export $1 | tar tf -; }
images_rstudio() { docker run --name rstudio -v `pwd`:/w -w /wn --rm -it -p 8787:8787 rocker/hadleyverse; }

docker_es5() { docker_alias elasticsearch:5 es5; docker_run es5:latest -p 19205:9200; }
docker_redis() { docker_run redis:3.0; }
docker_mysql() {
    local cnf=/tmp/my.cnf
    cat <<EOS >> $cnf
[mysqld]
max_allowed_packet = 32M
EOS
    docker_run mysql:5.7 -e MYSQL_ALLOW_EMPTY_PASSWORD=1 -e MYSQL_DATABASE=db -v $cnf:/etc/mysql/conf.d/my.cnf;
}

docker_nginx() {
    local cnf=/tmp/nginx.conf
    cat <<EOS > $cnf
events {}
http {
  keepalive_timeout 1;
  server {
    root /data;
    server_name localhost;
    location / {
      proxy_max_temp_file_size 0;
      autoindex on;  # list files in dir
    }
  }
}
EOS
    docker run --rm -it -v `pwd`:/data -v $cnf:/etc/nginx/nginx.conf --name fs -p 8888:80 nginx:1.13
}

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

ssh_port_forwarding() {
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

phpmyadmin() {
    read_env "$1" pma || return 1
    local name=pma_$DOCKER_PORT
    local conf=~/.phpmyadmin/$name.config.php:/etc/phpmyadmin/config.user.inc.php
    local envs=`perl -E 'say join " ", map {"-e $_=$ENV{$_}"} (grep {/^PMA_/} %ENV)'`
    local cmd="docker run -v $conf --rm --name $name -p $DOCKER_PORT:80 $envs phpmyadmin/phpmyadmin"
    echo "start phpmyadmin ($name)"
    open http://localhost:$DOCKER_PORT  # don't care if $cmd is failed
    eval $cmd
}

mysql_dump() {
    if [ ! $# -eq 2 ]; then
        echo 'USAGE: mysqldump $database $envfile' >&2
        return 1
    fi
    read_env "$2" pma || return 1
    docker run -a stdout --rm mysql:8.0 mysqldump -h $PMA_HOST -P $PMA_PORT -u $PMA_USER -p$PMA_PASSWORD $1
}

mysql_store() {
    if [ ! $# -eq 3 ]; then
        echo 'USAGE: mysql_store $container_name $database $dumpfile' >&2
        return 1
    fi
    local name=$1
    local db=$2
    local dump=$3
    docker exec $name mysql -e "drop database $db; create database $db;"
    docker exec -i $name mysql --init-command="SET SESSION FOREIGN_KEY_CHECKS=0;" $db < $dump
}

# Add default docker options
docker_run() {
    local image=$1; shift
    local name=`perl -E '$ARGV[0] =~ /(.*):/; say $1' $image`
    docker_process_alive $name && docker rm -f $name
    touch ~/.profile  # ensure the file exists
    docker run "$@" --name $name -it --rm \
           -p 9999 --add-host=docker:$HOSTIP \
           -v ~/.profile:/etc/profile -v /Users:/Users \
           --detach-keys ctrl-q,q \
           $image
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
    python -c "$s" $@
}

open_recent_file () {
    local d=`cat $RECENT_FILES | peco`
    e $d
}

### GIT

alias git_add_stream="git remote add upstream"
# alias git_update="git checkout master; git fetch upstream; git merge upstream/master; git push"

git_origin() { git config --get remote.origin.url; }
git_first_commit() { git log --oneline | tail -1 | perl -nE '/(\w*?) /; say $1'; }

git_update() {
    git checkout master
    # if echo `git_origin` | grep 1 ; then
    # fi
    git fetch upstream
    git merge upstream/master
    git push
}

git_current_branch() { git rev-parse --abbrev-ref HEAD; }
git_branch_remove () {
        if [ $# -eq 0 ]; then
        git branch
        return
    fi
    local pattern=$1; shift
    git branch | grep $pattern | xargs -n 1 -P 4 git push --delete origin
    git branch | grep $pattern | xargs git branch -D
}
git_branch_remove_all () {
    git branch | xargs -n 1 -P 4 git push --delete origin
    git branch | xargs git branch -D
}

git_pr () {
    local number=$1; shift
    local remote=${1-upstream}
    local branch="PR$number"
    if git branch |grep $branch; then
        git checkout master
        git branch -D $branch
    fi
    git fetch $remote pull/$number/head:$branch
    git checkout $branch
}

git_pr_origin () {
    local number=$1; shift
    git_pr $number "origin"
}

git_make () {
    if [ -z "$GITHUB_TOKEN" -o -z "$GITHUB_USERNAME" ]; then
        echo "NO GITHUB_TOKEN or GITHUB_USERNAME"
        return 1
    fi
    local base=${1-master}; shift
    local title=`git log -1 --pretty=%B`

    local remote="origin";
    local branch="`git_current_branch`"
    local head="`git_current_branch`"
    if git remote -v | grep upstream > /dev/null; then
        remote="upstream"
        head="$GITHUB_USERNAME:$head"
        # head="$head"
    fi 
    echo git push origin $branch
    git push origin $branch

    local p=`git remote -v | grep $remote | perl -nlE 'say $1 if /:(.*?)(.git)? /' | head -1`
    local url="https://api.github.com/repos/$p"
    echo "make PR: $head on $base at $remote $url"
    echo "$title"
    curl -XPOST $url/pulls?access_token=$GITHUB_TOKEN -d "{\"title\": \"$title\", \"head\": \"$head\", \"base\": \"$base\", \"body\": \"\"}" 
}

git_submodule_init() {
    git submodule init && git submodule update;
}

# git submodule foreach git reset --hard HEAD
# git submodule update
git_submodule_update() { git submodule update --recursive --remote; }

h () {
    if [ ! -f $MYCMDS_HISTORY ]; then
        touch $MYCMDS_HISTORY
    fi
    if [ $# -eq 0 ]; then
        local d=`cat $MYCMDS_HISTORY | peco | perl -pE 'chomp $_'`
        update_files $MYCMDS_HISTORY $d
        # echo -n "$d" | tmux_set_buffer
        echo "$d" | chomp | pbcopy
    else
        local d=`echo $@`
        update_files $MYCMDS_HISTORY "$d"
        $@
    fi
}

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

def() { docker_edit_file "$@"; }

### TMUX

tmux_show_buffer () { tmux show-buffer | peco | chomp | pbcopy; }
tmux_set_buffer() { perl -E '@a=<stdin>; `tmux set-buffer "@a"`'; }
tmux_show_buffers() { perl -E 'say `tmux show-buffer -b $_ ` for 0..20'; }

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

r() {
    local path=$1; shift
    local ext="`f_ext $path`"
    local wext="`f_without_ext $path`"
    local dir="`f_dirname $path`"
    local base="`f_basename $path`"
    local base_wext="`f_without_ext $base`"
    local cmd="docker run -it -v `pwd`:/w --rm"
    if [ "$ext" = "go" ]; then
        $cmd "golang:dev" go run "/w/$path"
    elif [ "$ext" = "hs" ]; then
        $cmd "haskell:dev" runhaskell "/w/$path"
    elif [ "$ext" = "js" ]; then
        $cmd "node:7" node "/w/$path"
    elif [ "$ext" = "erl" ]; then
        $cmd "erlang:19" sh -c "cd /w/$dir && erlc $base && erl +B -s $base_wext main -s init stop"
    elif [ "$ext" = "py" ]; then
        $cmd "py3" python "/w/$path"
    elif [ "$ext" = "c" ]; then
        $cmd "rsmmr/clang" sh -c "clang /w/$path -o /a.out && /a.out"
    elif [ "$ext" = "cpp" ]; then
        $cmd "rsmmr/clang" sh -c "clang++ -std=c++11 /w/$path -o /a.out && /a.out"
    elif [ "$ext" = "scm" ]; then
        $cmd "algo" gosh "/w/$path"
    elif [ "$ext" = "java" ]; then
        $cmd "java:8" sh -c "cd /w/$dir && javac $base && java $base_wext"
    elif [ "$ext" = "ts" ]; then
        $cmd "ts" sh -c "tsc /w/$path --outFile /tmp/a.js && node /tmp/a.js"
    else
        echo "No supported language"
        return 1
    fi
}

### REPL

erl () { dr erlang:19 erl $@;}
iex () {
    if [ $# -eq 0 ]; then
        dr iex:dev iex -S mix
    elif [ $# -eq 1 -a -f $1 ]; then
        dr iex:dev mix run $1
    else
        dr iex:dev iex "$@"
    fi
}

run_c () { local f=`mktemp`; clang -xc $1 -o $f; $f; } 
run_cpp () { local f=`mktemp`; clang++ -std=c++14 $1 -o $f; $f; }
run_java () { javac $1; java `f_without_ext $1`; }

mix () {
    if [ $1 = "up" ]; then
        docker rm -f IEX_DEV_UPDATED
        docker run -dit --name IEX_DEV_UPDATED -w /app -v `pwd`:/v -d iex:dev /bin/bash
        docker exec IEX_DEV_UPDATED cp /v/$2 /app/
        docker exec IEX_DEV_UPDATED mix deps.get compile
        docker exec IEX_DEV_UPDATED mix compile
        docker commit IEX_DEV_UPDATED iex:dev
        docker rm -f IEX_DEV_UPDATED
    else
        dr iex:dev mix "$@"
    fi
}

make_100M() {
    mkfile 100m 100M_FILE
}

### HEROKU
# brew install heroku
heroku_install() { wget -O- https://toolbelt.heroku.com/install-ubuntu.sh | sh; }
heroku_login() { heroku auth:login --app $1; }
heroku_db() { heroku pg:psql --app $1; }  # use redis-cli instead
heroku_env() { heroku run env --app $1; }
heroku_run() { local app=$1; shift; heroku run --app $app $@; }  # python -m MODULE cmd
heroku_log() { heroku logs --app $1 --tail; }
heroku_config() { heroku config --app $1; }
heroku_info() { heroku apps:info --app $1; }
# heroku config:set KEY=$VAL -a $APP

python_upload()  { python setup.py sdist bdist bdist_egg upload; }
python_upload3() { python3 setup.py sdist bdist bdist_egg upload; }
# python -c 'import bottle as b; b.route("<p:path>")(lambda p: b.jinja2_template(p[1:], **dict(b.request.params.items()))); b.run(host="0.0.0.0", debug=True, port=8000)'

kill_port () { local port=$1; lsof -t -i tcp:$port | xargs kill -9; }

go_compile () {
    # or: go build -gcflags -S $1
    OOS=linux GOARCH=amd64 go tool compile -S $1 2>&1
}
go_linux () { GOOS=linux GOARCH=amd64 go build $1; }

vs () { local a=${1:-.}; VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args $a; }
as () { local a=${1:-.}; open -a /Applications/Android\ Studio.app $a; }

adb_web() { adb shell am start -a android.intent.action.VIEW -d $1; }
adb_log() { adb logcat ; }
date_GMT() { TZ=GMT date; }

ssl_expired() { openssl s_client -connect $1:443 < /dev/null | openssl x509 -text |grep Not; }
ssl_crt() { openssl req -nodes -newkey rsa:2048 -keyout myserver.key -out server.csr; }
ssh_add_key() { eval `ssh-agent` && ssh-add $1; }

ip_global() {
    curl http://wtfismyip.com/text
}

ip_info() {
    curl ipinfo.io/$1
}

aws_credentials() { cat ~/.aws/credentials | perl -nlE 'tr/a-z/A-Z/; s/ //g; say if /^AWS/'; }

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

# VS CODE
vs_settings() { vim "$VSCODE_SETTINGS"; }
# vs_init() { ln -s "$VSCODE_SETTINGS"; }

# url_escape() {}
# _fork_bomb :(){ :|:& };:

### ALIAS

alias s='rlwrap sh -l'
alias dc="docker-compose"
alias g="git"
alias ll='ls -alF'
alias ls='ls -aCF'
alias sl=ls
alias l=ls
alias sudo="sudo "  # sudo時にアリアス有効
alias cd="cdls"
alias dei="docker exec -i"
alias curl_json='curl -H "Accept: application/json" -H "Content-type: application/json"'  # -d オプションには、-d '{"key": "value", "number": int}'とkeyは"で囲むこと
alias T=tmux_set_buffer
alias b="tmux_show_buffer"
alias f="peco_select_recent_files"
alias w="peco_grep_word"
alias d="peco_select_dir"
alias cd="peco_select_dir"
alias e="emacsclient"

### BINDS

bind -x '"\eb": peco_git_branch'
bind -x '"\es": peco_select_find'
# bind -x '"\ea": peco_select_docker_shell'
bind -x '"\ew": peco_select_docker_shell'
bind -x '"\ef": peco_select_recent_files'
bind -x '"\ed": peco_select_dir'
bind -x '"\eD": "cdls .."'
bind -x '"\C-r": peco_select_history'
bind    '"\C-xr": reverse-search-history'
bind -x '"\eo": open_recent_file'
bind -x '"\eB": tmux capture-pane'

echo "DONE"
# read i1 i2 <<< 'foo bar'; echo -E "status=$? i1=[$i1] i2=[$i2]"
