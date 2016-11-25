# set -x

# go get github.com/motemen/ghq
# go get github.com/motemen/github-list-starred

# For Bash

### TODO
# - L, T (pbcopy)

echo LOADING...

### EXPORT

# \h hostname
# \t \T 24 12 time
# \w \W currend dir
# \u user name
export PATH="/Applications/Docker.app/Contents/Resources/bin/:$PATH"
export PS1="\u@\w\n$ "
export GOPATH=~/go
export GOBIN=~/go/bin
export PATH="$PATH:$GOPATH/bin:$GOBIN"
export INPUTRC=~/.inputrc
export HOSTIP=192.168.100.100

# CUSTOM VARS
export MYDIRS_HISTORY=~/.mydirs
export MYCMDS_HISTORY=~/.mycmds

do_sudo () {
    sudo ifconfig lo0 alias $HOSTIP
}

### INSTALL IF NEEDED

init_install () {
    if which go 2>&1 1>/dev/null; then
       go get github.com/rogpeppe/godef
       go get -u github.com/nsf/gocode
       go get github.com/golang/lint/golint
       go get github.com/kisielk/errcheck
    fi
}

#install_dotfile () {
#  local filepath
#  for name in .vimrc .tmux.conf do
#    filepath="~/$name"
#    if [ ! -f $filepath ]; then
#      curl https://raw.githubusercontent.com/her0e1c1/home/master/$name -o $filepath
#    fi
#  done 
#}

# install_dotfile

# [ ! -d "$GOPATH" ] && mkdir $GOPATH

## REGEX
f_basename() { echo ${1##*/}; }    # 左からマッチしたものを除外(greedy)
f_ext() { echo ${1#*.}; }          # 左からマッチしたものを除外(non greedy)
f_without_ext() { echo ${1%%.*}; } # 右からマッチしたものを除外(greedy)
f_dirname() { echo ${1%/*}; }      # 右からマッチしたものを除外(non greedy)

### FUNCTIONS

f () {
    if [ ! -f $MYDIRS_HISTORY ]; then
        touch $MYDIRS_HISTORY
    fi
    if [ $# -eq 0 ]; then
        local d=`cat $MYDIRS_HISTORY | peco`
        if [ -d $d ]; then
            cdls $d
            update_files $MYDIRS_HISTORY $d
        fi
    else
        local d=`python -c "import os; print(os.path.abspath('$1'))"`
        update_files $MYDIRS_HISTORY $d
        cd $d
    fi

}
bind -x '"\ef": f'

### BINDS

# stty -a
# bind -p
bind -f $INPUTRC

### ALIAS

alias s='rlwrap sh -l'
alias d="docker"
alias dc="docker-compose"
alias g="git"
alias ll='ls -alF'
alias ls='ls -aCF'
alias sl=ls
alias l=ls
alias sudo="sudo "  # sudo時にアリアス有効

export LANG=ja_JP.UTF-8
export PAGER=less
export CLICOLOR=1  # lsに色づけ
export LSCOLORS=DxGxcxdxCxegedabagacad
export TERM=xterm-256color  # for zenburn-emacs

if [ -n "$(which vim)" ]; then
    export EDITOR=vim
else
    export EDITOR=vi
fi

# export DOCKER_TLS_VERIFY="1"
# export DOCKER_CERT_PATH="/Users/mbp/.docker/machine/machines/default"
# export DOCKER_HOST="tcp://192.168.99.100:2376"


### FOR BASH

# export HISTCONTROL=ignorespace  # 空白から始めたコマンドを無視
# export HISTCONTROL=ignoredups  # 重複履歴を無視
# export HISTCONTROL=ignoreboth  # 両方

#履歴の共有
# function share_history {  # 以下の内容を関数として定義
#     history -a  # .bash_historyに前回コマンドを1行追記
#     history -c  # 端末ローカルの履歴を一旦消去
#     history -r  # .bash_historyから履歴を読み込み直す
# }

# PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
# shopt -u histappend   # .bash_history追記モードは不要なのでOFFに

# #よく使うコマンドは履歴保存対象から外す。
# export HISTIGNORE="fg*:bg*:history:cd*:ls*"

# #ヒストリのサイズを増やす
# export HISTSIZE=100000

peco_select_history() {
    declare l=$(HISTTIMEFORMAT= history | sort -k1,1nr | perl -ne 'BEGIN { my @lines = (); } s/^\s*\d+\s*//; $in=$_; if (!(grep {$in eq $_} @lines)) { push(@lines, $in); print $in; }' | peco --query "$READLINE_LINE")
    READLINE_LINE="$l"
    READLINE_POINT=${#l}
    if [ `uname` = "Darwin" ]; then
        echo "${READLINE_LINE}" | chomp | pbcopy
    fi
}
bind -x '"\C-r": peco_select_history'
bind    '"\C-xr": reverse-search-history'

export RECENT_FILES=~/.recent_files

emacs () {
    if docker ps -a --format "{{.Names}}" | grep emacs; then
        docker rm -f emacs
    fi
    touch ~/.recentf
    docker run -e "GOPATH=/go:/share/go" -e "TERM=xterm-256color" -v ~/workspace/emacs.d/.emacs.d/lisp:/root/.emacs.d/lisp -v ~/go:/share/go -v ~/.recentf:/root/.emacs.d/recentf -v /Users/mbp:/Users/mbp --name emacs -d -it emacs sh -c "emacs --daemon && bash -l"
}
e () {
    if [ $# -eq 0 ]; then
        local p=`pwd`
    else
        local p=`perl -E "use Cwd 'abs_path'; say abs_path('$1')"`
    fi
    if [ -e $p ]; then
        update_files $RECENT_FILES $p
        docker exec -it emacs script -q -c "'/bin/bash' -c 'emacsclient -t $p'" /dev/null
    else
        echo "$p does not exist"
    fi
}

cdls(){
	if [ ${#1} -eq 0 ]; then
	   \cd && ls
	else
       \cd "$*" && ls -G
	fi
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

alias cd="cdls"

docker_kill()        { docker rm -f `docker ps -aq`; docker network rm `docker network ls -q`; }
docker_rename()      { docker tag $1 $2; docker rmi $1; }
docker_remove_images() { docker rmi `docker images | perl -anlE 'say "$F[2]" if $F[0] =~ /<none>/'`; }
docker_compose_all() { docker-compose `perl -E 'say map {" -f \$_"} reverse <docker-compose*.yml>'` $@; }

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
            docker exec -it --detach-keys ctrl-q,q $name /bin/bash
        fi
    fi
}
alias dei="docker exec -i"

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
        sh -c "docker run --rm -p 9999 --add-host=docker:$HOSTIP $env -v /Users/mbp:/Users/mbp -w `pwd` --detach-keys ctrl-q,q -it $name $cmd"
    fi
}

docker_mysql() {
    if docker ps --format "{{.Names}}" | perl -E 'exit !(grep {$_=~ /^mysql$/} <STDIN>);'; then
       docker rm -f mysql 
    fi
    docker run --name mysql -v /Users/mbp:/Users/mbp -w `pwd` --rm -it -e MYSQL_ALLOW_EMPTY_PASSWORD=1 -e MYSQL_DATABASE=db mysql:5.7;
}

docker_es() {
    if docker ps --format "{{.Names}}" | perl -E 'exit !(grep {$_=~ /^es$/} <STDIN>);'; then
       docker rm -f es 
    fi
    docker run --name es -v /Users/mbp:/Users/mbp -w `pwd` --rm -it elasticsearch:2.3;
}

# TODO: grep |peco | open file

# alias h='cat ~/GDrive/.zsh_history | peco | perl -pE "chomp \$_" | pbcopy'
gr () { find . -type f -exec grep -nH -e $1 {} \;; }
tmux_set_buffer() { perl -E '@a=<stdin>; `tmux set-buffer "@a"`'; }
tmux_show_buffers() { perl -E 'say `tmux show-buffer -b $_ ` for 0..20'; }
alias T=tmux_set_buffer
# alias h='cat ~/GDrive/.zsh_history |peco | perl -pE "chomp \$_" | tmux_set_buffer'

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
bind -x '"\eo": open_recent_file'

# -d オプションには、-d '{"key": "value", "number": int}'とkeyは"で囲むこと
alias curl_json='curl -H "Accept: application/json" -H "Content-type: application/json"'
# git_add_stream git@...
alias git_add_stream="git remote add upstream"
alias git_update="git checkout master; git fetch upstream; git merge upstream/master; git push"

git_branch_remove () {
    if [ $# -eq 0 ]; then
        git branch   
        return
    fi
    local pattern=$1; shift
    git branch | grep $pattern | xargs git branch -D
}

esc () { perl -plE "s#'#'\\''# "; }

alias urlencode='python -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias timestamp='python -c "import sys, datetime as d; print(d.datetime.fromtimestamp(float(sys.argv[1])))"'

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
bind -x '"\eh": h'

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
            local d=~/$sync
            if [ ! -d $d ]; then
                mkdir -p `dirname $d`
                echo "create a symbol link $d"
                ln -s "$sync" $d
            fi
            echo "start sync ..."
            watchmedo shell-command -R "$sync" -c "docker exec $name rsync -avz --exclude '*.git*' $sync/ $trim" $@
        else
            echo "You can't sync on `pwd`. Go to $sync on host"
        fi
    else
        echo "$sync dir is not found on docker."
    fi
}

docker_working() {
    docker inspect $1 | python -c 'import sys, json; print(json.loads(sys.stdin.read())[0]["Mounts"][0]["Source"])'
}

chomp () { perl -pE "chomp \$_"; }

b () {
    local p=`tmux show-buffer`
    echo "$p" | peco | chomp | pbcopy
}

bind -x '"\eb": b'
bind -x '"\eB": tmux capture-pane'

docker_remove_volume () { docker volume rm `docker volume ls -q`; }

lang_run() {
    local name="$1"; shift
    local container_name="lang_$1"
    if ! docker ps --format "{{.Names}}" | grep $container_name; then
        docker run --name "$container_name" -it --rm -v `pwd`:/working "$name:dev"
    fi
    if [ $# -eq 0 ]; then
        docker exec -it "$container_name" bash
    else
        docker exec -it "$container_name" $@
    fi
}

go_run () { lang_run "golang" $@; }

dict () {
    local cmd=$1;
    if ! docker ps --format "{{.Names}}" | grep dict > /dev/null ; then
        docker-compose -f /Users/mbp/workspace/sandbox/dict/docker-compose.yml up -d
    fi
    if [ "$cmd" = "setup" ]; then
        docker exec -it dict python main.py "$@"
    else
        docker exec -it dict python main.py s "$@"
    fi
}

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

docker_working() {
    docker inspect $1 | python -c 'import sys, json; print(json.loads(sys.stdin.read())[0]["Mounts"][0]["Source"])'
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
    else
        echo "No supported language"
        return 1
    fi
}

math() {
    # math IN > OUT or -o OUT
    pandoc --self-contained -s --mathjax=https://gist.githubusercontent.com/yohm/0c8ed72b6f18948a2fd3/raw/624defc8ffebb0934ab459854b7b3efc563f6efb/dynoload.js -c https://gist.githubusercontent.com/griffin-stewie/9755783/raw/13cf5c04803102d90d2457a39c3a849a2d2cc04b/github.css $@
}

docker_process_live () {
    docker ps --format "{{.Names}}" | perl -E "exit !(grep {\$_=~ /^$1\$/} <STDIN>);"
}

ej() {
    if ! docker_process_live ej; then
        docker-compose -f ~/workspace/sandbox/lang/erl/ej/docker-compose.yml up -d;
    fi
    de ej;
}
ghci() { dr haskell:dev ghci; }
ip2() { dr py2 ipython; }
ip3() { dr py3 ipython; }
ipm() { dr math ipython; }
ma () { dr math; }
gore () { dr golang:dev gore; }
spy () { dr py2 scrapy shell $1;}
erl () { dr erlang:19 erl $@;}
iex () {
    if [ $# -eq 0 ]; then
        dr elixir iex
    elif [ $# -eq 1 -a -f $1 ]; then
        dr elixir elixir $1
    else
        dr elixir iex "$@"
    fi
}

mix () {
    dr elixir mix "$@"
}

echo "DONE"
