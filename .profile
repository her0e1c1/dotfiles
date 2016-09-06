# set -x
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
export PATH="$PATH:$GOPATH/bin"
export INPUTRC=~/.inputrc

# CUSTOM VARS
export MYDIRS_HISTORY=~/.mydirs
export MYCMDS_HISTORY=~/.mycmds

### INSTALL IF NEEDED

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

[ ! -d "$GOPATH" ] && mkdir $GOPATH

### FUNCTIONS

mydirs_dirs () {
    # 最近のものを先頭に
    local d=`cat $MYDIRS_HISTORY | peco`
    if [ -d $d ]; then
      cdls $d
    fi
}

mydirs_pushd () {
    local p=`python -c "import os; print(os.path.abspath('$1'))"`
    update_files $MYDIRS_HISTORY $p
    cd $p
}

myhistroy () {
    cat ~/ | peco
}

### BINDS

# stty -a
# bind -p
bind -f $INPUTRC

### ALIAS

alias s='rlwrap sh -l'
alias p='mydirs_pushd'
alias d="docker"
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
        echo "${READLINE_LINE}" | pbcopy
    fi
}
bind -x '"\C-r": peco_select_history'
bind    '"\C-xr": reverse-search-history'
bind -x '"\ef": mydirs_dirs'

export RECENT_FILES=~/.recent_files

emacs_start () { docker run -e "TERM=xterm-256color" -v /Users/mbp:/Users/mbp --name emacs -d -it emacs sh -c "emacs --daemon && bash -l"; }
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

# recent_files () {;
# }

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

docker_kill()        { docker rm -f `docker ps -aq`; }
docker_rename()      { docker tag $1 $2; docker rmi $1; }

docker_compose_all() { docker-compose `perl -E 'say map {" -f \$_"} reverse <docker-compose*.yml>'` $@; }

docker_exec() {
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
        if [ $# -ne 0 ]; then
            # $@はかなり特殊な変数(配列っぽい動きする。そのため他の変数に代入できないっぽい)
            docker exec -it --detach-keys ctrl-q,q $name $@
        else
            docker exec -it --detach-keys ctrl-q,q $name /bin/bash
        fi
    fi
}
alias de=docker_exec
alias dei="docker exec -i"

docker_run() { 
    if [ $# -eq 0 ]; then
        docker images
    else
        local name=$1; shift
        local cmd=/bin/bash
        [ $# -ne 0 ] && cmd=$@
        sh -c "docker run --rm -v /Users/mbp:/Users/mbp -w /Users/mbp --detach-keys ctrl-q,q -it $name $cmd"
    fi
}
alias dr=docker_run

# dei db mysql db < FILEPATH.sql
docker_temp_mysql() { docker run --name db --rm -it -e MYSQL_ALLOW_EMPTY_PASSWORD=1 -e MYSQL_DATABASE=db mysql:5.7; }

# TODO: grep |peco | open file

# alias h='cat ~/GDrive/.zsh_history |peco | perl -pE "chomp \$_" | pbcopy'
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

filepath = '$1'
word = '$2\n'

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
        echo -n "$d" | tmux_set_buffer
    else
        local d=`echo $@`
        update_files $MYCMDS_HISTORY "$d"
        $@
    fi
}
bind -x '"\eh": h'

echo "DONE"
