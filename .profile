# For Bash

### TODO
# - L, T (pbcopy)

echo hi start

### EXPORT

# \h hostname
# \t \T 24 12 time
# \w \W currend dir
# \u user name
export PS1="\u@\w\n$ "
export GOPATH=~/go 
export PATH="$PATH:$GOPATH/bin"
export INPUTRC=~/.inputrc

# CUSTOM VARS
export MYDIRS_HISTORY=~/.mydirs

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
    echo $p >> $MYDIRS_HISTORY
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

export DOCKER_TLS_VERIFY="1"
export DOCKER_CERT_PATH="/Users/mbp/.docker/machine/machines/default"
export DOCKER_HOST="tcp://192.168.99.100:2376"


### FOR BASH

# export HISTCONTROL=ignorespace  # 空白から始めたコマンドを無視
# export HISTCONTROL=ignoredups  # 重複履歴を無視
export HISTCONTROL=ignoreboth  # 両方

#履歴の共有
function share_history {  # 以下の内容を関数として定義
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}

PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
shopt -u histappend   # .bash_history追記モードは不要なのでOFFに

#よく使うコマンドは履歴保存対象から外す。
export HISTIGNORE="fg*:bg*:history:cd*:ls*"

#ヒストリのサイズを増やす
export HISTSIZE=100000

peco_select_history() {
    declare l=$(HISTTIMEFORMAT= history | sort -k1,1nr | perl -ne 'BEGIN { my @lines = (); } s/^\s*\d+\s*//; $in=$_; if (!(grep {$in eq $_} @lines)) { push(@lines, $in); print $in; }' | peco --query "$READLINE_LINE")
    READLINE_LINE="$l"
    READLINE_POINT=${#l}
    if [ `uname` = "Darwin" ]; then
        ${READLINE_LINE}
    fi
}
bind -x '"\C-r": peco_select_history'
bind    '"\C-xr": reverse-search-history'
bind -x '"\ef": mydirs_dirs'

emacs_start () { docker run -e "TERM=xterm-256color" -v /Users/mbp:/Users/mbp --name emacs --rm -it emacs sh -c "emacs --daemon && bash -l"; }
e () {
    if [ $# -eq 0 ]; then
        docker exec -it emacs script -q -c '"/bin/bash" -c "emacsclient -t ."' /dev/null
    else
        local p="`pwd`/$1";
        if [ -e $p ]; then
            docker exec -it emacs script -q -c "'/bin/bash' -c 'emacsclient -t $p'" /dev/null
        else
            echo "$p does not exist"
        fi
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
