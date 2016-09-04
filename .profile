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
    local d=`cat $MYDIRS_HISTORY | peco`
    if [ -d $d ]; then
      cd $d
    fi
}

mydirs_pushd () {
    echo $1 >> $MYDIRS_HISTORY
    cd $1
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
