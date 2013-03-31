#空白から始めたコマンドを無視
#export HISTCONTROL=ignorespace
#重複履歴を無視
#export HISTCONTROL=ignoredups 
#ignorespace+ignoredups = ignoreboth
export HISTCONTROL=ignoreboth

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

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias pd='pushd'
alias shutdown='shutdown -h now'
alias ls='ls -F'
alias ds='dirs -v'
#alias reboot="reboot"
alias x="xargs"
alias j="jobs"
alias i="ipython3"
alias h="histroy"
alias cd=cdls
alias e='echo $?'
function cdls() {
    # cdがaliasでループするので\をつける
    #aliasを一時的に無効にする
    \cd $1;
    ls ;
}

#python config
if [ -f /usr/local/bin/virtualenvwrapper.sh ];then
    source /usr/local/bin/virtualenvwrapper.sh
fi
#for ipython cache
export PIP_DOWNLOAD_CACHE=~/.pip_cache
export PYTHONSTARTUP="$HOME/.python_startup"
export PYTHONSTARTUP=~/.pystartup

#環境変数
export PATH="$PATH":"~/dropbox/conf/lib":.
#cdコマンドに対する候補を「:」で区切って複数記述する
#export CDPATH="~/dropbox/work:~/dropbox"


#for ubuntu
if [ `whoami` = "ubuntu1" ]; then
   xmodmap ~/dropbox/conf/ubuntu/xmodmap
fi
export PATH=~/dropbox/lib:$PATH
export PATH=~/dropbox/public/lib:$PATH
export PATH=~/python/3.3.0/bin:$PATH
export EDITOR=vi
export PATH=~/python/2.7.3/bin:$PATH
export PATH=$PATH:/Applications/UpTeX.app/teTeX/bin
export CCL_DEFAULT_DIRECTORY=/Users/air/ccl

#auto-run
start.py
export PYTHONPATH=~/s/public/lib
