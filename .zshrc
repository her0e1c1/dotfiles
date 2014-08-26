typeset -U path PATH

#--------------------------------------------------
#prompt
#--------------------------------------------------
#LEFT PROMPT
PS1="[${USER}]%(!.#.$) "
##右プロンプト
# 右側に時間を表示する
#RPROMPT="%T"
RPROMPT='%B%F{yellow}[%f%b %B%F{yellow}%~]%f%b'

#ディレクトリの区切りである/も含める
#C-wでディレクトリの一部のみ削除
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# 履歴
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

#dirsの履歴
DIRSTACKSIZE=100

# カレントディレクトリ中にサブディレクトリが無い場合に cd が検索するディレクトリのリスト
cdpath=($HOME)

#--------------------------------------------------
#autoload
#--------------------------------------------------
#標準関数で使用するものは, ロードする

# history 操作まわり
autoload history-search-end
autoload smart-insert-last-word
autoload -U colors

colors

#cd ~/d/ => ~/desctopのように補完
#c C-p/C-nでcから始まるコマンドのみの履歴を表示
autoload -U compinit
compinit -u

typeset -ga chpwd_functions
autoload -U chpwd_recent_dirs cdr
chpwd_functions+=chpwd_recent_dirs

## ディレクトリが変わったらディレクトリスタックを表示。
chpwd_functions=($chpwd_functions dirs)

zstyle ":chpwd:*" recent-dirs-max 50
#zstyle ":chpwd:*" recent-dirs-default true
zstyle ":completion:*" recent-dirs-insert always


#--------------------------------------------------
#setopt
#--------------------------------------------------

#右側まで入力がきたら時間を消す
setopt transient_rprompt          
setopt prompt_subst               # 便利なプロント

#cdしたときにも、dirsのスタックに積む(puと区別するので使用しない)
#setopt auto_pushd

# 同ディレクトリを履歴に追加しない
setopt pushd_ignore_dups

#ディレクトリの名前だけでcdできる
setopt auto_cd

# ファイルがある場合のリダイレクト(>)の防止
# したい場合は>!を使う
setopt noclobber

#色を使う
setopt prompt_subst

#rm で*をつけた場合の確認
setopt rm_star_wait

# リストを詰めて表示
setopt list_packed

# 補完一覧ファイル種別表示(@ /など)
setopt list_types                 

setopt nolistbeep

#タイプミスを訂正してくれる(おせっかいなので使用しない)
#setopt correct

#括弧の対応等を自動的に補完(うまく動かない?)
setopt auto_param_keys

#ディレクトリ名の補完で末尾に/を自動的に付加する
setopt auto_param_slash

#show error code
setopt print_exit_value

#C-dでログアウトしない
setopt ignore_eof

#pushd,popdの度にディレクトリスタックの中身を表示しない
setopt pushd_silent

#beep音を鳴らさない
setopt nobeep

#cd -[tab]とcd +[tab]の役割を逆にする
#表示する順番がreverseされるだけみたい？ 
#setopt pushd_minus

#入力の予測(一文字入力するたびに補完するので鬱陶しい)
autoload predict-on
#predict-on

# 直前の履歴を重複させない
setopt hist_ignore_dups
setopt hist_ignore_space
# 履歴を重複させない
setopt hist_ignore_all_dups
setopt hist_reduce_blanks         # スペース排除

#複数のzshのプロセスで履歴を共有
setopt share_history
# zshの開始終了を記録
setopt EXTENDED_HISTORY
setopt hist_no_store

# バックグラウンドジョブの状態変化を即時報告する
setopt notify

# =以降も補完する(--prefix=/usrなど)
setopt magic_equal_subst 

setopt extended_glob # グロブ機能を拡張する
unsetopt caseglob    # ファイルグロブで大文字小文字を区別しない

setopt long_list_jobs
setopt rc_quotes
#--------------------------------------------------
#key binds
#--------------------------------------------------

# emacsライクなキーバインド
bindkey -e
bindkey '^]' insert-last-word
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
bindkey "^[/" undo
bindkey "^[?" redo
bindkey "^[H" run-help

#--------------------------------------------------
#function
#--------------------------------------------------

#cdの後にls実行
cdls(){
	if [ ${#1} -eq 0 ]; then
	   cd && ls
	else
       \cd "$*" && ls -G
	fi
}

#emacsのデーモン再起動
function restart_emacs(){
    emacsclient -e "(kill-emacs)";
    emacs --daemon
}

function kill_emacs(){
	emacsclient -e "(kill-emacs)";
}

#圧縮ファイルを名前だけで展開
function extract() {
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

zshaddhistory(){
    local line=${1%%$'\n'}
    local cmd=${line%% *}

    [[ ${#line} -ge 4
       && ${cmd} != (l[sal])
       && ${cmd} != (c|cd)
       && ${cmd} != (m|man)
    ]]
}

#--------------------------------------------------
#zstyle
#--------------------------------------------------
#参照 http://zsh.sourceforge.net/Doc/Release/Completion-System.html
#zstyle ":HOOK-MODE: :COMMAND:" STYLE STRINGS

#zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -e -o pid,user,tty,cmd

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

#cdをしたときに現在のディレクトリを補完に含めない
zstyle ':completion' ignore-parents parent pwd ..

#補完候補を ←↓↑→ で選択 (補完候補が色分け表示される)
zstyle ':completion:*:default' menu select=2

# カレントディレクトリに候補がない場合のみ cdpath 上のディレクトリを候補
#zstyle ':completion:*:cd:*' tag-order local-directories path-directories
zstyle ':completion:*:cd:*' ignore-parents parent pwd

#補完時に大文字と小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'


zstyle ':completion:*' list-colors ''

#コマンドsudoを実行するときだけ, 補完するときのコマンドパス
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
			     /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

zstyle :insert-last-word match \
  '*([^[:space:]][[:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'

#Tab一回押すだけで補完(一回目だと無駄なものが補完されやすいので使用しない)
#zstyle ':completion:*:default' menu select true

#--------------------------------------------------
#zle
#--------------------------------------------------

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
zle -N insert-last-word smart-insert-last-word

#--------------------------------------------------
#exprot
#--------------------------------------------------

export LSCOLORS=exfxcxdxbxegedabagacad
export LANG=ja_JP.UTF-8
export EDITOR=vi
export PAGER=less
export DROPBOX_PATH=~/Dropbox
export MINEDB=~/Dropbox/mine.db
export SPHINX_PATH=~/github/sphinx_document:~/Dropbox/private/sphinx
export PYTHONSTARTUP=~/.pythonrc

sphinx_auto_build(){
    OLD_PATH=`pwd`;
    for p in ${(s/:/)SPHINX_PATH};do
        if [ -d $p ];then
            \cd $p;
            echo `pwd`
            if which inotifywait ;then
                while inotifywait -e modify ./**/*.rst;do make html; done &
            fi
       else
            echo "$pは存在しません";
        fi
    done
    \cd $OLD_PATH;
}

### Perl for one liner
PERL_MODULES=(
    '"File::Spec::Functions qw(:ALL)"'
    '"File::Basename"'
    '"List::Util qw(first max maxstr min minstr reduce shuffle sum)"'
    '"MIME::Base64"'
    '"Digest::MD5  qw(md5 md5_hex md5_base64)"'
    '"File::Copy qw(cp mv)"'
    '"POSIX"'
)
PERL_OPTION=`perl -e 'print sprintf " %s ", join " ", map {"-M$_"} @ARGV' $PERL_MODULES`

#--------------------------------------------------
#alias
#--------------------------------------------------

if uname -a | grep -q 'Ubuntu'
then
 alias ls="ls --color"

elif uname -a |grep -q "air"
then
 alias ls="ls -G"
 alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
 alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
else
 alias ls="ls -G"
fi

#alias -g
alias -g L="| less -SR"
alias -g G='| grep'
alias -g X='| xargs'
alias -g N1='1>/dev/null'
alias -g N2='2>/dev/null'
alias -g N21='2>&1'
alias -g PP="| perl $PERL_OPTION -aplE"
alias -g P0="| perl $PERL_OPTION -an0lE"
alias -g P="| perl $PERL_OPTION -anlE"
alias ..=".."

#標準出力をクリップボードにコピー
if which pbcopy >/dev/null 2>&1 ; then 
    # Mac  
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then 
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then 
    # Cygwin 
    alias -g C='| putclip'
fi

#alias
alias h=history
alias cd=cdls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls'
alias pd='pushd'
alias po='popd'
alias ds='dirs -v'
alias j="jobs"
alias i="ipython3"
#alias r='echo $?'
#alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias sl='ls -CF'
alias gd='dirs -v; echo -n "select number: "; read newdir; pushd +"$newdir"'
alias ea='emacsclient -nw -a ""'
alias en='emacsclient -n'
alias e='emacsclient -t'
alias ee='emacsclient -e'
alias d='emacsclient -t .'
alias r='ruby -e'
alias p="perl $PERL_OPTION -E"
alias pn="perl $PERL_OPTION -nalE"
alias pp="perl $PERL_OPTION -palE"
alias pp='perl -plE'
alias pn='perl -nlE'
alias c="cdr"
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract
alias sudo="sudo "  # sudo時にアリアス有効
alias v="view"
export MYSQL_USER="mytest"
export MYSQL_DATABASE="mytest"
alias m=mysql_execute
mysql_execute (){
    mysql -u $MYSQL_USER $MYSQL_DATABASE -e "$1";
}
alias my=mysql_execute

function repeat (){
    # do arg2 command `arg1` times
    if [ $# = 2 ]; then
        for i in {1..$1}; do $2;done;
    fi
}

#--------------------------------------------------
#compctl
#--------------------------------------------------

#compctl 指示 コマンド名(のリスト)
#ディレクトリ名のみ補完
compctl -/ cd chdir dirs pushd

#補完候補を.bz2にする
compctl -g '*.(bz2)' bunzip2

#--------------------------------------------------
#load setting files
#--------------------------------------------------

[ -f ~/.zshrc.include ] && source ~/.zshrc.include # 設定ファイルのinclude
[ -f ~/.sh.d/export ] && source ~/.sh.d/export
[ -f ~/.sh.d/alias ] && source ~/.sh.d/alias
[ -f ~/.shell_local ] && source ~/.shell_local # 各マシンローカルごとの設定ファイル


#--------------------------------------------------
#each settings
#--------------------------------------------------

if uname -a | grep -q "FreeBSD"
then
    setxkbmap -rules xorg -model jp106 -layout jp
fi

alias racket="/Applications/Racket\ v5.3.6/bin/racket"
export PATH=$PATH:~/gae/google_appengine

eval `ssh-agent` > /dev/null && ssh-add ~/.ssh/git_id_rsa 2> /dev/null

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi

# for zenburn-emacs
export TERM=xterm-256color


### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

### python password manager
export PW_URL="sqlite:///~/.mine.db"

alias prm='perl -E ''
use File::Basename;
use File::Spec;
$d="$ENV{HOME}/.trash";
mkdir $d unless -d $d; $c=1;
for(@ARGV){while(1){
  unless(-e){say "no $_ exists"; last};
  $base = basename $_;
  $base .= "_$c" if($c !=1);
  $o = File::Spec->catfile($d, $base);
  $cmd = "mv $_ $o";
  unless(-e $o){say $cmd; system $cmd; last};$c++;
}}
'''
alias rm="prm"

### Python Code
alias urlencode='python -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'

PYALIAS_PYTHON='python3 -c'
PYALIAS_IMPORT='
from os.path import isdir, exists
from sys import exit
from shutil import copy, rmtree
from argparse import ArgumentParser
'

export PATH="$PATH:$HOME/.cask/bin"
alias pyrm='python3 -c ''
from os.path import *
from shutil import *
import os
import argparse
p = argparse.ArgumentParser()
p.add_argument("-r")
p.add_argument("paths", nargs="+")
args = p.parse_args()
trash = join(os.environ["HOME"], ".trash")
for p in args.paths:
    counter = 0
    while True:
        if not exists(p):
            print("Not exists: {}".format(p))
            break
        b = basename(p)
        if counter > 0:
            b += "_{}".format(counter)
        dst = join(trash, b)
        if not exists(dst):
            print("move {p} {dst}".format(**locals()))
            move(p, dst)
            break
        counter += 1
'''

alias pytrash='python3 -c ''
from os.path import *
from shutil import *
import os
import argparse
p = argparse.ArgumentParser()
p.add_argument("-r", action="store_true")
args = p.parse_args()
trash = join(os.environ["HOME"], ".trash")
if not isdir(trash):
    os.makedirs(trash)
if args.r:
    rmtree(trash)
else:
    for t in os.listdir(trash):
        print(t)
'''

alias pyswap="$PYALIAS_PYTHON \"
$PYALIAS_IMPORT
p = ArgumentParser()
p.add_argument('paths', nargs=2)
args = p.parse_args()
for p in args.paths:
    if not exists(p):
        print('No {} exists'.format(p))
        exit()
\""


function exists { which $1 &> /dev/null }

if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(history -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi

function insert_quote {
    local rbuff
    rbuff="$RBUFFER"
    BUFFER=$(echo "$LBUFFER''")
    CURSOR=$(($#BUFFER -1))
    BUFFER=$(echo "${BUFFER}$rbuff")
    zle -R -c
}
zle -N insert_quote
bindkey "'" insert_quote


function echo_variable {
    local s
    s="$1"
    # echo ${{s}}
    echo ${path}
}
alias o=echo_variable

## 実行したプロセスの消費時間が3秒以上かかったら
## 自動的に消費時間の統計情報を表示する。
REPORTTIME=3

function cl (){
	clang++ -I. $1 && ./a.out
}
function clc (){
	clang -I. $1 && ./a.out
}
