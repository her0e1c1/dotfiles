typeset -U path PATH

#--------------------------------------------------
#prompt
#--------------------------------------------------
#LEFT PROMPT
PS1="[${USER}@${HOST%%.*}]%(!.#.$) "
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
DIRSTACKSIZE=10000000

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

zstyle ":chpwd:*" recent-dirs-max 100000
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

# ファイルがある場合のリダイレクト(>)の防止。上書きは>!を使う
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

limit coredumpsize 0  # coreファイルを作らない

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

LIB_PERL=~/lib/perl
#--------------------------------------------------
#exprot
#--------------------------------------------------

### Perl for one liner
PERL_MODULES=(
    '"File::Spec::Functions qw(:ALL)"'
    '"Cwd qw(chdir abs_path cwd fastcwd fast_abs_path realpath)"'
    '"File::Basename"'
    '"List::Util qw(first max maxstr min minstr reduce shuffle sum)"'
    '"MIME::Base64"'
    '"Digest::MD5  qw(md5 md5_hex md5_base64)"'
    '"File::Copy qw(cp mv)"'
    '"POSIX"'
    '"My::Utils qw(:ALL)"'
)
PERL_OPTION=`perl -e 'print sprintf " %s ", join " ", map {"-M$_"} @ARGV' $PERL_MODULES`
PERL_OPTION="$PERL_OPTION -I $LIB_PERL"

ysql_execute (){
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

[ -f ~/sh/function.sh ] && source ~/sh/function.sh
[ -f ~/sh/alias.sh ] && source ~/sh/alias.sh
[ -f ~/sh/export.sh ] && source ~/sh/export.sh
[ -f ~/sh/stty.sh ] && source ~/sh/stty.sh

#--------------------------------------------------
#each settings
#--------------------------------------------------

# if uname -a | grep -q "FreeBSD"
# then
#     setxkbmap -rules xorg -model jp106 -layout jp
# fi
# alias racket="/Applications/Racket\ v5.3.6/bin/racket"
# export PATH=$PATH:~/gae/google_appengine

eval `ssh-agent` > /dev/null && ssh-add ~/.ssh/id_rsa_git 2> /dev/null

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi


if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(history -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    # bindkey '^[r' percol_select_history
    bindkey '^r' percol_select_history
fi

function percol_cd() {
    local p
    p=$(cdr -l | perl -nlE 'say glob +(split /\s+/)[1]'| percol)
    if [ -n $p ]; then
        \cd $p
        pwd && ls
    fi
}
zle -N percol_cd
bindkey "^[f" percol_cd

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
	clang++ -I. $1 -std=c++11 && ./a.out
}
function clc (){
	clang -I. $1 && ./a.out
}

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end

echo "finish loading .zshrc"
