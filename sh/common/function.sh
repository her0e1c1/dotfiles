
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

function exists { which $1 &> /dev/null }

__C_INCLUDED_HEADERS="
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/acl.h>
#include <sys/param.h>
#include <sys/mount.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <paths.h>
#include <err.h>
"

_complie_and_run_in_c(){
    local main source tfile
    main=$1; shift;
    source="
$__C_INCLUDED_HEADERS
extern char **environ;
int main(int argc, char* argv[]){
    $main
    return 0;
}"
    tfile=`mktemp`
    echo $source | clang -x c - -o $tfile && $tfile "$@"
    [ -f $tfile ] && \rm $tfile
}
alias ce=_complie_and_run_in_c

_run_in_haskell(){
    local main source tfile
    main=$1; shift
    source="
import Data.List
import Data.Array
import System.Directory
import System.Environment
main = $main
"
   # 以下の場合は、getArgsが動作しない
   # echo $source | runhaskell
    
   tfile=`mktemp`
   echo $source >> $tfile
   runhaskell $tfile "$@"
   \rm $tfile
}
alias he=_run_in_haskell

_complie_and_run_in_cpp(){
    local main source tfile
    main=$1; shift;
    [ -z "$main" ] && main='""'
    source="
$__C_INCLUDED_HEADERS

#include <iostream>
#define P(x) cout << (x) << endl;

using namespace std;
int main(int argc, char* argv[]){
    // cout << $main << endl;
    $main;
    return 0;
}"
    tfile=`mktemp`
    echo $source | clang++ -std=c++11 -x c++ - -o $tfile && $tfile "$@"
    [ -f $tfile ] && \rm $tfile
}
alias cpe=_complie_and_run_in_cpp


__emacs_oneliner(){
    local arr Fflag nflag pre STDIN;
    Fflag="\n"
    nflag=false
    dflag=false  # debug
    pre=""
    STDIN=""
    while getopts ndF: OPT; do
	    case $OPT in
            d) dflag=true
               ;;
	        n) nflag=true
	           ;;
            F) Fflag=$OPTIN
               ;;
	    esac
    done
    shift $((OPTIND - 1))

    if $nflag; then
        # ここでまとめて入力を受け取るので、一つ前のコマンドが完了する必要あり
        STDIN=`cat -`;
        pre='(setq STDIN "'$STDIN'")';
        $dflag && echo "STDIN=$STDIN"
    fi

    if $nflag; then
        args=`echo '(--map ' "$@" '(s-split "' "$Flag"' " STDIN))'`
    else
        args="$@"
    fi
    $dflag && echo $args
    emacsclient -e "(progn $pre $args)"

    # close
    [ -n "$STDIN" ] && emacsclient -e '(setq STDIN "")';
    return 0;
}
