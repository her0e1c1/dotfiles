alias sshX="ssh -c blowfish -2 -C -Y"
alias h=history
alias vm=VBoxManage
alias cd=cdls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls'
alias pd='pushd'
alias po='popd'
alias ds='dirs -v'
alias j="jobs"
alias i="ipython3"
alias g="git"
#alias r='echo $?'
#alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias sl='ls -CF'
alias gd='dirs -v; echo -n "select number: "; read newdir; pushd +"$newdir"'
alias ea='emacsclient -nw -a ""'
alias en='emacsclient -n'
alias e='emacsclient -t'
alias d='emacsclient -t .'
alias r='ruby -e'
alias p="perl $PERL_OPTION -E"
alias pn="perl $PERL_OPTION -nalE"
alias pp="perl $PERL_OPTION -palE"
alias pp='perl -plE'
alias pn='perl -nlE'
alias f='fab -f ~/fabfile'
alias c="cdr"
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract
alias sudo="sudo "  # sudo時にアリアス有効
alias v="view"
alias m=mysql_execute
alias tree="pwd;find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'"
alias clang++="clang++ -std=c++11"
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

_complie_and_run_in_c(){
    local main source tfile
    main=$1; shift;
    source="
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

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
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

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
    # Fflag="$IFS"
    nflag=false
    dflag=false  # debug
    pre=""
    STDIN=""
    arr=()

    while getopts nd OPT; do
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

    # set -- $STDIN
    # zshだと上記でsplitできないのでechoで対応
    for line in `echo $STDIN`; do
        pre='(setq LINE "'$line'")';
        $dflag && echo "(progn $pre $@)"
        emacsclient -e "(progn $pre $@)";
    done
    
    # close
    [ -n "$STDIN" ] && emacsclient -e '(setq STDIN "")';
    return 0;
}
alias eo=__emacs_oneliner
alias ee="emacsclient -e"
