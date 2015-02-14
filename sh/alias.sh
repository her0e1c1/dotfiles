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
alias ee='emacsclient -e'
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
