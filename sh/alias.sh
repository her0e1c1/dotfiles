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
alias r='ruby -e'
alias p="perl $PERL_OPTION -E"
alias pn="perl $PERL_OPTION -nalE"
alias pp="perl $PERL_OPTION -palE"
alias pp='perl -plE'
alias pn='perl -nlE'
alias f='fab -f ~/fabfile'
alias c="cdr"
alias sudo="sudo "  # sudo時にアリアス有効
alias v="view"
alias m=mysql_execute
alias tree="pwd;find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'"
alias clang++="clang++ -std=c++11"
alias eo=__emacs_oneliner
alias ee="emacsclient -e"
alias ea='emacsclient -nw -a ""'
alias en='emacsclient -n'
alias e='emacsclient -t'
alias d='emacsclient -t .'
alias ..=".."