typeset -U path PATH

# カレントディレクトリ中にサブディレクトリが無い場合に cd が検索するディレクトリのリスト
cdpath=($HOME)

typeset -ga chpwd_functions
autoload -U chpwd_recent_dirs cdr
chpwd_functions+=chpwd_recent_dirs

## ディレクトリが変わったらディレクトリスタックを表示。
chpwd_functions=($chpwd_functions dirs)

limit coredumpsize 0  # coreファイルを作らない

LIB_PERL=~/lib/perl

#--------------------------------------------------
#load setting files
#--------------------------------------------------

. ~/sh/init

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi
