#LEFT PROMPT
PS1="[${USER}]%(!.#.$) "
##右プロンプト
# 右側に時間を表示する
#RPROMPT="%T"
RPROMPT='%B%F{yellow}[%f%b %B%F{yellow}%~]%f%b'
setopt transient_rprompt          # 右側まで入力がきたら時間を消す
setopt prompt_subst               # 便利なプロント
bindkey -e                        # emacsライクなキーバインド

export LANG=ja_JP.UTF-8
export EDITOR=vi
export PAGER=less

autoload -U colors
colors
#cd ~/d/ => ~/desctopのように補完
autoload -U compinit
compinit -u
#cdしたときにも、dirsのスタックに積む
setopt auto_pushd
# 同ディレクトリを履歴に追加しない
setopt pushd_ignore_dups
#
setopt auto_cd
#色を使う
setopt prompt_subst
# リストを詰めて表示
setopt list_packed
# 補完一覧ファイル種別表示
setopt list_types                 
setopt nolistbeep
#setopt correct

#括弧の対応等を自動的に補完
setopt auto_param_keys

#ディレクトリ名の補完で末尾に/を自動的に付加する
setopt auto_param_slash

#show error code
setopt printexitvalue

# 履歴
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

#dirsの履歴
DIRSTACKSIZE=100
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks         # スペース排除
setopt share_history
setopt EXTENDED_HISTORY           # zshの開始終了を記録
setopt hist_no_store

# history 操作まわり
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
bindkey "^[/" undo
bindkey "^[?" redo
bindkey "^[H" run-help

setopt long_list_jobs
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

#cdをしたときに現在のディレクトリを補完に含めない
zstyle ':completion' ignore-parents parent pwd ..
#補完候補を ←↓↑→ で選択 (補完候補が色分け表示される)
zstyle ':completion:*:default' menu select=2
# カレントディレクトリに候補がない場合のみ cdpath 上のディレクトリを候補
#zstyle ':completion:*:cd:*' tag-order local-directories path-directories
zstyle ':completion:*:cd:*' ignore-parents parent pwd

#function
cdls(){
	if [ ${#1} -eq 0 ]; then
	   cd && ls
	else
       \cd "$*" && ls
	fi
}

autoload smart-insert-last-word
zle -N insert-last-word smart-insert-last-word
zstyle :insert-last-word match \
  '*([^[:space:]][[:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'
bindkey '^]' insert-last-word

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

zshaddhistory(){
    local line=${1%%$'\n'}
    local cmd=${line%% *}

    [[ ${#line} -ge 4
       && ${cmd} != (l|l[sal])
       && ${cmd} != (c|cd)
       && ${cmd} != (m|man)
    ]]
}


[ -f ~/.zshrc.include ] && source ~/.zshrc.include # 設定ファイルのinclude
[ -f ~/.sh.d/export ] && source ~/.sh.d/export
[ -f ~/.sh.d/alias ] && source ~/.sh.d/alias

zstyle ':completion:*' list-colors ''



export LSCOLORS=exfxcxdxbxegedabagacad

if uname -a | grep -q 'Ubuntu'
then
 alias ls="ls --color"
else
 alias ls="ls -G"
fi

zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
			     /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

zstyle ':completion:*:default' menu select true


#C-dでログアウトしない
setopt ignore_eof

# pushd,popdの度にディレクトリスタックの中身を表示しない
setopt pushd_silent

# カレントディレクトリ中にサブディレクトリが無い場合に cd が検索するディレクトリのリスト
cdpath=($HOME)

#beep音を鳴らさない
setopt nobeep

# cd -[tab]とcd +[tab]の役割を逆にする
setopt pushd_minus

#入力の予測
autoload predict-on
#predict-on

#alias -s
alias -s tgz="tar xvf"
alias -s tar.gz="tar xvf"
alias -s zip="unzip"

# alias -s zip=zipinfo
# alias -s gz=gzcat
# alias -s tbz=bzcat
# alias -s bz2=bzcat
# alias -s java=lv
# alias -s c=lv
# alias -s h=lv
# alias -s C=lv
# alias -s cpp=lv
# alias -s sh=lv
# alias -s txt=lv
# alias -s xml=lv
# alias -s html=firefox
# alias -s xhtml=firefox
# alias -s gif=display
# alias -s jpg=display
# alias -s jpeg=display
# alias -s png=display
# alias -s bmp=display
# alias -s mp3=amarok
# alias -s m4a=amarok
# alias -s ogg=amarok
# alias -s mpg=svlc
# alias -s mpeg=svlc
# alias -s avi=svlc
# alias -s mp4v=svlc

#alias -g
alias -g L="| less"
alias -g G='| grep'
alias -g X='| xargs'

# バックグラウンドジョブの状態変化を即時報告する
setopt notify

# =以降も補完する(--prefix=/usrなど)
setopt magic_equal_subst 

#alias -g pd="pushd +"

setopt extended_glob # グロブ機能を拡張する
unsetopt caseglob    # ファイルグロブで大文字小文字を区別しない


alias -g ..="../.."
#zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -e -o pid,user,tty,cmd'

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
alias l='ls -CF'
alias pu='pushd'
alias po='popd'
alias ds='dirs -v'
alias j="jobs"
alias i="ipython3"
alias e='echo $?'
#alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

