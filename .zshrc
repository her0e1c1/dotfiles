# PROMPT
PS1="[@${HOST%%.*} %1~]%(!.#.$) " # この辺は好み
RPROMPT="%T"                      # 右側に時間を表示する
setopt transient_rprompt          # 右側まで入力がきたら時間を消す
setopt prompt_subst               # 便利なプロント
bindkey -e                        # emacsライクなキーバインド

export LANG=ja_JP.UTF-8
export EDITOR=vi 
export PAGER=less

#cd ~/d/ => ~/desctopのように補完
autoload -U compinit
compinit -u                       # このあたりを使わないとzsh使ってる意味なし
setopt autopushd
setopt pushd_ignore_dups          # 同ディレクトリを履歴に追加しない
setopt auto_cd
setopt list_packed 		  # リストを詰めて表示
setopt list_types                 # 補完一覧ファイル種別表示

# 履歴
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
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

setopt long_list_jobs
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34' 

#cd ..をしたときに現在のディレクトリを補完に含めない
zstyle ':completion' ignore-parents parent pwd ..
zstyle ':completion:*:default' menu select=2

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

alias -g L="| less"
