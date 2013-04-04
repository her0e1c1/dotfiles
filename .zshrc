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
setopt autopushd		          # cdの履歴を表示
setopt pushd_ignore_dups          # 同ディレクトリを履歴に追加しない
setopt auto_cd                    # 自動的にディレクトリ移動
setopt list_packed 		  # リストを詰めて表示
setopt list_types                 # 補完一覧ファイル種別表示

# 履歴
HISTFILE=~/.zsh_history           # historyファイル
HISTSIZE=10000                    # ファイルサイズ
SAVEHIST=10000                    # saveする量
setopt hist_ignore_dups           # 重複を記録しない
#先頭が空白のコマンドを履歴に残さない
setopt hist_ignore_space
setopt hist_reduce_blanks         # スペース排除
# 履歴ファイルを共有
setopt share_history
setopt EXTENDED_HISTORY           # zshの開始終了を記録

# history 操作まわり
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
bindkey "^[/" undo
bindkey "^[?" redo


zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34' 

#cd ..をしたときに現在のディレクトリを補完に含めない
zstyle ':completion' ignore-parents parent pwd ..

#function
cdls(){
	if [ ${#1} -eq 0 ]; then
	   cd && ls
	else
       \cd "$*" && ls
	fi
}

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
[ -f ~/.sh.d/export ] && source .sh.d/export
[ -f ~/.sh.d/alias ] && source .sh.d/alias

