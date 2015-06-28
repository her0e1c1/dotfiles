# 空白から始めたコマンドを無視
# export HISTCONTROL=ignorespace
# 重複履歴を無視
# export HISTCONTROL=ignoredups
# 両方
# ignorespace+ignoredups = ignoreboth
export HISTCONTROL=ignoreboth

#履歴の共有
function share_history {  # 以下の内容を関数として定義
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}
PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
shopt -u histappend   # .bash_history追記モードは不要なのでOFFに

#よく使うコマンドは履歴保存対象から外す。
export HISTIGNORE="fg*:bg*:history:cd*:ls*"

#ヒストリのサイズを増やす
export HISTSIZE=100000

for i in ~/sh/*; source $i; done
