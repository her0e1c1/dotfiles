#ubuntuの場合は~/.profileがログイン時に一回だけ呼ばれるので
#そのファイルに source "$HOME/.bash_profile" を書き込んでおくこと

#mac ubuntuの判定
#本格的になったらwhoamiを使えばいい
if [ `uname` = "Darwin" ]; then
    export DROPBOX="$HOME/dropbox"

    # Read bashrc はじめに読み込むように設定する
    if [ -f ~/.bashrc ] ; then
       . "$HOME/.bashrc"
    fi

elif [ `uname` = "Linux" ]; then
     export DROPBOX="$HOME/Dropbox"
fi

#環境設定
#カレントディレクトリと共通webのライブラリを入れておく
#export PATH="$PATH":"$DROPBOX/conf/lib":.

export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

