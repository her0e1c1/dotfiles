if uname -a | grep -qv Darwin; then
   return 0;
fi   

alias opens="open ~/Dropbox/sphinx/_build/index.html"
alias openp="open ~/Dropbox/diary/output/index.html"

alias emacs="/usr/local/bin/emacs"
alias emacsclient="/usr/local/bin/emacsclient"
# パッケージからインストールした場合
alias emacsp="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacsclientp="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
