if uname -a | grep -qv Darwin; then
   return 0;
fi   

alias opens="open ~/Dropbox/sphinx/_build/index.html"
alias openp=" ~/Dropbox/diary/output/index.html"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
