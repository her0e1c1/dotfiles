if uname -a | grep -qv Darwin; then
   return 0;
fi   

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"