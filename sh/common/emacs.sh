
#emacsのデーモン再起動
restart_emacs(){
    emacsclient -e "(kill-emacs)";
    emacs --daemon
}

kill_emacs(){
	emacsclient -e "(kill-emacs)";
}
