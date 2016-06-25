
#emacsのデーモン再起動
restart_emacs(){
    emacsclient -e "(kill-emacs)";
    emacs --daemon
}

kill_emacs(){
	emacsclient -e "(kill-emacs)";
}

emacs_daemon() {
    if [ -n "`emacs -Q --batch --execute "(progn (require 'server) (princ (server-running-p)))"| grep nil`" ]; then
        emacs --daemon
    else
        echo "emacs is already launched"
    fi
}

start() {
    emacs_daemon
    tmux new \; splitw -h \; neww
};


ee() {
    emacs --batch --execute "
(progn
 (require 'cl)
 (require 'comint)
  (defmacro let1 (var expr &rest body)
    \`(let ((,var ,expr))
        ,@body))
 ; caskをパスを指定してロードすれば、
 ; 合わせて以下のモジュールをロードしたことになる
 ; '(s dash f commander git epl shut-up cl-lib package-build)
 (require 'cask \"~/.cask/cask.el\")
 (print (progn $1))
)
"
}
