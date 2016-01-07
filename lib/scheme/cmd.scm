; mv

; commands
; ls/find/which/grep/xargs/csv/cat
; peco(anything)

(define-macro (with-current-directory path . body)
  `(let1 old-path (current-directory)
         (current-directory ,path)
         ,@body
         (current-directory old-path)))

(define (--ls . dirs)
  (let* ((keys (filter keyword? dirs))
         (opt-abs (memq :abs keys))
         (opt-c (not (memq :c keys)))
         (opt-e (not (memq :e keys)))
         (opt-a (not (memq :a keys)))
         (opt-all (not (memq :all keys)))
         (sdirs (filter string? dirs))
         (sdirs (if (null? sdirs) (list "./") sdirs)))
    (let1 lists (map (^x (--> x
                         (sys-normalize-pathname it :absolute opt-abs :canonicalize opt-c :expand opt-e)
                         (build-path it "*")
                         (glob it)
                         ))
                     sdirs)
          (if opt-a (apply append lists) lists))))

; (ls ~/.emacs.d :abs :e)
(define-macro (ls . dirs)
  `(--ls ,@(map x->string-without-keyword dirs)))


(define-macro (! . args)
  (let1 ss (map x->string args)
        `(sys-system (string-join (map x->string (list ,@ss)) " "))))

(define (which cmd)
  (let1 found (filter-map (^x (and-let* ((p (build-path x cmd))
                                         (q (file-is-executable? p)))
                                        p))
                          (string-split (sys-getenv "PATH") ":"))
        (if (null? found) #f (car found))))


(define (-find a)
  (map -find (--ls a)))

; directory-fold path proc seed
(define (!find path :key (max #f) (min #f) (type #f))
  (fold (lambda (x acc) (if (file-is-directory? x)
                            (append (cons x (!find x)) acc)
                            (cons x acc)))
        '() (--ls path)))


(define (get-time thunk)
  (let1 t (make <real-time-counter>)
        (with-time-counter t (thunk))
        (time-counter-value t)))

(define (process-output-with-error->string cmd)
  (port->string
   (process-output 
    (run-process cmd :redirects '((>& 2 1) (> 1 stdout)) :wait #t))))

; sh doesn't work if there are more than one process
(define (oneliner-wrap-shell cmd :key (sh 'sh))
  (let1 c (shell-escape-string cmd)
        `(,sh -c ,#". ~~/.shrc \n ~cmd")))

(define (oneliner-run cmd :key (sh 'sh))
  (let1 s (oneliner-wrap-shell cmd :sh sh)
        (process-output-with-error->string s)))

(define (oneliner-c cmd) #"ce '~cmd'")
(define (oneliner-elisp cmd) #"emacs --batch --execute '(message ~cmd)'")

(define (oneliner-simple-run-print cmd)
  (let1 ret (oneliner-run cmd)
        (p (sphinx-block #"~cmd\n~ret" :code-block "sh"))))

;;; run from string
;;; ignore argv for now
(define (run-c-from-string str argv)
  #"run_c_from_string '~str'")
  ;; (let1 e (quote-expression str)
  ;;       #"run_c_from_string '~e'"))
(define (run-cpp-from-string str argv) #"run_cpp_from_string '~str'")
(define (run-gauche-from-string str argv)
  #"gosh << EOS
(let1 r (print (format \"~~s\" ~str)) (if (not (undefined? r)) r))
EOS")
(define (run-ruby-from-string str argv)
  #"ruby << EOS
~str
EOS")
(define (run-node-from-string str argv)
  #"node << EOS
~str
EOS")
(define (run-sh-from-string str argv)
  #"sh << EOS
~str
EOS")
(define (run-zsh-from-string str argv)
  #"zsh << EOS
~str
EOS")
(define (run-py-from-string str argv)
  #"python << EOS
#coding: utf-8
~str
EOS")
(define (run-gosh-from-string str argv)
  #"gosh << EOS
~str
EOS")
(define (run-java-from-string str argv)
  #"
cd `mktemp -d`
cat << EOS > Main.java
~str
EOS
javac Main.java
java Main
")
(define (run-ghc-from-string str argv)
  #"
t=`mktemp`
o=$t.hs
mv $t $o
cat << EOS > $o
~str
EOS
runhaskell $o
rm $o
")

(define (run-from-string str lang argv)
  (let1 proc (eval-null (string->symbol (format "run-~a-from-string" lang)))
        (oneliner-run (proc str argv))))

;;; quote
(define (escape-single-quote str)
  (regexp-replace-all #/'/ str "'\\\\''"))

(define (quote-expression-with-single cmd)
  (let1 e (escape-single-quote cmd)
        #"'~e'"))

(define (quote-expression-with-dobule cmd)
  (let1 e cmd
        #"\"~e\""))

(define (quote-expression cmd :key (quote #\'))
  (if (eq? quote #\')
      (quote-expression-with-single cmd)
      (quote-expression-with-dobule cmd)))

(define (run-py-from-path path argv) #"python ~path")
(define (run-java-from-path path argv)
  (let* ((d (sys-dirname path))
         (b (sys-basename path)))
    #"o=`pwd` && javac ~path && \cd ~d > /dev/null && java ~(path-swap-extension b #f) && cd $o > /dev/null"))

(define (run-from-path path :key (language #f) (argv #f))
  (set! language (if language language (path-extension path)))
  (let1 proc (eval-null (string->symbol (format "run-~a-from-path" language)))
        (oneliner-run (proc path argv))))

(define-macro (here path)
  (build-path (sys-dirname *program-name*) path))
