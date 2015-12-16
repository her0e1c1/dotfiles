; mv

; commands
; ls/find/which/grep/xargs/csv/cat
; peco(anything)

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

(define (run-scheme path)
  (if (file-exists? path)
      (process-output->string-list #"gosh ~path")
      ""))

(define (run-ruby path)
  (process-output-with-error->string `(ruby ,path)))


(define (get-run-process language)
  (cond ((member language '("scheme" "scm")) run-scheme)
        ((equal? language "c") run-c)
        (else (error "No language " language))))

(define (get-time thunk)
  (let1 t (make <real-time-counter>)
        (with-time-counter t (thunk))
        (time-counter-value t)))

(define (process-output-with-error->string cmd)
  (port->string
   (process-output 
    (run-process cmd :redirects '((>& 2 1) (> 1 stdout)) :wait #t))))

; sh doesn't work if there are more than one process
(define (oneliner-wrap-shell cmd)
  (let1 c (shell-escape-string cmd)
        `(sh -c ,#". ~~/.shrc \n ~cmd")))

(define (oneliner-run cmd)
  (process-output-with-error->string (oneliner-wrap-shell cmd)))

(define (oneliner-c cmd) #"ce '~cmd'")
(define (oneliner-elisp cmd) #"emacs --batch --execute '(message ~cmd)'")

; TODO: make macro and use args as string
(define (run-ce cmd . args)
  (let* ((s (string-join (map x->string args) " "))
         (wrap (oneliner-c cmd))
         (cmd+args #"~wrap ~s")) 
    (process-output-with-error->string (oneliner-wrap-shell cmd+args))))

(define (oneliner-simple-run-print cmd)
  (let1 ret (oneliner-run cmd)
        (p (sphinx-block #"~cmd\n~ret" :code-block "sh"))))

(define (cmd-c path . args)
  (let1 sa (string-join (map x->string args) " ")
        #"clang ~path && ./a.out ~sa"))

(define (run-c path . args)
  (define sa (string-join (map x->string args) " "))
  (if (file-exists? path)
      (process-output->string-list #"clang ~path && ./a.out ~sa")
      ""))

(define (run-c-from-string str)
  (oneliner-run #"run_c_from_string '~str'"))

(define (run-cpp-from-string str)
  (oneliner-run #"run_cpp_from_string '~str'"))
