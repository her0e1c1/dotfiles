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

(define (!find path :key (max #f) (min #f) (type #f))
  )

(define (cmd-c path . args)
  (let1 sa (string-join (map x->string args) " ")
        #"clang ~path && ./a.out ~sa"))

(define (run-c path . args)
  (define sa (string-join (map x->string args) " "))
  (if (file-exists? path)
      (process-output->string-list #"clang ~path && ./a.out ~sa")
      ""))

(define (run-scheme path)
  (if (file-exists? path)
      (process-output->string-list #"gosh ~path")
      ""))

(define (get-run-process language)
  (cond ((equal? language "scheme") run-scheme)
        ((equal? language "c") run-c)
  ))

(define (get-time thunk)
  (let1 t (make <real-time-counter>)
        (with-time-counter t (thunk))
        (time-counter-value t)))
