
(define %ws ($skip-many ($one-of #[ \t\r\n])))

(define %s ($lift list->string ($many ($one-of #[^{}]))))
(define %c-f-body ($lazy ($lift string-append
                                   ($lift x->string ($char #\{))
                                   ; ($or ($try ($lift string-append %s ($many %c-f-body %s))) %s)
                                   ($lift (^(x xs) (string-append x (x->string xs)))
                                          %s
                                          ($lift (^x (apply string-append x)) ($many ($lift string-append %c-f-body %s))))
                                   ($lift x->string ($char #\})))))
(define %name ($lift list->string ($many1 ($none-of #[*, (){}]))))
(define %c-types ($or
                  ($string "vector_t")
                  ($string "list_t")
                  ($string "void")
                  ($string "int")
                  ($string "char")
                  ))
(define %c-type ($lift (^[t p] (let1 ts (rope->string t) (if p #"~ts *" #"~ts ")))
                       %c-types ($optional ($try ($seq %ws ($char #\*))))))

(define %c-signature ($lift
                      (^x (format "(~a)" (list->string x)))
                      ($between ($char #\()
                                ($many ($none-of #[()]))
                                ($char #\)))))

(define %c ($do
            %ws
            (type %c-type)
            %ws
            (name %name)
            %ws
            (sgnt %c-signature)
            %ws
            (body %c-f-body)
            %ws
            ($return `((name . ,name)
                       (type . ,type)
                       (sgnt . ,sgnt)
                       (body . ,body)
                       (func . ,#"~|type|~|name|~|sgnt| ~body")
                       ))))

(define %cc ($lift ($ filter (.$ not null?) $)
                   ($many ($or ($try %c)
                               ($lift (^x '()) anychar)))))
