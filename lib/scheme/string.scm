

(define s-join string-join)

(define (string-slices str len)
    (map list->string (slices (string->list str) len)))


