
(define (string-slices str len)
    (map list->string (slices (string->list str) len)))


(define (lineno-at index string-list)
  (let loop ((no 0) (count 0) (lines string-list))
    (cond ((null? lines) -1)
          ((<= index count) no)
          (else
           (loop (+ no 1)
                 (+ count (string-length (car lines)))
                 (cdr lines))))))

(define (string-line-range container string)
  (if-let1 index (string-contains container string)
           (let1 s-list (string-split container "\n")
                 (values (lineno-at index s-list)
                         (lineno-at (+ index (string-length string)) s-list)))
           (values -1 -1))) 

(define (string-indent s :key (indent "    "))
  (if (null? s) ""
  (let1 slist (if (pair? s) s (string-split s "\n"))
        (string-join (map (^x (string-append indent x)) slist) "\n")
        )))

(define s-indent string-indent)

(define (string-width s)
  (string-fold (lambda (c acc) (+ acc (if (char-set-contains? char-set:ascii c) 1 2))) 0 s))


(define (join-line list) (string-join list "\n"))
(define string-join-line join-line)
