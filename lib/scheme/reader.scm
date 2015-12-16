
; (print #!Q   hofg  ggg )
; (let ((a #!Q"hoge"Q)) a)
(define-reader-directive 'Q
  (^(sym port ctx)
    (define (finish l) (string-trim-both (list->string (reverse l))))
    (let loop ((ch (read-char port))
               (acc '()))
      (let1 chacc (cons ch acc)
            (cond
             ;((char=? #\) (peek-char port)) (finish chacc))
             ;((char=? #\newline ch) (finish acc))
             ((char=? #\Q ch) (finish acc))
             (else (loop (read-char port) chacc)))))))

(define-reader-directive 'HEREDOCUMENT
  (^(sym port ctx)
    (let1 delimiter (string-trim-both (read-line port))
          (do ((line (read-line port) (read-line port))
               (document '() (cons line document)))
              ((string=? line delimiter)
               (string-concatenate-reverse (intersperse "\n" document)))))))
