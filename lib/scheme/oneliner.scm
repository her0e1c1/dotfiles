
(define-macro (s regexp replaced :optional (options :))
  (let* ((chars (string->list (keyword->string options)))
         (regexp (if (symbol? regexp) (symbol->string regexp) regexp))
         (g (if (memq #\g chars) regexp-replace-all regexp-replace))
         (p (memq #\p chars))
         )
    (if p
        `(lambda (s) (,g ,regexp s ,replaced))
        `(,g ,regexp it ,replaced))))

; s '(a 10 HOGE)'
(define-syntax a
  (syntax-rules ()
    ((a b . hoge)
     (apply + hoge)))
  )
; (define-macro hoge (a) `(+ a 1))
