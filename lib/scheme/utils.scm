
; (clear)
(define clear
  (let1 c (process-output->string '("clear"))
        (lambda () (display c))))

(define (create-file-from-string filepath string)
  (with-output-to-file (x->string filepath)
    (lambda () (print string))))
