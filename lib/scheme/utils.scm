
; (clear)
(define clear
  (let1 c (process-output->string '("clear"))
        (lambda () (display c))))

(define (create-file-from-string filepath string)
  (with-output-to-file (x->string filepath)
    (lambda () (print string))))

(define (x->human x)
  (let loop ((unit '("" "K" "M" "G" "T" "P" "Z"))
             (num x))
    (cond
     ((null? unit) #"~num byte")
     ((< num (expt 2 10)) #"~(x->integer num) ~(car unit)byte")
     (else (loop (cdr unit) (/ num (expt 2 10)))))))

(define (swap alist i j)
  ((pa$ flip map (iota (length alist)))
   (^x (ref alist
            (cond ((= x i) j)
                  ((= x j) i)
                  (else x))))))

; (integers$ 1e10) ; error
(define (random :optional (size 1e10))
  (car (generator->list (integers$ (x->integer size)) 1)))

(define (shuffle alist)
  (if (null? alist) '()
      (let* ((i (mod (random) (length alist)))
             (l (swap alist 0 i)))
        (cons (car l) (shuffle (cdr l))))))
