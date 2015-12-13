

(define A0 (ignore *program-name*))
(define A1 (ignore (car *argv*)))
(define A2 (ignore (cadr *argv*)))
(define A3 (ignore (caddr *argv*)))
(define A4 (ignore (cadddr *argv*)))

(define (pp x) (if (false? x) "" (print x)))


(define wof with-output-to-file)
(define cof call-with-output-file)
(define envs (get-environment-variables))
; d describe
; sys-lstat
(define pwd current-directory)
(define d describe)
(define df define)
(define p print)
(define f format)
(define b begin)
(define i iota)
(define sq subseq)
(define (t . body) (lambda () body))
; when unless if
(define-macro (fi a b :optional c)
  `(if ,a ,c ,b))
(define m1 macroexpand-1)
(define-macro (m . body)  ; (m MACRO)
  `(macroexpand (quote ,body)))
(define-macro (m- body) ; (m (MACRO))
  `(macroexpand (quote ,body)))
; hash
(define hm make-hash-table)
(define hp hash-table-put!)
(define hg hash-table-get)
; (define pp (pa$ print))
(define rv receive)
(define id identity)
(define pps peg-parse-string)
(define exec process-output->string)
(define execl process-output->string-list)
(define IN (standard-input-port))
(define OUT (standard-output-port))
(define ERR (standard-error-port))

(define f? file-exists?)
; use as it is
; time
; sys-sleep
