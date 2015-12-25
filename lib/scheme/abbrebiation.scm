
(define pwd current-directory)
(define envs (get-environment-variables))

(define A0 (ignore *program-name*))
(define A1 (ignore (car *argv*)))
(define A2 (ignore (cadr *argv*)))
(define A3 (ignore (caddr *argv*)))
(define A4 (ignore (cadddr *argv*)))

(define IN (standard-input-port))
(define OUT (standard-output-port))
(define ERR (standard-error-port))

(define rr regexp-replace)
(define rra regexp-replace-all)
(define (pp x) (if (false? x) "" (print x)))


(define wof with-output-to-file)
(define cof call-with-output-file)

; d describe
; sys-lstat

(define d describe)
(define df define)
(define p print)
(define f format)
(define b begin)
(define i iota)
(define sq subseq)
(define (t . body) (lambda () body))

(define m1 macroexpand-1)
(define-macro (m . body)  ; (m MACRO)
  `(macroexpand (quote ,body)))

; join method
(define (f-join . paths)
  (let1 p (apply build-path
                 (map (^p (sys-normalize-pathname p :absolute #f :canonicalize #t :expand #t)) paths))
        (sys-normalize-pathname p :absolute #t)))

(define-method abs ((x <string>))
  (sys-normalize-pathname x :absolute #t :canonicalize #t :expand #t))
(define-method rltv ((x <string>))
  (sys-normalize-pathname x :absolute #f :canonicalize #t :expand #t))

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

(define f? file-exists?)
; use as it is
; time
; sys-sleep

(define s-join string-join)


; for regex
; TODO: ($M a b) くらいの記述量にする
(define-macro ($a :optional match)
  (if (undefined? match)
      `(it 'after)
      `(,match 'after)))
(define-macro ($b :optional match)
  (if (undefined? match)
      `(it 'before)
      `(,match 'before)))
