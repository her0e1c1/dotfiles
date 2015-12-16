(add-load-path "." :relative)

(define-macro (ignore body :optional default)
  `(guard (_ (else ,default)) ,body))

(load "load.scm")
(load "fp.scm")
(load "anaforic.scm")
(load "abbrebiation.scm")
(load "string.scm")


(define (false? x)
  (or (eq? x '())
      (eq? x #f)
      (equal? x "")
      (undefined? x)
      (eof-object? x)
      (and (number? x) (= x 0))
      ))
(define (x->empty-string x))
;; call- in/outを受け取る
;; with- current-portを変更

(define rl read-line)

; anaforic-read-lines
; (file->string "PATH")
(define (rls filepath f)
  (call-with-input-file filepath
    (lambda (in)
      (let loop ((line (read-line in)))
        (if (eof-object? line)
            (undefined)
            (begin (f line)
                   (loop (read-line in))))))))

; sliceを作る
; (~ a 1)
; (~ a 1 2)

;; (define (path p)
;;   (string-append (home-directory) p))

; s '(p (string-slices "abcd" 2))' => (ab cd)
;; path
(define (path-join . ls)
  (let loop ((path ls)
             (acc '()))
    (let* ((n (null? path))
           (p (and (not n) (car path)))
           (next (and (not n) (cdr path)))
           (p0 (and p (not (string-null? p)) (char=? #\/ (~ p 0))))
           )
      (cond (n (apply path-normalize (reverse acc)))
            (p0 (loop next (list (string-append "/" (string-trim p #\/)))))
            (else (loop next (cons p acc)))
            ))))

(define-macro  (rl regex s vars . body)
  `(rxmatch-let (rxmatch ,regex ,s)
                ,vars
                ,@body))

(define (rm rxp str)
  (if-let1 m (rxmatch rxp str)
       (list
        (m 0)
        (m 'before 0)
        (m 'after 0)

        (rxmatch-num-matches m)
        (rxmatch-named-groups m)
        (rxmatch-start m)
        (rxmatch-end m)
        (rxmatch-substring m)
        (rxmatch-substrings m)
        (rxmatch-positions m)
        (rxmatch-before m)
        (rxmatch-after m)

        (regexp->string rxp)
        (regexp-num-groups rxp)
        (regexp-named-groups rxp)

        (rxmatch->string rxp str)
        )
       #f))
; "\\0" => #!r"\0"
; #!""がいいせめて
(define-reader-directive 'r
  (^(sym port ctx)
    (if (char=? #\" (read-char port))
        (do ((ch (read-char port) (read-char port))
             (str '() (cons ch str)))
            ((char=? #\" ch) (list->string (reverse str))))
        (error "\" is needed"))))
; s '(rr #/(\w*)/ "abc" #!r"\0 ~1")'

(define orig~ ~)
(define-macro (~ . body)
  (case (length body)
    ((0) '())
    ((1) (car body))
    ((2) `(orig~ ,(car body) ,(cadr body)))
    ((3) `(subseq ,(car body) ,(cadr body) ,(caddr body)))
    ))


(define (x->string-without-keyword x)
  (if (keyword? x) x (x->string x)))

(define type-equals '(number? string? char? char-set?))
(define (same? a b) 1)

(define (to a b)
  (let ((an (x->number a))
        (bn (x->number b)))
    (if (> an bn) (error "a < b"))
    (map integer->char (iota (+ 1 (- bn an)) an))))
        

(define (P a b)
  (p (peg-parse-string a b)))

(define (generator-from-file filepath %grammer proc)
  (let1 gen (call-with-input-file filepath
              (lambda (in)
                (generator->list (peg-parser->generator %grammer in))))
        ((flip$ map) (car gen) proc)))
    
(load "utils.scm")
(load "parser.scm")
(load "sphinx.scm")
(load "cmd.scm")
(load "oneliner.scm")
(load "reader.scm")

; sphinx-load-from-current-dirctory (p section)
(define-macro (load-from-current-dirctory path)
  `(begin
     (add-load-path "../.." :relative)
     (add-load-path ".." :relative)
     (add-load-path "." :relative)
     (load ,path)))

(define (debug-source-info obj)
  (and-let* ([ (pair? obj) ]
             [info ((with-module gauche.internal pair-attribute-get)
                    obj 'source-info #f)]
             [ (pair? info) ]
             [ (pair? (cdr info)) ])
            info))
