(add-load-path "." :relative)
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

(define (path p)
  (string-append (home-directory) p))
(define path-n path-normalize)

; s '(p (string-slices "abcd" 2))' => (ab cd)
;; path
(define (path-normalize . paths)
  ; (resolve-path)
   (expand-path
    (string-join paths "/")))

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

(define-reader-directive 'hd
  (^(sym port ctx)
    (let1 delimiter (string-trim-both (read-line port))
          (do ((line (read-line port) (read-line port))
               (document '() (cons line document)))
              ((string=? line delimiter)
               (string-concatenate-reverse (intersperse "\n" document)))))))
#|
(display #!hd END
aa
bb
END 
)
|#

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


; (clear)
(define clear
  (let1 c (process-output->string '("clear"))
        (lambda () (display c))))

(define orig~ ~)
(define-macro (~ . body)
  (case (length body)
    ((0) '())
    ((1) (car body))
    ((2) `(orig~ ,(car body) ,(cadr body)))
    ((3) `(subseq ,(car body) ,(cadr body) ,(caddr body)))
    ))

(define (f-join . paths)
  (let1 p (apply build-path
                 (map (^p (sys-normalize-pathname p :absolute #f :canonicalize #t :expand #t)) paths))
        (sys-normalize-pathname p :absolute #t)))


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

(define (generator-from-file filepath %grammer proc)
  (let1 gen (call-with-input-file filepath
              (lambda (in)
                (generator->list (peg-parser->generator %grammer in))))
        ((flip$ map) (car gen) proc)))

(define (s-indent s :key (indent "    "))
  (if (null? s) ""
  (let1 slist (if (pair? s) s (string-split s "\n"))
        (string-join (map (^x (string-append indent x)) slist) "\n")
        )))

(define (cmd-c path . args)
  (let1 sa (string-join (map x->string args) " ")
        #"clang ~path && ./a.out ~sa"))

(define (run-c path . args)
  (define sa (string-join (map x->string args) " "))
  (if (file-exists? path)
      (process-output->string-list #"clang ~path && ./a.out ~sa")
      ""))

(define (run-scheme path)
  (if (file-exists? path)
      (process-output->string-list #"gosh ~path")
      ""))

(define (get-run-process language)
  (cond ((equal? language "scheme") run-scheme)
        ((equal? language "c") run-c)
  ))

(define (get-time thunk)
  (let1 t (make <real-time-counter>)
        (with-time-counter t (thunk))
        (time-counter-value t)))

(define-macro (s regexp replaced :optional (options :))
  (let* ((chars (string->list (keyword->string options)))
         (regexp (if (symbol? regexp) (symbol->string regexp) regexp))
         (g (if (memq #\g chars) regexp-replace-all regexp-replace))
         (p (memq #\p chars))
         )
    (if p
        `(lambda (s) (,g ,regexp s ,replaced))
        `(,g ,regexp it ,replaced))))
    
(define-macro (ignore body :optional default)
  `(guard (_ (else ,default)) ,body))

(load "sphinx.scm")
(load "cmd.scm")
