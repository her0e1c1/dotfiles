(use srfi-1)
(use srfi-13)  ; string-null? string-every
(use srfi-14)  ; char-set:whitespace

(use srfi-27)  ; random
(use srfi-42)  ; range
(use srfi-43)  ; vector-*

(use util.stream)
(use util.list)
(use util.match)

(use text.tr)
(use file.util)

(use gauche.cgen)
(use gauche.process)
(use gauche.parseopt)
(use gauche.parameter)
(use gauche.termios)
(use gauche.sequence)
; (use gauche.cgen)
; (use gauche.internal)

(define-macro (import-only module . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,module ,sym))) syms)))

(import-only gauche.internal extended-pair? extended-cons pair-attribute-get pair-attribute-set! pair-attributes)

; s '($ p $ + 1 2 3)' => 6
(define p print)
(define f format)
(define b begin)
(define i iota)
; (define l lambda)  ; ^
(define m macroexpand)  ; TODO: quoteなくしたい (m (aif 1 it))
(define m1 macroexpand-1)
; hash
(define hm make-hash-table)
(define hp hash-table-put!)
(define hg hash-table-get)
; (define pp (pa$ print))
(define pos process-output->string)
(define IN (standard-input-port))
(define OUT (standard-output-port))
(define ERR (standard-error-port))

; sliceを作る
; (~ a 1)
; (~ a 1 2)

(define (path p)
  (string-append (home-directory) p))

; s '(p (string-slices "abcd" 2))' => (ab cd)
(define (string-slices str len)
    (map list->string (slices (string->list str) len)))

;; anaforic
(define-macro (aif pred t . f)
  `(let ((it ,pred))
     (if it ,t ,@f)))

; combinator
(define (~$ . selectors)
  (cut apply ~ <> selectors))
; ((~$ 1) "abc")  "b"

(define (slot-ref*$ . selectors)
  (cut apply slot-ref* <> selectors))

(define (flip f x y) (f y x))
; ((pa$ flip map) (iota 10) print)


;; path
(define (path-normalize . paths)
  ; (resolve-path)
   (expand-path
    (string-join paths "/")))
(define path-n path-normalize)

; build-pathがあった
; s '(p(build-path "~a" "b"))' => "~a/b"
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

; TODO: lazy let*
; (llet* ((a 1) (b 2)) (if #t a b))  ; 必要になってから評価

; 正規化あった
; (sys-normalize-pathname "~//a/./d/b" :expand #t :absolute #t :canonicalize #t)
; "/home/me/a/d/b"

(define rra regexp-replace-all)
(define rr regexp-replace)
(define-macro  (rl regex s vars . body)
  `(rxmatch-let (rxmatch ,regex ,s)
                ,vars
                ,@body))
; ((rxmatch) 1) ; => aに束縛とかよさげ?
; #"~a~b"

(define-macro (it! value)
  `(set! it ,value))

(define-macro (-> x form . more)
  (if (pair? more)
      `(-> (-> ,x ,form) ,@more )
      (if (pair? form)
          `(,(car form) ,x ,@(cdr form))
          `(,form ,x))))

(define-macro (->> x form . more)
  (if (pair? more)
      `(->> (->> ,x ,form) ,@more )
      (if (pair? form)
          `(,(car form) ,@(cdr form) ,x)
                    `(,form ,x))))

(define-macro (--> x form . more)
  (if (pair? more)
      `(--> (--> ,x ,form) ,@more)
      (if (pair? form)
          `(let1 it ,x (,(car form) ,@(cdr form)))
          `(,form ,x))))

(define-macro (-?> x form . more)
  (let1 v (gensym)
        (if (pair? more)
            `(if-let1 ,v (-?> ,x ,form) (-?> ,v ,@more ) #f )
            (if (pair? form)
                `(,(car form) ,x ,@(cdr form))
                `(,form ,x)))))

(define-macro (-?>> x form . more)
  (let1 v (gensym)
        (if (pair? more)
            `(if-let1 ,v (-?>> ,x ,form) (-?>> ,v ,@more ) #f )
            (if (pair? form)
                `(,(car form) ,@(cdr form) ,x)
                            `(,form ,x)))))

(define-macro (awhile pred . body)
  `(do ((it ,pred ,pred))
       ((or (not it) (eof-object? it)) it)
     ,@body))

(use srfi-13)
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


(define (canonicalize-argdecl argdecls)
  (define (rec args)
    (match (canonicalize-vardecl args)
      [() '()]
      [((var ':: type) . rest) `((,var . ,type) ,@(rec rest))]
      [(var . rest) `((,var . ScmObj) ,@(rec rest))]))
  (rec argdecls))

(define (canonicalize-vardecl vardecls)
  (define (expand-type elt seed)
    (cond
     [(keyword? elt)  ;; The case of (var ::type)
      (rxmatch-case (keyword->string elt)
        [#/^:(.+)$/ (_ t) `(:: ,(string->symbol t) ,@seed)]
        [else (cons elt seed)])]
     [(symbol? elt)
      (rxmatch-case (symbol->string elt)
        [#/^(.+)::$/ (_ v) `(,(string->symbol v) :: ,@seed)]
        [#/^(.+)::(.+)$/ (_ v t)
            `(,(string->symbol v) :: ,(string->symbol t) ,@seed)]
        [else (cons elt seed)])]
     [else (cons elt seed)]))

  (define (err decl) (error "invlaid variable declaration:" decl))

  (define (scan in r)
    (match in
      [() (reverse r)]
      [([? symbol? var] ':: type . rest)
       (scan rest `((,var :: ,type) ,@r))]
      [([? symbol? var] . rest)
       (scan rest `((,var :: ScmObj) ,@r))]
      [(([? symbol? v] [? symbol? t] . args) . rest)
       (scan rest `(,(expand-type v (expand-type t args)) ,@r))]
      [(([? symbol? vt] . args) . rest)
       (scan rest `(,(expand-type vt args) ,@r))]
      [(xx . rest) (err xx)]))

  (scan (fold-right expand-type '() vardecls) '()))


  (define (expand-type elt seed)
    (cond
     [(keyword? elt)  ;; The case of (var ::type)
      (rxmatch-case (keyword->string elt)
        [#/^:(.+)$/ (_ t) `(:: ,(string->symbol t) ,@seed)]
        [else (cons elt seed)])]
     [(symbol? elt)
      (rxmatch-case (symbol->string elt)
        [#/^(.+)::$/ (_ v) `(,(string->symbol v) :: ,@seed)]
        [#/^(.+)::(.+)$/ (_ v t)
            `(,(string->symbol v) :: ,(string->symbol t) ,@seed)]
        [else (cons elt seed)])]
     ; seedに結果が累積
     [else (cons elt seed)]))


