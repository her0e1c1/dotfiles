(use srfi-1)
(use srfi-11)
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
(use parser.peg)  ; parsec

(use gauche.cgen)
(use gauche.generator)
(use gauche.interactive)
(use gauche.process)
(use gauche.parseopt)
(use gauche.parameter)
(use gauche.sequence)
(use gauche.termios)
(use gauche.test)

(use rfc.json)
; (use gauche.internal)

(define-macro (import-only module . syms)
  `(begin
     ,@(map (lambda (sym) `(define ,sym (with-module ,module ,sym))) syms)))

(import-only gauche.internal extended-pair? extended-cons pair-attribute-get pair-attribute-set! pair-attributes)

;; 標準出力を文字列に
;; (p (call-with-output-string
;;   (lambda (out) (write "HOGE" out))))
;; fileのportを受け取る
;; (call-with-output-file "./test.txt"
;;   (lambda (out) (write "this is a test" out)))
;; (with-output-to-file "./test.txt"
;;   (lambda () (print "this is a test")))

(define wof with-output-to-file)
(define cof call-with-output-file)

; s '($ p $ + 1 2 3)' => 6
; d describe
; sys-lstat
(define pwd current-directory)
(define d describe)
(define df define)
(define p print)
(define f format)
(define b begin)
(define i iota)
(define s subseq)
; (define l lambda)  ; ^
; when unless if
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
(define (false? x)
  (or (eq? x '())
      (eq? x #f)
      (equal? x "")
      (undefined? x)
      (eof-object? x)
      (= x 0)  ; last
      ))
(define (x->empty-string x))
;; call- in/outを受け取る
;; with- current-portを変更

;(define open call-with-input-file)
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

; s '(filter ($ not $ even? $) (i 10))'
; s '(filter (.$ not even?) (i 10))'

(define s-join string-join)
(define f? file-exists?)
; use as it is
; time
; sys-sleep

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
(define (flip$ f) (pa$ flip f))
; ((pa$ flip map) (iota 10) print)


;; path
(define (path-normalize . paths)
  ; (resolve-path)
   (expand-path
    (string-join paths "/")))
(define path-n path-normalize)

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
; (#/REGEX/ "strng")
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

(define-macro (it! value)
  `(set! it ,value))

; ... は 0 個を含む任意個の式 (,@の代わりっぽい)

; (guard (var cond) cddr)
; (guard (exc) 1)
; (guard (exc) (/ 1 0))
; (guard (exc (else 0)) (/ 1 0))

; 関数にすると、ローカル変数に代入する意味のないものになる
;; (define-macro (null! x)
;;   (guard (_ (else `(define ,x '())))
;;           x `(set! ,x '())))

; (lambda (it) BODY) => (--^ BODY)
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

(define-macro (amap f ls)
  `(map (lambda (it) ,f) ,ls))

(define-macro (afilter f ls)
  `(filter (lambda (it) ,f) ,ls))

(define-macro (aeach f ls)
  `(for-each (lambda (it) ,f) ,ls))

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


; commands
; ls/find/which/grep/xargs/csv/cat
; peco(anything)

(define (--ls . dirs)
  (let* ((keys (filter keyword? dirs))
         (opt-abs (memq :abs keys))
         (opt-c (not (memq :c keys)))
         (opt-e (not (memq :e keys)))
         (opt-a (not (memq :a keys)))
         (opt-all (not (memq :all keys)))
         (sdirs (filter string? dirs))
         (sdirs (if (null? sdirs) (list "./") sdirs)))
    (let1 lists (map (^x (--> x
                         (sys-normalize-pathname it :absolute opt-abs :canonicalize opt-c :expand opt-e)
                         (build-path it "*")
                         (glob it)
                         ))
                     sdirs)
          (if opt-a (apply append lists) lists))))

; (ls ~/.emacs.d :abs :e)
(define-macro (ls . dirs)
  `(--ls ,@(map x->string-without-keyword dirs)))

; 結果違う
; (list->string (quote (#\a #\b)))
; (x->string (quote (#\a #\b)))

(define (x->string-without-keyword x)
  (if (keyword? x) x (x->string x)))

(define-macro (! . args)
  (let1 args (map x->string args)
        (sys-system (string-join args " "))))

(define (which cmd)
  (let1 found (filter-map (^x (and-let* ((p (build-path x cmd))
                                         (q (file-is-executable? p)))
                                        p))
                          (string-split (sys-getenv "PATH") ":"))
        (if (null? found) #f (car found))))


(define type-equals '(number? string? char? char-set?))
(define (same? a b) 1)

; (list->string (to #\あ #\ん))
; (char-set->list #[a-z])
; (char-set->list #[あ-ん])
(define (to a b)
  (let ((an (x->number a))
        (bn (x->number b)))
    (if (> an bn) (error "a < b"))
    (map integer->char (iota (+ 1 (- bn an)) an))))
        

(define (-find a)
  (map -find (--ls a)))


; (df a (lcons* 1 2 3 (sys-sleep 10) 5))
; size-of
;; (define (len obj)
;;   (cond
;;    ((string? obj) (string-length obj))
;;    ((pair? obj) (length obj))
;;    (else 0)))


;; (p (peg-parse-string ($string "abc") "abc"))
;; (p (peg-parse-string ($char #\a) "abc"))
;; (p (peg-parse-string ($one-of #[a-z]) "cd"))
;; (p (peg-parse-string ($none-of #[a-z]) "A"))
; (p (peg-parse-string anychar "a"))
; (p (peg-parse-string digit "123"))

(define (P a b)
  (p (peg-parse-string a b)))
; (p (peg-parse-string anychar "a"))
; (p (peg-parse-string digit "123"))
; (p (peg-parse-string eof ""))
; (p (peg-parse-string anychar ""))
;; (P ($or ($string "abc")
;;          ($string "efg"))
;;     ; "abc"))
;;     "efg")
;; (p(equal? "bar"
;;             (pps
;;  ($try ($seq ($string "foo") ($string "bar")))
;;  "foobar")))

; $stringは、完全一致しないと、消費しない?
;; (P ($or ($string "abc")
;;         ($string "aef")
;;         )
;;    "aef")
;; (P ($or ($seq ($char #\a) ($char #\b))
;;         ($seq ($char #\a) ($char #\c))
;;         )
;;    "ab")
   ; "ac")  ; error

;; (P ($or ($try ($seq ($char #\a) ($char #\b)))
;;         ($seq ($char #\a) ($char #\c))
;;         )
;;    "ac")

; (pps ($do [x ($string "a")] [y ($string "b")] ($return (cons x y)) ) "ab")
; $lift は、パースの結果を使って、結果を新たに返す
;(pps ($lift cons ($string "a") ($string "b")) "ab")

; #[^a-b]
; (char-set-complement #[a-b])

;; (define-macro (rva a)
;;   `(recieve ret ,a ret))

; (define-values (a b . c) (values 1 2 3 4))

(define %ws ($skip-many ($one-of #[ \t\r\n])))
; lazyしないと、内側で定義している%pがないと、言われる
; (define %w ($lazy ($lift (^[v _] v) ($or %p %ws) %ws)))
; $liftは、引数そろえないと！($lift (^(a b c) Pa Pb Pc))
; (define %w ($lazy ($lift (^[v] v) ($or %p %ws))))
; (define %w ($lazy ($lift (^[v] v) ($or %p))))

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
                  ))
(define %c-type ($lift (^[t p] (let1 ts (rope->string t) (if p #"~ts *" #"~ts ")))
                       %c-types ($optional ($try ($seq %ws ($char #\*))))))

(define %c-signature ($lift
                      (^x (format "(~a)" (list->string x)))
                      ($between ($char #\()
                                ($many ($none-of #[()]))
                                ($char #\)))))
; (p (pps ($seq %c-f-body) "{a{b}c{d}e}"))


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

(define (sphinx sphinx-abspath :key (grammer %cc) (cd (current-directory)))
  (define filepath (if (#/^\// sphinx-abspath)
                       #"~|cd|~sphinx-abspath"
                       (f-join cd sphinx-abspath)))
  (define file-string (file->string filepath))
  ((pa$ generator-from-file filepath grammer)
   (^[alist]
     (let*-values (((body) (assoc-ref alist 'body))
                   ((start end) (string-line-range file-string body))
                   ((start) (- start 1))
                   ((lines) (cond ((= start -1) "")
                                  ((= end -1) #"~|start|-")
                                  (else #"~|start|-~|end|")))
                   )
       
       (acons 'literalinclude
              #"
.. literalinclude:: ~|sphinx-abspath|
   :language: c
   :lines: ~lines"
              alist))
     )))


(define (s-indent s :key (indent "    "))
  (let1 slist (if (pair? s) s (string-split s "\n"))
        (string-join (map (^x (string-append indent x)) slist) "\n")
        ))

(define (run-c path)
  (if (file-exists? path)
      (process-output->string-list #"clang ~path && ./a.out")
      ""))

(define (run-scheme path)
  (if (file-exists? path)
      (process-output->string-list #"gosh ~path")
      ""))

(define (get-run-process language)
  (cond ((equal? language "scheme") run-scheme)
        ((equal? language "c") run-c)
  ))

(define (shinx-section-test path :key (language "scheme"))
  (and-let* ((ok (file-exists? path))
             (file (s-indent (file->string path)))
             (content ((get-run-process language) path))
             (result (s-indent content))
             )
   #"
test
----

.. code-block:: ~language

~file

test result ::

~result
"))
