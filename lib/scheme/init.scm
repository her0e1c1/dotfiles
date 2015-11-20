(use srfi-13)  ; string-null? string-every
(use srfi-14)  ; char-set:whitespace

(use srfi-27)  ; random
(use srfi-42)  ; range

(use util.stream)
(use util.list)
(use text.tr)
(use file.util)
(use gauche.process)

; s '($ p $ + 1 2 3)' => 6
(define p print)
(define l lambda)
(define pp (pa$ print))
(define pos process-output->string)
(define IN (standard-input-port))
(define OUT (standard-output-port))
(define ERR (standard-error-port))

; s '(p (string-slices "abcd" 2))' => (ab cd)
(define (string-slices str len)
    (map list->string (slices (string->list str) len)))

;; anaforic
(define-macro (aif pred t . f)
  `(let ((it ,pred))
     (if it ,t ,@f)))

(define-syntax if-let1
  (syntax-rules ()
    ((_ var test then else)
          (or (and-let* ((var test)) then) else))))
; (aif (+ 1 2 3) (* it 10) 1)
; (if-let1 it (+ 1 2 3) (* it 10) 1)  ; 60

; combinator
(define (~$ . selectors)
  (cut apply ~ <> selectors))

(define (slot-ref*$ . selectors)
  (cut apply slot-ref* <> selectors))

(define (flip f x y) (f y x))

