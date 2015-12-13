
(define-macro (aif pred t . f)
  `(let ((it ,pred))
     (if it ,t ,@f)))

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

(define-macro (amap f ls)
  `(map (lambda (it) ,f) ,ls))

(define-macro (afilter f ls)
  `(filter (lambda (it) ,f) ,ls))

(define-macro (aeach f ls)
  `(for-each (lambda (it) ,f) ,ls))

; when unless if
(define-macro (fi a b :optional c)
  `(if ,a ,c ,b))
