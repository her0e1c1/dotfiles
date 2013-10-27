
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; load module
(ql:quickload 'anaphora)
(use-package 'anaphora)

;;; lambdaの代わりにfnを使う
(let ( (r (copy-readtable nil)) )
  (defun read-symbol (stream)
    (let ( (*readtable* r) )
      (read-preserving-whitespace stream))))

(defun symbol-reader-macro-reader (stream char)
  (unread-char char stream)
  (let* ((s (read-symbol stream))
         (f (get s 'symbol-reader-macro)))
      (if f (funcall f stream s) s)))

(map nil (lambda (c)
           (set-macro-character c 'symbol-reader-macro-reader t))
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghipqrstuvwxyz_")

(defun set-macro-symbol (symbol readfn)
  (setf (get symbol 'symbol-reader-macro) readfn)
  t)

(set-macro-symbol 'fn
                  (lambda (stream symbol)
                    (declare (ignore stream symbol))
                    'cl:lambda))


;;; utility

(defmacro p(&rest rest)
  `(pprint ,@rest))

(defmacro m(expr)
  `(pprint (macroexpand-1 ',expr)))

