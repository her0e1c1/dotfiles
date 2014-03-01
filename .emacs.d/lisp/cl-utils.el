;cl
(defmacro mac (expr)
  `(print (macroexpand ',expr)))

(defmacro nil!(x)
  `(setf ,x nil))
