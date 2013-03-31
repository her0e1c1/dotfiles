;; license is GPL
;; This is a major mode for python
;; There is only one way to do it!!
;; why are there many modes for python?
;; this mode name is pythone 
;; python with e means python + one
;; I really hope there is only one way to write python for Emacs (and all IDEs :p)


;; TODO:
;; pyscrach 
;; M-: mode

(require 'ansi-color)
(require 'comint)

;;;###autoload
;(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'pythone-mode))

(defgroup pythone nil
  "Python Language's flying circus support for Emacs."
  :group 'languages
  :version "1.0"
  :link '(emacs-commentary-link "pythone"))


(defvar pythone-mode-map
 (let ((map (make-sparse-keymap)))
    ;key config
    (define-key map (kbd "C-q") 'pythone-test-cq)
    ;menu
    (easy-menu-define mypython-menu map "Python Mode menu"
      `("Python"
        :help "Python-specific"
        ["test" pythone-test-cq]))
    map)
 "Keymap for `pythone-mode'.")


(defvar pythone-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
          "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
          "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
          "try"
          ;; Python 2:
          "print" "exec"
          ;; Python 3:
          ;; False, None, and True are listed as keywords on the Python 3
          ;; documentation, but since they also qualify as constants they are
          ;; fontified like that in order to keep font-lock consistent between
          ;; Python versions.
          "nonlocal"
          ;; Extra:
          "self")
         symbol-end)
    ;; functions
    (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    ;; classes
    (,(rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    ;; Constants
    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end) . font-lock-constant-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
    (,(rx symbol-start
          (or
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
           "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError"
           "ImportError" "ImportWarning" "IndexError" "KeyError"
           "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
           "NotImplementedError" "OSError" "OverflowError"
           "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
           "RuntimeWarning" "StopIteration" "SyntaxError" "SyntaxWarning"
           "SystemError" "SystemExit" "TypeError" "UnboundLocalError"
           "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
           "UnicodeTranslateError" "UnicodeWarning" "UserWarning" "VMSError"
           "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
           ;; Python 2:
           "StandardError"
           ;; Python 3:
           "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
           "TabError")
          symbol-end) . font-lock-type-face)
    ;; Builtins
    (,(rx symbol-start
          (or
           "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
           "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
           "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
           "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
           "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
           "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
           "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
           "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
           "__import__"
           ;; Python 2:
           "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
           "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
           "intern"
           ;; Python 3:
           "ascii" "bytearray" "bytes" "exec"
           ;; Extra:
           "__all__" "__doc__" "__name__" "__package__")
          symbol-end) . font-lock-builtin-face)
    ;; assignments
    ;; support for a = b = c = 5
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_)))
                             (? ?\[ (+ (not (any  ?\]))) ?\]) (* space)
                             assignment-operator)))
          (when (re-search-forward re limit t)
            (while (and (python-syntax-context 'paren)
                        (re-search-forward re limit t)))
            (if (not (or (python-syntax-context 'paren)
                         (equal (char-after (point-marker)) ?=)))
                t
              (set-match-data nil)))))
     (1 font-lock-variable-name-face nil nil))
    ;; support for a, b, c = (1, 2, 3)
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                             (* ?, (* space) (+ (any word ?. ?_)) (* space))
                             ?, (* space) (+ (any word ?. ?_)) (* space)
                             assignment-operator)))
          (when (and (re-search-forward re limit t)
                     (goto-char (nth 3 (match-data))))
            (while (and (python-syntax-context 'paren)
                        (re-search-forward re limit t))
              (goto-char (nth 3 (match-data))))
            (if (not (python-syntax-context 'paren))
                t
              (set-match-data nil)))))
     (1 font-lock-variable-name-face nil nil))))




;;;

(defconst python-rx-constituents nil)

(defun pythone-test-cq ()
  (interactive)
  (message "hello"))



(defcustom pythone-indent-offset 4
  "Default indentation offset for Python."
  :group 'pythone
  :type 'integer
  :safe 'integerp)


(define-derived-mode pythone prog-mode "Pythone"
   (set (make-local-variable 'font-lock-defaults)
        '(pythone-font-lock-keywords nil nil nil nil))

 (set (make-local-variable 'tab-width) 8))


(provide 'pythone)

