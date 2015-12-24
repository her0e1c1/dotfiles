; カテゴリ 言語、型、処理
; 処理が同じでも、言語によって、名前が違う(length, string-length)
; ただ、polymorphismにして、データに対して同じ処理名を記述すればよいね!

; psを自動付加
; psにインクルード文追加
; (ps "msg" :inlucde)

; (group みたにして囲むべきかな(階層))
; warnを全てに設定するのはよいが、全てに出力したくないね (dataとしては持っていてよいのか)
; (run "" :msg :quote :warn :tags :zsh :dummy :language :args :stdin :file)

; file(aliasなし)/multi line(HEREDOC?) (改行があった場合とか)/oneliner/alias onliner

; /perl/node/py/gosh/c/cpp/ghc/java/ruby/sh

; (links 'ghc 'list)

(define (sphinx-section name :key (ch #\=) (up #f))
  (let* ((bar (make-string (string-length name) ch))
         (upbar (if up bar "")))
    #"
~upbar
~name
~bar
"))

(define (sphinx-section-test path :key (language #f))
  (define language (if (not language) language (path-extension path)))
  (and-let* ((ok (file-exists? path))
             (section (sphinx-section "test" :ch #\-))
             (file (sphinx-block (file->string path) :code-block language))
             (content ((get-run-process language) path))
             (result (sphinx-block content :block #t))
             )
   #"
~section
~file
~result
"))

(define (sphinx-warn msg)
  (let* ((indented (string-indent msg)))
  #"
.. warning::

~indented
"))

(define (sphinx-block-path path :key (linenos #f))
  (let1 code (path-extension path)
        (sphinx-block (file->string path) :code-block code :linenos linenos)))

(define (sphinx-block s :key (code-block #f) (block #f) (linenos #f) (toctree #f) (maxdepth #f))
  (define indented (s-indent s))
  (define arg-linenos (if linenos ":linenos:" ""))
  (cond
   ((string-null? indented) "")
   (block #"
::

~indented
")
   (code-block #"
.. code-block:: ~|code-block|
   ~arg-linenos

~indented
")
   (toctree (let* ((m (if maxdepth #":maxdepth: ~maxdepth" "")))
              #"
.. toctree::
    ~m

~indented
"))

;; .. literalinclude:: ~|sphinx-abspath|
;;    :language: c
;;    :lines: ~lines"
   )
)

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
       alist)
     )))

(define (sphinx-run-path-ruby path)
  (let* ((file (sphinx-block-path path))
         (rslt (sphinx-block (run-ruby path) :block #t)))
    #"~file \n~path => ~rslt"))

; この関数は2回目以降有効っぽいは(１回目ではrstファイルが生成されてない)
(define (sphinx-toctree-directory :optional (dir "."))
  (--> 
   ((flip$ filter-map) (--ls dir)
    (^x (and-let* ((_ (file-exists? #"~|x|/index.rst"))
                   (p #"~|x|/index"))
                  p)))
   (sort it)
   (sphinx-block (string-join it "\n") :toctree #t :maxdepth 1)))

       ;(sphinx-toctree :glob
(define (sphinx-toctree :key (maxdepth #f) (glob #f) (pattern #f) (path #f))
  (set! maxdepth (if maxdepth (string-indent #":maxdepth: ~maxdepth") ""))
  (cond (glob #"
.. toctree::
~maxdepth
    :glob:

    ~glob
")
        (path (let1 p (string-join (map string-indent (if (pair? path) path (list path)))
                                   "\n")
                    #"
.. toctree::
~maxdepth

~p
"))
        (else (error "No"))))
;;   (let* ((m (if maxdepth #":maxdepth: ~maxdepth" "")))
;;     #"
;; .. toctree::
;;     ~m

;; ~indented
;; "))

  ;; ((flip$ filter-map) (ls "langs")
  ;;  (^x (and-let* ((_ (file-exists? #"~|x|/index.rst"))
  ;;                 (p #"~|x|/index"))
  ;;                p)))

;;    (sphinx-block (string-join it "\n") :toctree #t :maxdepth 1)

;; ;;    (toctree (let* ((m (if maxdepth #":maxdepth: ~maxdepth" "")))
;;               #"
;; .. toctree::
;;     ~m

;; ~indented
;; "))

; TODO: create a c file if it is necessary

(define (sphinx-todo s) #".. todo:: ~s")
(define (sphinx-contents :key (depth #f) (label #f))
  (let* ((d (if (number? depth) (string-indent #":depth: ~|depth|") "")))
    #".. contents::
~d
"))

(define (sphinx-ext-scm->rst scm)
  (regexp-replace #/\.scm/ scm ".rst"))

; [Filepath] -> IO String
(define-method sphinx-include-scm-list ((scm-path-list <pair>))
  (for-each sphinx-scm->rst scm-path-list)
  (sphinx-toctree :path (map sphinx-ext-scm->rst scm-path-list)))

(define-method sphinx-include-scm-list ((scm-path-list <pair>) (output <string>) :key header)
  (sphinx-scm->rst scm-path-list output :header header)
  (sphinx-toctree :path output))

(define (sphinx-load path)
  (let1 abspath (abs path)
        (if (not (file-exists? abspath))
            (error #"ERROR: ~abspath doesn't exist")
            (guard (e (else (format (standard-error-port) #"ERROR: ~abspath => ~e")))
                   (print (sphinx-section (sys-basename path)))
                   (load abspath)))))

(define-method sphinx-scm->rst ((scm <string>))
  (let1 rst (sphinx-ext-scm->rst scm)
         (with-output-to-file rst
           (^() (sphinx-load scm)))))

(define-method sphinx-scm->rst ((scm-list <pair>) (output <string>) :key (header ""))
  (with-output-to-file output
    (^()
      (print header)
      (for-each sphinx-load scm-list))))

(define (sphinx-import-directory x :key (dir "s"))
  (let* ((it (join-line (list (sphinx-section x :up #t) (sphinx-contents :depth 2))))
         (files (glob #"./~|dir|/~|x|/*.scm")))
    (if (not (null? files))
        (print (sphinx-include-scm-list (sort files) #"~|x|.rst" :header it)))))

(define (sphinx-import-each-directory dir)
  (let1 dirs (map ($ sys-basename $) (glob #"~|dir|/*"))
        (for-each (^x (sphinx-import-directory x :dir dir)) dirs)))

(define (template$ str)
  (pa$ regexp-replace #/REPLACE/ str))

(define (template-map proc list template)
  (map (^x (proc ((template$ template) x))) list))

(define (language->command lang)
  (match (x->string lang)
         ("cpp" "cpe")
         ("c" "ce")
         ("node" "ne")
         ("py" "py")

         ("perl" "perl -E")
         ("php" "php -r")
         ("ruby" "ruby -e")
         ("ghc" "ghc -e")
         ("sh" "sh -c")
         ("zsh" "zsh -c")
         ))

(define (code->cmd code :key quote language argv)
  (let* ((esc (if (eq? quote #\')
                  (escape-single-quote code)
                  ; double quote でカコッた場合を記述
                  code))
         (quoted #"~|quote|~|esc|~|quote|")
         (cmd (language->command language)))
    #"~cmd ~quoted ~argv"))

(define (oneliner-run-str cmd :key language argv)
  (let1 ret (run-from-string cmd language argv)
        (format "~a~a"
                (sphinx-block #"~cmd" :code-block language)
                (sphinx-block #"~ret" :code-block "sh"))))

(define (oneliner-run-line cmd :key language quote argv)
  (let* ((line (if language (code->cmd cmd :quote quote :language language :argv argv) cmd))
         (ret (oneliner-run line)))
    (sphinx-block #"$ ~line\n~ret" :code-block "sh")))

; TODO: quoteを自動識別  
; TODO: expected追加
(define (oneliner-run+ cmd :key (msg #f) (warn #f) (quote #\') (language #f) (file #f) (str #f) (argv ""))
  (if msg (print msg))
  (if warn (print (sphinx-warn warn)))
  (print (cond (str (oneliner-run-str cmd :language language :argv argv))
               (else (oneliner-run-line cmd :language language :quote quote :argv argv)))))

; for typeless
(define ptodo ($ print $ sphinx-todo $))
(define ps ($ print $ sphinx-section $))
(define pw ($ print $ sphinx-warn $))
(define run oneliner-run+)

(define-macro (sphinx-setup-function name)
  `(define (,name code . rest) (apply run code :language ',name rest)))

(let1 langs '(c cpp node perl php ruby py ghc sh zsh)
      (eval-null `(begin ,@(map (^x `(sphinx-setup-function ,x)) langs))))
