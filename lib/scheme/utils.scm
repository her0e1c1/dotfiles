
; (clear)
(define clear
  (let1 c (process-output->string '("clear"))
        (lambda () (display c))))

(define (create-file-from-string filepath string)
  (with-output-to-file (x->string filepath)
    (lambda () (print string))))


;; def to_human(num):
;;     for i, unit in enumerate(["", "K", "M", "G", "T", "P", "Z"]):
;;         min = 2 ** (i * 10)
;;         max = 2 ** ((i + 1) * 10)
;;         if min <= num < max:
;;             return "%s %s bite (= %s)" % (num // min, unit, num)
;;     else:
;;         return "%s bite" % num
