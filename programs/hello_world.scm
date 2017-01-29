(def var-counter 0)
(defn generate-var ()
  (set! var-counter (fxadd1 var-counter))
  (string-append "%tmp"
                 (fixnum->string var-counter)))

(print (generate-var))
(print (generate-var))

; (defn map (f lst)
;       (if (null? lst)
;         lst
;         (cons (f (fst lst))
;               (map f (rst lst)))))

; (defn fib (n)
;       (if (fx<=? n 1)
;           n
;           (fx+ (fib (fx- n 1))
;                (fib (fx- n 2)))))

; (inspect fib)
; ; (print "hello world")

; (inspect (map fib (list 1 2 3 4)))

; (puts "test")
; (inspect (fib 10))
; (puts "test")
; ; (print (char->string (fixnum->char (fib 10))))
; (puts "test")
