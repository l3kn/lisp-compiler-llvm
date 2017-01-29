(print "test")

; (defn emit-alloca (var)
;   (print (string-append* (list "  " var " = alloca i64"))))

; (defn emit-store (value in)
;   (print (string-append* (list "  store i64 " (any->string value) ", i64* " in))))

; (defn emit-load (var from)
;   (print (string-append* (list "  " var " = load i64, i64* " from))))

; (defn emit-label (name)
;   (print (string-append name ":")))

; (defn emit-br1 (label)
;   (print (string-append "  br label %" label)))

; (defn emit-ret (val)
;   (print (string-append "  ret i64 " val)))

; (defn emit-call0 (var name)
;   (print (string-append* (list "  " var " = call i64 " name "()"))))

; (defn emit-call1 (var name arg)
;   (print (string-append* (list "  " var " = call i64 " name "(i64 " arg ")"))))

; (defn emit-call2 (var name arg1 arg2)
;   (print (string-append* (list "  " var " = call i64 " name "(i64 " arg1 ", i64 " arg2 ")"))))

; (defn emit-call3 (var name arg1 arg2 arg3)
;   (print (string-append* (list "  " var " = call i64 " name "(i64 " arg1 ", i64 " arg2 ", i64 " arg3 ")"))))

; (defn arg-str (arity)
;   (cond
;     ((eq? arity 0) "")
;     ((eq? arity 1) "i64")
;     (else
;       (string-append "i64, "
;                      (arg-str (sub1 arity))))))

; (def var-counter 0)
; ; (defn generate-var ()
; ;   (begin
; ;     (set! var-counter (add1 var-counter))
; ;     (string-append "%tmp"
; ;                    (fixnum->string var-counter))))

; (inspect var-counter)
; (set! var-counter (add1 var-counter))
; ; (begin
; ;   (set! var-counter (add1 var-counter))
; ;   (set! var-counter (add1 var-counter))
; ; )
; (inspect var-counter)

; ; (newline)
; ; (print (generate-var))
; ; (newline)
; ; (print (generate-var))
