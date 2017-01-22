(define (begin? expr) (tagged-list? expr 'begin))
(define begin-expressions cdr)

(define (make-begin expressions)
  (cons 'begin expressions))

(define (emit-begin var env expr)
  (define (helper lst)
    (cond ((null? lst)
           (error "Empty begin"))
          ((null? (cdr lst))
           (emit-expr var env (car lst)))
          (else
            (emit-expr (generate-var) env (car lst))
            (helper (cdr lst)))))
  (helper (begin-expressions expr)))

