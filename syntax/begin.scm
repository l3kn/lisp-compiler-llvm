(defn begin? (expr) (tagged-list? expr 'begin))
(def begin-expressions cdr)

(defn make-begin (expressions)
  (cons 'begin expressions))

(defn emit-begin (var env expr)
  (def (helper lst)
    (cond ((null? lst)
           (error "Empty begin"))
          ((null? (cdr lst))
           (emit-expr var env (car lst)))
          (else
            (emit-expr (generate-var) env (car lst))
            (helper (cdr lst)))))
  (helper (begin-expressions expr)))

