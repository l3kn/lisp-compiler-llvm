(defn begin? (expr) (tagged-list? expr 'begin))
(def begin-expressions rst)

(defn make-sequence (expressions)
  (if (null? (rst expressions))
      (fst expressions)
      (make-begin expressions)))

(defn make-begin (expressions)
  (cons 'begin expressions))

(defn emit-begin (var env expr)
  (def (helper lst)
    (cond ((null? lst)
           (error "Empty begin"))
          ((null? (rst lst))
           (emit-expr var env (fst lst)))
          (else
            (emit-expr (generate-var) env (fst lst))
            (helper (rst lst)))))
  (helper (begin-expressions expr)))

