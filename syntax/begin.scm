(defn emit-begin (var env expr)
  (emit-begin_ var env (begin-expressions expr)))

(defn emit-begin_ (var env lst)
  (cond ((null? lst) (error "Empty begin"))
        ((null? (rst lst)) (emit-expr var env (fst lst)))
        (else
          (emit-expr (generate-var) env (fst lst))
          (emit-begin_ var env (rst lst)))))
