(defn let? (expr) (tagged-list? expr 'let))
(def let-bindings frst)
(def (let-body expr)
     (make-begin (rrst expr)))

(def let-binding-variable fst)
(def let-binding-value frst)

(defn make-let (bindings body)
  (list 'let bindings body))

(defn emit-let (var env expr)
  (def (process-let bindings new-env)
    (cond
      ((null? bindings)
       (emit-expr var new-env (let-body expr)))
      (else
        (let ((b (fst bindings))
              (var_ (generate-var)))
          (emit-expr var_ env (let-binding-value b))
          (process-let
            (rst bindings)
            (extend-env (let-binding-variable b) var_ new-env))))))
  (process-let (let-bindings expr) env))

