(defn let? (expr) (tagged-list? expr 'let))
(def let-bindings frst)
(def (let-body expr)
     (make-sequence (rrst expr)))

(def let-binding-variable fst)
(def let-binding-value frst)

(defn make-let (bindings body)
  (list 'let bindings body))

(defn emit-let (var env expr)
  (process-let var expr (let-bindings expr) env))

(defn process-let (var expr bindings new-env)
  (if (null? bindings)
      (emit-expr var new-env (let-body expr))
      (let ((b (fst bindings))
            (var_ (generate-var)))
        ; It's ok to use new-env instead of env here,
        ; because all variable conflicts were solved during
        ; the alpha-conversion step
        (emit-expr var_ new-env (let-binding-value b))
        (process-let var expr
                     (rst bindings)
                     (extend-env (let-binding-variable b) var_ new-env)))))
