(defn syntax-desugar (expr)
  (cond
    ((let? expr)
     (let* ((bindings (let-bindings expr))
            (body (let-body expr))
            (new-bindings
              (map (fn (binding)
                     (list (let-binding-variable binding)
                           (syntax-desugar (let-binding-value binding))))
                   bindings))
            (new-body (syntax-desugar body)))
       (make-let new-bindings
                 new-body)))
    ((let*? expr)
     (syntax-desugar (let*->nested-lets expr)))
    ((cond? expr)
     (syntax-desugar (cond->nested-ifs expr)))
    ((begin? expr)
     (make-sequence
       (map syntax-desugar (begin-expressions expr))))
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            ; TODO:
            ; We can't use `defn-body` here
            ; because it does some conversions
            (body (rrrst expr)))
       (make-defn name args (map syntax-desugar body))))
    ((list? expr)
     (map syntax-desugar expr))
    ((atomic? expr) expr)
    (else
      (error "Can not desugar expr: " expr))))
