(define (syntax-desugar expr)
  (cond
    ((let? expr)
     (let* ((bindings (let-bindings expr))
            (body (let-body expr))
            (new-bindings
              (map (lambda (binding)
                     (list (let-binding-variable binding)
                           (syntax-desugar (let-binding-value binding))))
                   bindings))
            (new-body (syntax-desugar body)))
       (make-let new-bindings
                 new-body)))
    ((let*? expr)
     (syntax-desugar (let*->nested-lets expr)))
    ((begin? expr)
     (make-begin
       (map syntax-desugar (begin-expressions expr))))
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            ; TODO:
            ; We can't use `defn-body` here
            ; because it does some conversions
            (body (cdddr expr)))
       (make-defn name args (map syntax-desugar body))))
    ((list? expr)
     (map syntax-desugar expr))
    ((atomic? expr) expr)
    (else
      (error "Can not desugar expr: " expr))))
