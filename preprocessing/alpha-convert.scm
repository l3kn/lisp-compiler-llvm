(define (alpha-convert-expr expr)
  (alpha-convert expr empty-env))

(define (alpha-convert expr env)
  (cond
    ((let? expr)
     (let* ((bindings (let-bindings expr))
            (body (let-body expr))
            (new-env
              (extend-env*
                (map let-binding-variable bindings)
                (map (lambda (b) (gensym)) bindings)
                env))
            (new-bindings
              (map (lambda (binding)
                     (list (lookup (let-binding-variable binding) new-env)
                           (alpha-convert (let-binding-value binding)
                                          env)))
                   bindings))
            (new-body (alpha-convert body new-env)))
       (make-let new-bindings
                 new-body)))
    ((list? expr)
     (map (lambda (expr) (alpha-convert expr env))
          expr))
    ((variable? expr)
     ; If a variable doesn't apper in the env,
     ; just assume it to be a primitive function
     (lookup-or expr expr env))
    ((atomic? expr) expr)
    (else
      (error "Can not normalize expr: " expr))))
