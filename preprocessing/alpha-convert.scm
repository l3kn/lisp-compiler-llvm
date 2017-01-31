(defn alpha-convert (expr) (alpha-convert_ expr empty-env))
(defn alpha-convert_ (expr env)
  (cond
    ((let? expr)
     (let* ((bindings (let-bindings expr))
            (body (let-body expr))
            (new-env
              (extend-env*
                (map let-binding-variable bindings)
                (map (fn (b) (gensym)) bindings)
                env))
            (new-bindings
              (map (fn (binding)
                     (list (lookup (let-binding-variable binding) new-env)
                           (alpha-convert_ (let-binding-value binding) env)))
                   bindings))
            (new-body (alpha-convert_ body new-env)))
       (make-let new-bindings new-body)))
    ((list? expr)
     (map (fn (expr) (alpha-convert_ expr env))
          expr))
    ; If a variable doesn't appear in the env,
    ; just assume it to be a primitive function
    ((variable? expr) (lookup-or expr expr env))
    ((atomic? expr) expr)
    (else (error "Can not normalize expr: " expr))))
