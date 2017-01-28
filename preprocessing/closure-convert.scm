(defn closure-convert-expr (expr)
      (closure-convert expr))

(def lambdas '())
(def global-vars '())

(defn register-lamdba (name body)
      (let ((old-lambdas lambdas))
        (set! lambdas
          (cons (list name body)
                old-lambdas))))

(defn register-global-var (name)
      (let ((old-global-vars global-vars))
        (set! global-vars
          (cons name old-global-vars))))

(defn closure-convert (expr)
      (cond
        ; TODO: This doesn't work w/ nested lambdas
        ((fn? expr)
         (let* ((old-lambdas lambdas)
                (name (gensym))
                (arity (length (fn-params expr)))
                (free-vars (free expr))
                (replacements
                  (map-with-index (fn (v index)
                                      (list v
                                            `(list-ref ,index env)))
                                  0
                                  free-vars)))
           (let ((new-body
                   (substitute replacements (fn-body expr))))
             (register-lamdba name
                              (make-fn (cons 'env (fn-params expr))
                                       new-body))
             (list 'make-closure name arity (list->nested-cons_ free-vars)))))
        ((def? expr)
         (let ((name (frst expr))
               (value (frrst expr)))
           (register-global-var name)
           (make-def name (closure-convert value))))
        ((assignment? expr)
         (make-def (assignment-name expr)
                   (closure-convert (assignment-value expr))))
        ((let? expr)
         (let* ((bindings (let-bindings expr))
                (body (let-body expr))
                (new-bindings
                  (map (fn (binding)
                           (list (let-binding-variable binding)
                                 (closure-convert (let-binding-value binding))))
                       bindings)))
           (make-let new-bindings (closure-convert body))))
        ((list? expr)
         (map closure-convert expr))
        ((variable? expr) expr)
        ((atomic? expr) expr)
        (else
          (error "Can not closure convert expr: " expr))))


(defn free (expr)
      (cond
        ((fn? expr)
         (set-subtract (free (fn-body expr)) (fn-params expr)))
        ((let? expr)
         (let* ((bindings (let-bindings expr))
                (body (let-body expr))
                (bound (map let-binding-variable bindings)))
           (set-union*
             (cons 
               (set-subtract (free body) bound)
               (map (fn (x) (~> x let-binding-value free))
                    bindings)))))
        ((and (symbol? expr) (not (primitive? expr)))
         (set expr))
        ((list? expr)
         ; (set-union* (map free expr)))
         (if (null? expr)
           (empty-set)
           (set-union* (map free (rst expr)))))
        (else (empty-set))))

(defn substitute (replacements expr)
      (cond
        ((fn? expr)
         ; TODO: Filter out bound vars from the replacements?
         (make-fn (fn-params expr)
                  (substitute replacements (fn-body expr))))
        ((and (symbol? expr) (not (primitive? expr)))
         (lookup-or expr expr replacements))
        ((list? expr)
         (map (fn (x) (substitute replacements x)) expr))
        (else expr)))
