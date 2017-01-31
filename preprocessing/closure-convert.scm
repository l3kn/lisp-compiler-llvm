(defn closure-convert-expr (expr)
      (closure-convert expr))

(def global-vars '())
(defn global-var-env ()
      (extend-env* global-vars
                   (map (fn (var) (~>> var escape (string-append "@var_")))
                        global-vars)
                   empty-env))
(defn register-global-var (name)
      (set! global-vars (cons name global-vars)))

(def lambdas '())
(defn register-lamdba (name body)
      (set! lambdas (cons (list name body) lambdas)))

(defn closure-convert (expr)
      (cond
        ; TODO: This doesn't work w/ nested lambdas
        ((fn? expr)
         (let* ((old-lambdas lambdas)
                (name (gensym))
                (arity (length (fn-params expr)))
                (free-vars (free expr))
                (replacements
                  (map-with-index (fn (v index) `(,v list-ref ,index __env))
                                  free-vars)))
           (~>> expr
                fn-body
                closure-convert
                (substitute replacements)
                (make-fn (cons '__env (fn-params expr)))
                (register-lamdba name))
           (list 'make-closure name arity (list->nested-cons_ free-vars))))
        ((begin? expr)
         (~>> expr
              begin-expressions
              (map closure-convert)
              make-begin))
        ((def? expr)
         (let ((name (def-name expr)))
           (register-global-var name)
           (make-def name (closure-convert (def-value expr)))))
        ((defprim? expr)
         (make-defprim (defprim-name expr)
                       (defprim-args expr)
                       (closure-convert (defprim-body expr))))
        ((assignment? expr)
         (make-def (assignment-name expr)
                   (closure-convert (assignment-value expr))))
        ((let? expr)
         (let* ((bindings (let-bindings expr))
                (body (let-body expr))
                (new-bindings
                  (map (fn (binding)
                           (~>> binding let-binding-value closure-convert
                                (list (let-binding-variable binding))))
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
           (set-union
             (set-subtract (free body) bound)
             (set-union*
               (map (fn (x) (~> x let-binding-value free))
                    bindings)))))
        ((and (symbol? expr) (not (assoc expr (global-var-env))))
         (singleton-set expr))
        ((quote? expr) (empty-set))
        ((list? expr) (~>> expr rst (map free) set-union*))
        (else (empty-set))))

(defn substitute (replacements expr)
      (cond
        ((fn? expr)
         ; TODO: Filter out bound vars from the replacements?
         (make-fn (fn-params expr)
                  (substitute replacements (fn-body expr))))
        ((symbol? expr)
         (lookup-or expr expr replacements))
        ((list? expr)
         (map (fn (x) (substitute replacements x)) expr))
        (else expr)))
