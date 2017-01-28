(defn lambda? (expr)
      (tagged-list? expr 'fn))
(defn lambda-args (expr)
      (frst expr))
(defn lambda-body (expr)
      (make-sequence (rrst expr)))

(defn closure-convert-expr (expr)
  (closure-convert expr))

(def lambdas '())
(def global_vars '())

(defn closure-convert (expr)
  (cond
    ; TODO: This doesn't work w/ nested lambdas
    ((lambda? expr)
     (let* ((old-lambdas lambdas)
            (name (gensym))
            (arity (length (lambda-args expr)))
            (free-vars (free expr))
            (replacements
              (map-with-index (fn (v index)
                       (list v
                             `(list-ref ,index env)))
                              0
                              free-vars)))
       (let ((new-body
               (substitute replacements (lambda-body expr))))
         (set! lambdas (cons (list name (list 'fn (cons 'env (lambda-args expr))
                                                  new-body))
                             old-lambdas))
         (list 'make-closure name arity (list->nested-cons_ free-vars)))))
    ((tagged-list? expr 'def)
     (let ((name (frst expr))
           (value (frrst expr))
           (old-global_vars global_vars))
       (set! global_vars (cons name global_vars))
       (list 'def name (closure-convert value))))
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
    ((lambda? expr)
     (set-subtract (free (lambda-body expr)) (lambda-args expr)))
    ((let? expr)
     (let* ((bindings (let-bindings expr))
            (body (let-body expr))
            (bound (map let-binding-variable bindings)))
       (set-union*
         (cons 
           (set-subtract (free body) bound)
           (map (fn (x) (pipe x let-binding-value free))
                bindings)))))
    ((and (symbol? expr)
          (not (primitive? expr)))
     (set expr))
    ((list? expr)
     ; (set-union* (map free expr)))
     (if (null? expr)
         (empty-set)
         (set-union* (map free (rst expr)))))
    (else (empty-set))))

(defn substitute (replacements expr)
  (cond
    ((lambda? expr)
     ; TODO: Filter out bound vars from the replacements?
     (list 'lambda
           (lambda-args expr)
           (substitute replacements (lambda-body expr))))
    ((and (symbol? expr)
          (not (primitive? expr)))
     (let ((replacement (assoc expr replacements)))
       (if replacement
           (frst replacement)
           expr)))
    ((list? expr)
     (map (fn (x) (substitute replacements x))
          expr))
    (else expr)))
