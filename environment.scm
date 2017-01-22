(def empty-env '())

(defn extend-env (var val env)
  (cons (cons var val) env))

(defn extend-env* (vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars) (cdr vals)
                   (extend-env (car vars) (car vals) env))))

(def binding-variable car)
(def binding-value cdr)

(defn make-initial-env (vars vals)
  (def (loop vars vals env)
    (if (null? vars)
        env
        (loop
          (cdr vars)
          (cdr vals)
          (extend-env (car vars) (car vals) env))))
  (loop vars vals empty-env))

(defn lookup (var env)
  (cond
    ((null? env)
     (error "Trying to lookup unbound variable: " var))
    ((eq? (binding-variable (car env)) var)
     (binding-value (car env)))
    (else
      (lookup var (cdr env)))))

(defn lookup-or (var alt env)
  (cond
    ((null? env)
     alt)
    ((eq? (binding-variable (car env)) var)
     (binding-value (car env)))
    (else
      (lookup-or var alt (cdr env)))))
