(def empty-env (list))

(defn extend-env (var val env)
  (cons (cons var val) env))

(defn extend-env* (vars vals env)
  (if (null? vars)
      env
      (extend-env* (rst vars) (rst vals)
                   (extend-env (fst vars) (fst vals) env))))

(def binding-variable fst)
(def binding-value rst)

(defn make-initial-env (vars vals)
  (def (loop vars vals env)
    (if (null? vars)
        env
        (loop
          (rst vars)
          (rst vals)
          (extend-env (fst vars) (fst vals) env))))
  (loop vars vals empty-env))

(defn lookup (var env)
  (cond
    ((null? env)
     (error "Trying to lookup unbound variable: " var))
    ((eq? (binding-variable (fst env)) var)
     (binding-value (fst env)))
    (else
      (lookup var (rst env)))))

(defn lookup-or (var alt env)
  (cond
    ((null? env)
     alt)
    ((eq? (binding-variable (fst env)) var)
     (binding-value (fst env)))
    (else
      (lookup-or var alt (rst env)))))
