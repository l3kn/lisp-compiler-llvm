(def empty-env (list))

(defn extend-env (var val env)
      (alist-cons var val env))

(defn extend-env* (vars vals env)
  (if (null? vars)
      env
      (extend-env* (rst vars) (rst vals)
                   (extend-env (fst vars) (fst vals) env))))

(defn lookup (var env)
  (let ((res (assoc var env)))
    (if res
        (rst res)
        (error "Trying to lookup unbound variable: " var))))

(defn lookup-or (var alt env)
  (let ((res (assoc var env)))
    (if res (rst res) alt)))
