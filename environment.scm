(define empty-env '())

(define (extend-env var val env)
  (cons (cons var val) env))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars) (cdr vals)
                   (extend-env (car vars) (car vals) env))))

(define binding-variable car)
(define binding-value cdr)

(define (make-initial-env vars vals)
  (define (loop vars vals env)
    (if (null? vars)
        env
        (loop
          (cdr vars)
          (cdr vals)
          (extend-env (car vars) (car vals) env))))
  (loop vars vals empty-env))

(define (lookup var env)
  (cond
    ((null? env)
     (error "Trying to lookup unbound variable: " var))
    ((eq? (binding-variable (car env)) var)
     (binding-value (car env)))
    (else
      (lookup var (cdr env)))))

(define (lookup-or var alt env)
  (cond
    ((null? env)
     alt)
    ((eq? (binding-variable (car env)) var)
     (binding-value (car env)))
    (else
      (lookup-or var alt (cdr env)))))
