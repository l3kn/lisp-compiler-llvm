(define (normalize-term expr) (normalize expr (lambda (x) x)))

(define (normalize expr k)
  (cond
    ((atomic? expr) (k expr))
    ; TODO: Add support for lambads
    ; ((lambda? expr) ...)
    ((let? expr)
     ; NOTE: This is only possible,
     ; because this step runs after the alpha-conversion.
     ; Otherwise "unrolling" the let bindings
     ; would result in "let" behaving like "let*"
     (let ((bindings (let-bindings expr))
           (body (let-body expr)))
       (if (null? bindings)
           (normalize body k)
           (let ((first-binding (car bindings))
                 (rest-bindings (cdr bindings)))
             (normalize (let-binding-value first-binding)
                        (lambda (val)
                          (make-let (list (list (let-binding-variable first-binding) val))
                                    (normalize (make-let rest-bindings body) k))))))))
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            ; TODO:
            ; We can't use `defn-body` here
            ; because it does some conversions
            (body (cdddr expr)))
       (k (make-defn name args (map normalize-term body)))))
    ((if? expr)
     (normalize-name (if-test expr)
                     (lambda (t)
                       (k (make-if t (normalize-term (if-consequent expr))
                                     (normalize-term (if-alternative expr)))))))
    ((list? expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (normalize-name op
         (lambda (t)
                 (normalize-name* args
                   (lambda (t*)
                     (k (cons t t*))))))))
    (else
      (error "Can not normalize expr: " expr))))

(define (normalize-name m k)
  (normalize m (lambda (n)
                 (if (atomic? n)
                     (k n)
                     (let ((t (gensym)))
                          (make-let (list (list t n))
                                    (k t)))))))

(define (normalize-name* m* k)
  (if (null? m*)
      (k '())
      (normalize-name (car m*)
        (lambda (t)
                (normalize-name* (cdr m*)
                  (lambda (t*) (k (cons t t*))))))))

; (print (normalize-term
;          '(if (= 1 1) (+ 1 2) (+ 3 4))
;          ))
;          ; '(+ (+ 1 2) (+ 3 4))))
