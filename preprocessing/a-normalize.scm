(defn normalize-term (expr) (normalize expr (fn (x) x)))

(defn normalize (expr k)
  (cond
    ((atomic? expr) (k expr))
    ; TODO: Add support for lambads
    ; ((fn? expr) ...)
    ((let? expr)
     ; NOTE: This is only possible,
     ; because this step runs after the alpha-conversion.
     ; Otherwise "unrolling" the let bindings
     ; would result in "let" behaving like "let*"
     (let ((bindings (let-bindings expr))
           (body (let-body expr)))
       (if (null? bindings)
           (normalize body k)
           (let ((first-binding (fst bindings))
                 (rest-bindings (rst bindings)))
             (normalize (let-binding-value first-binding)
                        (fn (val)
                          (make-let (list (list (let-binding-variable first-binding) val))
                                    (normalize (make-let rest-bindings body) k))))))))
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            ; TODO:
            ; We can't use `defn-body` here
            ; because it does some conversions
            (body (rrrst expr)))
       (k (make-defn name args (map normalize-term body)))))
    ((if? expr)
     (normalize-name (if-test expr)
                     (fn (t)
                       (k (make-if t (normalize-term (if-consequent expr))
                                     (normalize-term (if-alternative expr)))))))
    ((list? expr)
     (let ((op (fst expr))
           (args (rst expr)))
       (normalize-name op
         (fn (t)
                 (normalize-name* args
                   (fn (t*)
                     (k (cons t t*))))))))
    (else
      (error "Can not normalize expr: " expr))))

(defn normalize-name (m k)
  (normalize m (fn (n)
                 (if (atomic? n)
                     (k n)
                     (let ((t (gensym)))
                          (make-let (list (list t n))
                                    (k t)))))))

(def (normalize-name* m* k)
  (if (null? m*)
      (k '())
      (normalize-name (fst m*)
        (fn (t)
                (normalize-name* (rst m*)
                  (fn (t*) (k (cons t t*))))))))

; (print (normalize-term
;          '(if (= 1 1) (+ 1 2) (+ 3 4))
;          ))
;          ; '(+ (+ 1 2) (+ 3 4))))
