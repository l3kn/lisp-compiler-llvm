(defn normalize-term (expr) (normalize expr id))

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
                              (make-let 
                                (~> first-binding
                                    let-binding-variable
                                    (list val)
                                    list)
                                (normalize (make-let rest-bindings body) k))))))))
        ((def? expr)
         (k (make-def (def-name expr)
                      (~> expr def-value normalize-term))))
        ((assignment? expr)
         (k (make-assignment (assignment-name expr)
                             (~> expr assignment-value normalize-term))))
        ((tagged-list? expr 'make-closure)
         (normalize-name (frrrst expr)
                         (fn (t)
                             (k (list 'make-closure (frst expr) (frrst expr) t)))))
        ; ((defn? expr)
        ;  (let* ((name (defn-name expr))
        ;         (args (defn-args expr))
        ;         ; TODO:
        ;         ; We can't use `defn-body` here
        ;         ; because it does some conversions
        ;         (body (rrrst expr)))
        ;    (k (make-defn (defn-name expr)
        ;                  (defn-args expr)
        ;                  (~>> expr
        ;                       rrrst
        ;                       (map normalize-term))))))
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
                                                (fn (t*) (k (cons t t*))))))))
        (else (error "Can not normalize expr: " expr))))

(defn normalize-name (m k)
      (normalize m (fn (n)
                       (if (or (immediate? n) (primitive? n))
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
