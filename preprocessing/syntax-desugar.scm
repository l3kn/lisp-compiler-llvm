(defn syntax-desugar (expr)
      ; (print "synde " expr)
      (cond
        ((pipe? expr)  (~> expr pipe->nested-calls syntax-desugar))
        ((list_? expr) (~> expr list->nested-cons syntax-desugar))
        ((let*? expr)  (~> expr let*->nested-lets syntax-desugar))
        ((cond? expr)  (~> expr cond->nested-ifs syntax-desugar))
        ((and? expr)   (~> expr and->if syntax-desugar))
        ((or? expr)    (~> expr or->if syntax-desugar))
        ((defn? expr)  (~> expr def->def-fn syntax-desugar))
        ((let? expr)
         (let* ((bindings (let-bindings expr))
                (new-bindings
                  (map (fn (binding)
                           (list (let-binding-variable binding)
                                 (syntax-desugar (let-binding-value binding))))
                       bindings)))
           (make-let new-bindings
                     (~> expr
                         let-body_
                         make-sequence
                         syntax-desugar))))
        ((begin? expr)
         (let ((expressions (begin-expressions expr)))
           (if (null? (rst expressions))
               (fst expressions)
               (~>> expressions
                    (map syntax-desugar)
                    make-begin))))

        ; ((begin? expr)
        ;  (cons 'begin
        ;  (~>> expr
        ;       begin-expressions
        ;       (map syntax-desugar))))
        ((def? expr)
         (make-def (def-name expr)
                   (syntax-desugar (def-value expr))))
        ((defprim? expr)
         (make-defprim (defprim-name expr)
                       (defprim-args expr)
                       (~> expr
                           defprim-body_
                           make-sequence
                           syntax-desugar)))
        ((fn? expr)
         (make-fn (fn-params expr)
                  (~> expr
                      fn-body_
                      make-sequence
                      syntax-desugar)))
        ((assignment? expr)
         (make-assignment (assignment-name expr)
                          (syntax-desugar (assignment-value expr))))
        ((list? expr)
         (map syntax-desugar expr))
        ((atomic? expr) expr)
        (else
          (error "Can not desugar expr: " expr))))

(defn let*->nested-lets (expr)
      (let* ((bindings (let-bindings expr))
             (body (make-sequence (let-body_ expr))))
        (if (null? bindings)
          body
          (make-let (list (fst bindings))
                    (~> bindings
                        rst
                        (make-let* body)
                        let*->nested-lets)))))

(defn def->def-fn (expr)
      (make-def (defn-name expr)
                (make-fn (defn-args expr) (defn-body expr))))

(defn list->nested-cons (expr)
      (list->nested-cons_ (rst expr)))
(defn list->nested-cons_ (elems)
      (if (null? elems)
        '()
        (list 'cons (fst elems)
              (list->nested-cons_ (rst elems)))))

(defn pipe->nested-calls (expr)
      (pipe->nested-calls_ (frst expr)
                           (rrst expr)))
(defn pipe->nested-calls_ (var fns)
      (if (null? fns)
        var
        (pipe->nested-calls_
          (list (fst fns) var)
          (rst fns))))

(defn cond->nested-ifs (expr)
      (defn helper (clauses)
            (cond
              ((null? clauses)
               (error "Empty cond: " expr))
              ((null? (rst clauses))
               (error "cond must have at least 2 branches: " expr))
              ((null? (rrst clauses))
               (let ((first-clause (fst clauses))
                     (second-clause (frst clauses)))
                 (if (eq? 'else (cond-clause-test second-clause))
                   (make-if (cond-clause-test first-clause)
                            (make-sequence (cond-clause-action_ first-clause))
                            (make-sequence (cond-clause-action_ second-clause)))
                   (error "Last clause of cond must be else: " expr))))
              (else
                (let ((first-clause (fst clauses))
                      (rest-clauses (rst clauses)))
                  (make-if (cond-clause-test first-clause)
                           (make-sequence (cond-clause-action_ first-clause))
                           (helper rest-clauses))))))
      (helper (cond-clauses expr)))

(defn or->if (expr)
      (define (loop expr)
        (if (null? expr)
          #f
          `(if ,(fst expr)
             #t
             ,(loop (rst expr)))))
      (loop (or-arguments expr)))

(defn and->if (expr)
      (define (loop expr)
        (if (null? expr)
          #t
          `(if ,(fst expr)
             ,(loop (rst expr))
             #f)))
      (loop (and-arguments expr)))
