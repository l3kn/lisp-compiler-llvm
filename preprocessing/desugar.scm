(defn desugar (expr)
      (cond
        ((thread-first? expr)  (~> expr thread-first->nested-calls desugar))
        ((thread-last? expr)  (~> expr thread-last->nested-calls desugar))
        ((list_? expr) (~> expr list->nested-cons desugar))
        ((let*? expr)  (~> expr let*->nested-lets desugar))
        ((cond? expr)  (~> expr cond->nested-ifs desugar))
        ((and? expr)   (~> expr and->if desugar))
        ((or? expr)    (~> expr or->if desugar))
        ((defn? expr)  (~> expr defn->def-fn desugar))
        ((quote? expr) (~> expr frst desugar-quote))
        ((quasiquote? expr) (~>> expr frst (desugar-quasiquote 1) desugar))
        ((let? expr)
         (let ((new-bindings
                 (map (fn (binding)
                          (list (let-binding-variable binding)
                                (desugar (let-binding-value binding))))
                      (let-bindings expr))))
           (make-let new-bindings
                     (~> expr let-body_ make-sequence desugar))))
        ((begin? expr)
         (~>> expr begin-expressions (map desugar) make-sequence))
        ((def? expr)
         (make-def (def-name expr)
                   (desugar (def-value expr))))
        ((defprim? expr)
         (make-defprim (defprim-name expr)
                       (defprim-args expr)
                       (~> expr defprim-body_ make-sequence desugar)))
        ((fn? expr)
         (make-fn (fn-params expr)
                  (~> expr fn-body_ make-sequence desugar)))
        ((assignment? expr)
         (make-assignment (assignment-name expr)
                          (desugar (assignment-value expr))))
        ((list? expr)
         (map desugar expr))
        ((atomic? expr) expr)
        (else
          (error "Can not desugar expr: " expr))))

(defn desugar-quote (expr)
      (cond
        ((pair? expr)
         `(cons ,(desugar-quote (fst expr))
                ,(desugar-quote (rst expr))))
        ((null? expr) '())
        ((symbol? expr) `(quote ,expr))
        ((immediate? expr) expr)
        ((string? expr) expr)
        (else
          (error "Strange value in quote: " expr))))

(defn desugar-quasiquote (level expr)
      (cond
        ((unquote? expr)
         (if (eq? level 1)
             (desugar (frst expr))
             (list 'list ''unquote
                   (desugar-quasiquote (sub1 level) (frst expr)))))
        ((quasiquote? expr)
         ; TODO: this does not desugar correctly
         ; `(list 'quasiquote ,(desugar-quasiquote (add1 level)
         ;                                         (frst expr))))
         (list 'list ''quasiquote
               (desugar-quasiquote (add1 level)
                                   (frst expr))))
        ((and (pair? expr) (unquote-splicing? (fst expr)))
         (if (eq? level 1)
             `(append ,(frfst expr) ,(desugar-quasiquote level (rst expr)))
             (cons (list 'unquote-splicing
                         (desugar-quasiquote (sub1 level) (frfst expr)))
                   (desugar-quasiquote level (frst expr)))))
        ((pair? expr)
         `(cons ,(desugar-quasiquote level (fst expr))
                ,(desugar-quasiquote level (rst expr))))
        (else (desugar-quote expr))))

(defn let*->nested-lets (expr)
      (let* ((bindings (let-bindings expr))
             (body (~> expr let-body_ make-sequence)))
        (if (null? bindings)
          body
          (make-let (list (fst bindings))
                    (~> bindings
                        rst
                        (make-let* body)
                        let*->nested-lets)))))

(defn defn->def-fn (expr)
      (~>> expr defn-body_ make-sequence
           (make-fn (defn-args expr))
           (make-def (defn-name expr))))

(defn list->nested-cons (expr) (list->nested-cons_ (rst expr)))
(defn list->nested-cons_ (elems)
      (if (null? elems)
        '()
        (list 'cons (fst elems)
              (list->nested-cons_ (rst elems)))))

(defn thread-first->nested-calls (expr) (thread-first->nested-calls_ (frst expr) (rrst expr)))
(defn thread-first->nested-calls_ (var fns)
      (if (null? fns)
          var
          (thread-first->nested-calls_
            (let ((fn_ (fst fns)))
              (if (list? fn_)
                  (cons (fst fn_)
                        (cons var
                              (rst fn_)))
                  (list fn_ var)))
            (rst fns))))

(defn thread-last->nested-calls (expr) (thread-last->nested-calls_ (frst expr) (rrst expr)))
(defn thread-last->nested-calls_ (var fns)
      (if (null? fns)
          var
          (thread-last->nested-calls_
            (let ((fn_ (fst fns)))
              (if (list? fn_)
                  (append fn_ (list var))
                  (list fn_ var)))
            (rst fns))))

(defn cond->nested-ifs (expr) (cond->nested-ifs_ (cond-clauses expr)))
(defn cond->nested-ifs_ (clauses)
      (cond
        ((null? clauses) (error "Empty cond" clauses))
        ((null? (rst clauses)) (error "cond must have at least 2 branches" clauses))
        ((null? (rrst clauses))
         (let ((first-clause (fst clauses))
               (second-clause (frst clauses)))
           (if (eq? 'else (cond-clause-test second-clause))
             (make-if (cond-clause-test first-clause)
                      (~> first-clause cond-clause-action_ make-sequence)
                      (~> second-clause cond-clause-action_ make-sequence))
             (error "Last clause of cond must be else" clauses))))
        (else
          (let ((first-clause (fst clauses))
                (rest-clauses (rst clauses)))
            (make-if (cond-clause-test first-clause)
                     (~> first-clause cond-clause-action_ make-sequence)
                     (cond->nested-ifs_ rest-clauses))))))

(defn or->if (expr) (or->if_ (or-arguments expr)))
(defn or->if_ (expr)
  (if (null? expr)
    #f
    `(if ,(fst expr) #t ,(or->if_ (rst expr)))))

(defn and->if (expr) (and->if_ (and-arguments expr)))
(defn and->if_ (expr)
  (if (null? expr)
    #t
    `(if ,(fst expr) ,(and->if_ (rst expr)) #f)))
