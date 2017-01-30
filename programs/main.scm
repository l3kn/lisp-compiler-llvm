(defn error (message)
      (print "ERROR: ")
      (puts message))

(defn immediate? (x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(defn atomic? (x)
  (or (immediate? x) (symbol? x) (string? x))) 

(defn def? (expr) (tagged-list? expr 'def))
(defn def-name (expr) (frst expr))
(defn def-value (expr) (frrst expr))
(defn make-def (name value) (list 'def name value))

(defn defn? (expr) (tagged-list? expr 'defn))
(defn defn-name (expr) (frst expr))
(defn defn-args (expr) (frrst expr))
(defn defn-body (expr) (frrrst expr))
(defn defn-body_ (expr) (rrrst expr))
(defn make-defn (name args body) (list 'defn name args body))

(defn defprim? (expr) (tagged-list? expr 'defprim))
(defn defprim-name (expr) (frst expr))
(defn defprim-args (expr) (frrst expr))
(defn defprim-body (expr) (frrrst expr))
(defn defprim-body_ (expr) (rrrst expr))
(defn make-defprim (name args body) (list 'defprim name args body))

(defn fn? (expr) (tagged-list? expr 'fn))
(defn fn-params (expr) (frst expr))
(defn fn-body (expr) (frrst expr))
(defn fn-body_ (expr) (rrst expr))
(defn make-fn (params body)
      (list 'fn params body))

(defn assignment? (expr) (tagged-list? expr 'set!))
(defn assignment-name (expr) (frst expr))
(defn assignment-value (expr) (frrst expr))
(defn make-assignment (name value) (list 'set! name value))

(defn if? (expr) (tagged-list? expr 'if))
(defn if-test (expr) (frst expr))
(defn if-consequent (expr) (frrst expr))
(defn if-alternative (expr) (frrrst expr))

(defn make-if (test con alt) (list 'if test con alt))

(defn list_? (expr) (tagged-list? expr 'list))

(defn thread-first? (expr) (tagged-list? expr '~>))
(defn thread-last? (expr) (tagged-list? expr '~>>))

(defn cond? (expr) (tagged-list? expr 'cond))
(defn cond-clauses (expr) (rst expr))
(defn cond-clause-test (expr) (fst expr))
(defn cond-clause-action (clause) (frst clause))
(defn cond-clause-action_ (clause) (rst clause))

(defn or? (expr) (tagged-list? expr 'or))
(defn or-arguments (expr) (rst expr))

(defn and? (expr) (tagged-list? expr 'and))
(defn and-arguments (expr) (rst expr))

(defn let? (expr) (tagged-list? expr 'let))
(defn let-bindings (expr) (frst expr))
(defn let-body (expr) (frrst expr))
(defn let-body_ (expr) (rrst expr))

(defn let-binding-variable (expr) (fst expr))
(defn let-binding-value (expr) (frst expr))

(defn make-let (bindings body)
  (list 'let bindings body))

(defn let*? (expr) (tagged-list? expr 'let*))
(defn make-let* (bindings body)
  (list 'let* bindings body))

(defn begin? (expr) (tagged-list? expr 'begin))
(defn begin-expressions (expr) (rst expr))

(defn make-sequence (expressions)
  (cond ((null? expressions) expressions)
        ((null? (rst expressions)) (fst expressions))
        (else (make-begin expressions))))

(defn make-begin (expressions)
  (cons 'begin expressions))

(inspect (make-sequence (list 'foo)))

(defn syntax-desugar (expr)
      ; (print "synde " expr)
      (cond
        ((thread-first? expr)  (~> expr thread-first->nested-calls syntax-desugar))
        ((thread-last? expr)  (~> expr thread-last->nested-calls syntax-desugar))
        ((list_? expr) (~> expr list->nested-cons syntax-desugar))
        ((let*? expr)  (~> expr let*->nested-lets syntax-desugar))
        ((cond? expr)  (~> expr cond->nested-ifs syntax-desugar))
        ((and? expr)   (~> expr and->if syntax-desugar))
        ((or? expr)    (~> expr or->if syntax-desugar))
        ((defn? expr)  (~> expr defn->def-fn syntax-desugar))
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

(defn defn->def-fn (expr)
      (make-def (defn-name expr)
                (make-fn (defn-args expr)
                         (make-sequence (defn-body_ expr)))))

(defn list->nested-cons (expr)
      (list->nested-cons_ (rst expr)))
(defn list->nested-cons_ (elems)
      (if (null? elems)
        (list)
        (list 'cons (fst elems)
              (list->nested-cons_ (rst elems)))))

(defn thread-first->nested-calls (expr)
      (thread-first->nested-calls_ (frst expr) (rrst expr)))

(defn thread-last->nested-calls (expr)
      (thread-last->nested-calls_ (frst expr) (rrst expr)))

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
        ((null? clauses) (error "Empty cond"))
        ((null? (rst clauses)) (error "cond must have at least 2 branches"))
        ((null? (rrst clauses))
         (let ((first-clause (fst clauses))
               (second-clause (frst clauses)))
           (if (eq? 'else (cond-clause-test second-clause))
             (make-if (cond-clause-test first-clause)
                      (make-sequence (cond-clause-action_ first-clause))
                      (make-sequence (cond-clause-action_ second-clause)))
             (error "Last clause of cond must be else"))))
        (else
          (let ((first-clause (fst clauses))
                (rest-clauses (rst clauses)))
            (make-if (cond-clause-test first-clause)
                     (make-sequence (cond-clause-action_ first-clause))
                     (cond->nested-ifs_ rest-clauses))))))

(defn or->if (expr)
      (or->if_ (or-arguments expr)))

(defn or->if_ (expr)
  (if (null? expr)
    #f
    `(if ,(fst expr)
       #t
       ,(or->if_ (rst expr)))))

(defn and->if (expr)
      (and->if_ (and-arguments expr)))

(defn and->if_ (expr)
  (if (null? expr)
    #t
    `(if ,(fst expr)
       ,(and->if_ (rst expr))
       #f)))

(inspect (syntax-desugar '(~> x add1 add1)))
; (inspect (syntax-desugar '(~> x add1 add1)))

