(defn syntax-desugar (expr)
  (cond
    ((pipe? expr) (syntax-desugar (pipe->nested-calls expr)))
    ((list_? expr) (syntax-desugar (list->nested-cons expr)))
    ((let*? expr) (syntax-desugar (let*->nested-lets expr)))
    ((cond? expr) (syntax-desugar (cond->nested-ifs expr)))
    ((and? expr) (syntax-desugar (and->if expr)))
    ((or? expr) (syntax-desugar (or->if expr)))
    ((let? expr)
     (let* ((bindings (let-bindings expr))
            (body (let-body expr))
            (new-bindings
              (map (fn (binding)
                     (list (let-binding-variable binding)
                           (syntax-desugar (let-binding-value binding))))
                   bindings))
            (new-body (syntax-desugar body)))
       (make-let new-bindings
                 new-body)))
    ((begin? expr)
     (make-sequence
       (map syntax-desugar (begin-expressions expr))))
    ((tagged-list? expr 'def)
     (let ((name (frst expr))
           (value (frrst expr)))
       (list 'def name (syntax-desugar value))))
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            ; TODO:
            ; We can't use `defn-body` here
            ; because it does some conversions
            (body (defn-body expr)))
       (make-defn name args (map syntax-desugar body))))
    ((tagged-list? expr 'defn)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            ; TODO:
            ; We can't use `defn-body` here
            ; because it does some conversions
            (body (defn-body expr)))
       (syntax-desugar (list 'def name (list 'fn args body)))))
       ; (make-defn name args (map syntax-desugar body))))
    ((list? expr)
     (map syntax-desugar expr))
    ((atomic? expr) expr)
    (else
      (error "Can not desugar expr: " expr))))

(defn let*? (expr) (tagged-list? expr 'let*))
(defn let*->nested-lets (expr)
  (let* ((bindings (let-bindings expr))
         (body (let-body expr)))
    (if (null? bindings)
        body
        (make-let (list (fst bindings))
                  (let*->nested-lets
                    (make-let* (rst bindings)
                               body))))))
(defn make-let* (bindings body)
  (list 'let* bindings body))

(defn list_? (expr) (tagged-list? expr 'list))
(defn list->nested-cons (expr)
      (list->nested-cons_ (rst expr)))
(defn list->nested-cons_ (elems)
      (if (null? elems)
          '()
          (list 'cons (fst elems)
                      (list->nested-cons_ (rst elems)))))

(defn pipe? (expr) (tagged-list? expr 'pipe))
(defn pipe->nested-calls (expr)
      (pipe->nested-calls_ (frst expr)
                           (rrst expr)))
(defn pipe->nested-calls_ (var fns)
  (if (null? fns)
      var
      (pipe->nested-calls_
        (list (fst fns) var)
        (rst fns))))

(defn cond? (expr)
  (tagged-list? expr 'cond))

(def cond-clauses rst)
(def cond-clause-test fst)
(defn cond-clause-action (clause)
     (make-sequence (rst clause)))

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
                       (cond-clause-action first-clause)
                       (cond-clause-action second-clause))
              (error "Last clause of cond must be else: " expr))))
         (else
          (let ((first-clause (fst clauses))
                (rest-clauses (rst clauses)))
            (make-if (cond-clause-test first-clause)
                     (cond-clause-action first-clause)
                     (helper rest-clauses))))))
  (helper (cond-clauses expr)))

(define (or? expr) (tagged-list? expr 'or))
(define or-arguments rst)

(define (or->if expr)
  (define (loop expr)
    (if (null? expr)
        #f
        `(if ,(fst expr)
             #t
             ,(loop (rst expr)))))
  (loop (or-arguments expr)))


(define (and? expr) (tagged-list? expr 'and))
(define and-arguments rst)

(define (and->if expr)
  (define (loop expr)
    (if (null? expr)
        #t
        `(if ,(fst expr)
             ,(loop (rst expr))
             #f)))
  (loop (and-arguments expr)))
