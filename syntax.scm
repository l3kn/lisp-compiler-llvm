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

(defn pipe? (expr) (tagged-list? expr 'pipe))

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
