(define (let? expr) (tagged-list? expr 'let))
(define let-bindings cadr)
(define let-body caddr)

(define let-binding-value cadr)
(define let-binding-variable car)

(define (make-let bindings body)
  (list 'let bindings body))

(define (emit-let var env expr)
  (define (process-let bindings new-env)
    (cond
      ((null? bindings)
       (emit-expr var new-env (let-body expr)))
      (else
        (let ((b (car bindings))
              (var_ (generate-var)))
          (emit-expr var_ env (let-binding-value b))
          (process-let
            (cdr bindings)
            (extend-env (let-binding-variable b) var_ new-env))))))
  (process-let (let-bindings expr) env))

(define (let*? expr) (tagged-list? expr 'let*))

(define (let*->nested-lets expr)
  (let* ((bindings (let-bindings expr))
         (body (let-body expr)))
    (if (null? bindings)
        body
        (make-let (list (car bindings))
                  (let*->nested-lets
                    (make-let* (cdr bindings)
                               body))))))


(define (make-let* bindings body)
  (list 'let* bindings body))

; (define (emit-let* var env expr)
;   (define (process-let bindings new-env)
;     (cond
;       ((null? bindings)
;        (emit-expr var new-env (let-body expr)))
;       (else
;         (let ((b (car bindings))
;               (var_ (generate-var)))
;           (emit-expr var_ new-env (let-binding-value b))
;           (process-let
;             (cdr bindings)
;             (extend-env (let-binding-variable b) var_ new-env))))))
;   (process-let (let-bindings expr) env))
