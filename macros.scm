(define-syntax fn
  (syntax-rules ()
    ((fn args body)
     (lambda args body))))

(define-syntax def
  (syntax-rules ()
    ((def var value)
     (define var value))))

(define-syntax defn
  (syntax-rules ()
    ((defn name args . body)
     (define name
       (lambda args . body)))))

