(include "compile.scm")


(define (normalize-term expr) (normalize expr (lambda (x) x)))

(define (normalize expr k)
  (cond
    ; TODO: Add support for lambads
    ; ((lambda? expr) ...)
    ((let? expr)
     (let ((bindings (let-bindings expr))
           (body (let-body expr)))
       (
    ; This is handled just fine by the clause below
    ; ((if? expr)
    ;  ...)
    ((list? expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (normalize-name op
         (lambda (t)
                 (normalize-name* args
                   (lambda (t*)
                     (k (cons t t*))))))))
    ((atomic? expr) (k expr))
    (else
      (error "Can not normalize expr: " expr))))

(define (normalize-name m k)
  (normalize m (lambda (n)
                 (if (atomic? n)
                     (k n)
                     (let ((t (gensym)))
                          (make-let (list (list t n))
                                    (list (k t))))))))

(define (normalize-name* m* k)
  (if (null? m*)
      (k '())
      (normalize-name (car m*)
        (lambda (t)
                (normalize-name* (cdr m*)
                  (lambda (t*) (k (cons t t*))))))))

(print (normalize-term
         '(if (= 1 1) (+ 1 2) (+ 3 4))
         ))
         ; '(+ (+ 1 2) (+ 3 4))))
