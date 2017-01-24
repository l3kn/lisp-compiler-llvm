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

(define-syntax pipe
  (syntax-rules ()
    [(_ x)
     x]
    [(_ x (s ss ...))
     (s x ss ...)]
    [(_ x y)
     (y x)]
    [(_ x y z ...)
     (pipe (pipe x y) z ...)]))

(define fst car)
(define rst cdr)

(define ffst caar)
(define frst cadr)
(define rfst cdar)
(define rrst cddr)

(define fffst caaar)
(define ffrst caadr)
(define frfst cadar)
(define frrst caddr)
(define rffst cdaar)
(define rfrst cdadr)
(define rrfst cddar)
(define rrrst cdddr)

(define ffffst caaaar)
(define fffrst caaadr)
(define ffrfst caadar)
(define ffrrst caaddr)
(define frffst cadaar)
(define frfrst cadadr)
(define frrfst caddar)
(define frrrst cadddr)

(define rfffst cdaaar)
(define rffrst cdaadr)
(define rfrfst cdadar)
(define rfrrst cdaddr)
(define rrffst cddaar)
(define rrfrst cddadr)
(define rrrfst cdddar)
(define rrrrst cddddr)

(define fixnum? number?)
