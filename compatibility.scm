(use srfi-1)

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

(define-syntax ~>
  (syntax-rules ()
    [(~> x)
     x]
    [(~> x (s ss ...))
     (s x ss ...)]
    [(~> x y)
     (y x)]
    [(~> x y z ...)
     (~> (~> x y) z ...)]))

(define-syntax ~>>
  (syntax-rules ()
    [(~>> x)
     x]
    [(~>> x (s ss ...))
     (s ss ... x)]
    [(~>> x y)
     (y x)]
    [(~>> x y z ...)
     (~>> (~>> x y) z ...)]))

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

(defn string-join2 (lst sep)
  (cond
    ((null? lst) "")
    ((null? (rst lst)) (fst lst))
    (else (string-append
            (string-append (fst lst) sep)
            (string-join2 (rst lst) sep)))))

(def any->string ->string)
(def fixnum->string number->string)

(defn string-append* (lst)
      (if (null? lst)
          ""
          (string-append (fst lst) (string-append* (rst lst)))))

(defn id (x) x)
