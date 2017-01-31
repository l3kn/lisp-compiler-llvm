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
(define puts print)

(defn join (sep lst)
  (cond
    ((null? lst) "")
    ((null? (rst lst)) (any->string (fst lst)))
    (else (string-append
            (string-append (any->string (fst lst)) sep)
            (join sep (rst lst))))))

(def any->string ->string)
(def fixnum->string number->string)

(defn id (x) x)

(defn char->string (char) (list->string (list char)))
(defn char->fixnum (char) (char->integer char))

(defn filter (pred lst)
         (cond ((null? lst) (list))
               ((eq? (pred (fst lst)) #t)
                (cons (fst lst)
                      (filter pred (rst lst))))
               (else (filter pred (rst lst)))))

(defn member? (var lst)
         (cond ((null? lst) #f)
               ((eq? var (fst lst)) #t)
               (else (member? var (rst lst)))))

(defn format (str vars) (format_ str vars 0 (string-length str) ""))

; TODO: This will break if there are not enough vars
(defn format_ (str vars idx len res)
      (cond
        ((eq? idx len) res)
        ((and (fx<=? idx (fx- len 2))
              (eq? (string-ref str idx) #\~)
              (eq? (string-ref str (fxadd1 idx)) #\A))
         (format_ str (rst vars) (fx+ idx 2) len
                  (string-append
                    res
                    (any->string (fst vars)))))
        (else
          (format_ str vars (fxadd1 idx) len
                   (string-append
                     res
                     (char->string (string-ref str idx)))))))

(def fx+ +)
(def fx<? <)
(def fx<=? <=)
(def fxadd1 add1)
