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

(def char->fixnum char->integer)
(def fixnum->char integer->char)
(def fixnum->string number->string)

(define fst car)
(define rst cdr)

; (define ffst caar)
; (define frst cadr)
; (define rfst cdar)
; (define rrst cddr)

; (define fffst caaar)
; (define ffrst caadr)
; (define frfst cadar)
; (define frrst caddr)
; (define rffst cdaar)
; (define rfrst cdadr)
; (define rrfst cddar)
; (define rrrst cdddr)

; (define ffffst caaaar)
; (define fffrst caaadr)
; (define ffrfst caadar)
; (define ffrrst caaddr)
; (define frffst cadaar)
; (define frfrst cadadr)
; (define frrfst caddar)
; (define frrrst cadddr)

; (define rfffst cdaaar)
; (define rffrst cdaadr)
; (define rfrfst cdadar)
; (define rfrrst cdaddr)
; (define rrffst cddaar)
; (define rrfrst cddadr)
; (define rrrfst cdddar)
; (define rrrrst cddddr)

(defn ffst (e) (fst (fst e)))
(defn frst (e) (fst (rst e)))
(defn rfst (e) (rst (fst e)))
(defn rrst (e) (rst (rst e)))

(defn fffst (e) (fst (fst (fst e))))
(defn ffrst (e) (fst (fst (rst e))))
(defn frfst (e) (fst (rst (fst e))))
(defn frrst (e) (fst (rst (rst e))))

(defn rffst (e) (rst (fst (fst e))))
(defn rfrst (e) (rst (fst (rst e))))
(defn rrfst (e) (rst (rst (fst e))))
(defn rrrst (e) (rst (rst (rst e))))

(defn ffffst (e) (fst (fst (fst (fst e)))))
(defn fffrst (e) (fst (fst (fst (rst e)))))
(defn ffrfst (e) (fst (fst (rst (fst e)))))
(defn ffrrst (e) (fst (fst (rst (rst e)))))
(defn frffst (e) (fst (rst (fst (fst e)))))
(defn frfrst (e) (fst (rst (fst (rst e)))))
(defn frrfst (e) (fst (rst (rst (fst e)))))
(defn frrrst (e) (fst (rst (rst (rst e)))))

(defn rfffst (e) (rst (fst (fst (fst e)))))
(defn rffrst (e) (rst (fst (fst (rst e)))))
(defn rfrfst (e) (rst (fst (rst (fst e)))))
(defn rfrrst (e) (rst (fst (rst (rst e)))))
(defn rrffst (e) (rst (rst (fst (fst e)))))
(defn rrfrst (e) (rst (rst (fst (rst e)))))
(defn rrrfst (e) (rst (rst (rst (fst e)))))
(defn rrrrst (e) (rst (rst (rst (rst e)))))

(define fixnum? number?)

(define print_ print)
(define puts print_)

(defn char->string (char) (list->string (list char)))

(def fx+ +)
(def fx<? <)
(def fx<=? <=)
(def fx>=? >=)
(def fxadd1 add1)
(def fxsub1 sub1)
(defn fxneg (n) (* n -1))
(def fxzero? zero?)
(def fxrem remainder)

(defn digit->string (dgt)
      (case dgt
        ((0) "0")
        ((1) "1")
        ((2) "2")
        ((3) "3")
        ((4) "4")
        ((5) "5")
        ((6) "6")
        ((7) "7")
        ((8) "8")
        ((9) "9")))

(def read-char_ read-char)

(defn getchar ()
      (let ((res (read-char_)))
        (if (eq? res #!eof)
            '()
            res)))


(defn fixnum->string_ (fx)
         (if (fxzero? fx)
           ""
           (string-append
             (fixnum->string_ (fx/ fx 10))
             (digit->string (fxrem fx 10)))))

(defn fixnum->string (fx)
         (cond
           ((fxzero? fx) "0")
           ((fx<? fx 0) (string-append "-" (fixnum->string_ (fx- 0 fx))))
           (else (fixnum->string_ fx))))

(defn string->fixnum (str)
         (let ((digit (string-ref str 0)))
           (cond
             ((eq? digit #\0) 0)
             ((eq? digit #\-)
              (fxneg (string->fixnum_ str 1 (string-length str) 0)))
             (else (string->fixnum_ str 0 (string-length str) 0)))))

(defn string->fixnum_ (str idx len acc)
         (if (eq? idx len)
             acc
             (let ((digit
                     (~> (string-ref str idx)
                         char->fixnum
                         (fx- 48))))
               (string->fixnum_ str (fxadd1 idx) len
                                (fx+ (fx* 10 acc)
                                     digit)))))

(defn string->list (str)
         (string->list_ str 0 (string-length str)))

(defn string->list_ (str idx len)
         (if (eq? idx len)
           '()
           (cons (string-ref str idx)
                 (string->list_ str (fxadd1 idx) len))))

(defn any->string (val)
         (cond
           ((eq? val #t) "#t")
           ((eq? val #f) "#f")
           ((null? val) "()")
           ((fixnum? val)
            (fixnum->string val))
           ((list? val) (format "(~A)" (list (join " " val))))
           ((pair? val)
            (pair->string val))
           ((char? val)
            (cond
              ((eq? val #\newline) "#\\newline")
              ((eq? val #\space) "#\\space")
              ((eq? val #\tab) "#\\tab")
              ((eq? val #\return) "#\\return")
              (else (string-append "#\\" (char->string val)))))
           ((string? val)
            val)
           ((symbol? val)
            (symbol->string val))
           ; ((closure? val)
           ;  (format "<closure/~A>" (list (closure-arity val))))
           (else "unknown")))

; (defn list->string (lst)
;          (if (null? lst)
;              ""
;              (string-append
;                (~> lst fst char->string)
;                (list->string (rst lst)))))

(defn pair->string (pair)
         (format "(~A . ~A)" (list (any->string (fst pair))
                                   (any->string (rst pair)))))

(defn inspect (val)
         (puts (inspect_ val)))
(defn inspect_ (val)
         (cond
           ((symbol? val) (string-append "'" (symbol->string val)))
           (else (any->string val))))

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

(defn puts (val)
         (print val))

(defn char-numeric? (char)
         (let ((ord (char->fixnum char)))
           (and (fx<=? 48 ord) (fx>=? 57 ord))))

(defn char-whitespace? (char)
         (or (eq? char #\space)
             (eq? char #\newline)))

(defn char-alphabetic? (char)
         (let ((ord (char->fixnum char)))
           (or (and (fx<=? 65 ord) (fx>=? 90 ord))
               (and (fx<=? 97 ord) (fx>=? 122 ord)))))

(defn char-special? (char)
         (or (eq? char #\<)
             (eq? char #\>)
             (eq? char #\=)
             (eq? char #\-)
             (eq? char #\+)
             (eq? char #\*)
             (eq? char #\?)
             (eq? char #\!)
             (eq? char #\~)
             (eq? char #\\)
             (eq? char #\/)
             (eq? char #\_)))


(defn map (f lst)
         (if (null? lst)
           lst
           (cons (f (fst lst))
                 (map f (rst lst)))))

(defn for-each (f lst)
         (if (null? lst)
           'done
           (begin
             (f (fst lst))
             (for-each f (rst lst)))))

(defn filter (pred lst)
         (cond ((null? lst) '())
               ((eq? (pred (fst lst)) #t)
                (cons (fst lst)
                      (filter pred (rst lst))))
               (else (filter pred (rst lst)))))

(defn alist-cons (var val alist)
         (cons (cons var val) alist))

(defn assoc (var alist)
         (cond ((null? alist) #f)
               ((eq? (ffst alist) var) (fst alist))
               (else (assoc var (rst alist)))))

(defn member? (var lst)
         (cond ((null? lst) #f)
               ((eq? var (fst lst)) #t)
               (else (member? var (rst lst)))))

(defn append (a b)
         (cond ((null? a) b)
               ((null? b) a)
               (else
                 (cons (fst a)
                       (append (rst a) b)))))

(defn join (sep lst)
         (cond
           ((null? lst) "")
           ((null? (rst lst)) (any->string (fst lst)))
           (else (string-append
                   (string-append (any->string (fst lst)) sep)
                   (join sep (rst lst))))))

(defn reverse_ (lst res)
         (if (null? lst)
             res
             (reverse_ (rst lst)
                       (cons (fst lst) res))))

(defn reverse (lst)
         (reverse_ lst '()))


(defn list? (lst)
         (if (pair? lst)
           (list?_ lst)
           #f))

(defn list?_ (pair)
         (cond
           ((null? (rst pair)) #t)
           ((not (pair? (rst pair))) #f)
           (else (list?_ (rst pair)))))

(defn length (lst)
         (if (null? lst)
           0
           (fx+ 1 (length (rst lst)))))

(defn tagged-list? (lst tag)
         (and (not (null? lst))
              (eq? (fst lst) tag)))

(defn error (msg val)
         (print "ERROR: ")
         (print msg)
         (print " ")
         (inspect val))
