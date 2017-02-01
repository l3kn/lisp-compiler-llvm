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

(def fixnum->char integer->char)

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
(def fxsub1 sub1)
(defn fxneg (n)
      (* n -1))

(defn getchar ()
      (read-char))

(def cur-char #f)
(defn read-char_ ()
      (if (char? cur-char)
        (let ((old cur-char))
          (set! cur-char #f)
          old)
        (getchar)))
(defn peek-char ()
      (if (char? cur-char)
        cur-char
        (let ((new (getchar)))
          (set! cur-char new)
          new)))

(defn skip-chars (n)
      (if (eq? n 0)
          'done
          (begin
            (read-char_)
            (skip-chars (fxsub1 n)))))

(defn read-token ()
      (let ((first-char (read-char_)))
        (cond
          ((char-whitespace? first-char)
           (read-token))
          ((eq? first-char #\()
           'left-paren)
          ((eq? first-char #\))
           'right-paren)
          ((eq? first-char #\')
           'quote)
          ((eq? first-char #\`)
           'quasiquote)
          ((eq? first-char #\,)
           'unquote)
          ((eq? first-char #\")
           (read-string ""))
          ; TODO: support unquote splicing
          ((eq? first-char #\#)
           (let ((next-char (read-char_)))
             (cond
               ((eq? next-char #\\) (read-character))
               ((eq? next-char #\b)
                (read-number (read-char_) 2))
               ((eq? next-char #\o)
                (read-number (read-char_) 8))
               ; TODO: Handle hex numbers,
               ; bin & oct work right now bc/
               ; char - 48 = value,
               ; for "A".."F" this is not the case
               (else (error "Illegal symbol after #: " next-char)))))
          ((char-alphabetic? first-char)
           (read-identifier first-char))
          ((eq? first-char #\-)
           (fxneg (read-number (read-char_) 10)))
          ((char-numeric? first-char)
           (read-number first-char 10))
          (else
            (error "Illegal lexical syntax: " first-char)))))

(def string-escape-codes
     (list (cons #\n #\newline)
           (cons #\t #\tab)
           (cons #\" #\")
           (cons #\\ #\\)))

(defn read-string (acc)
      (let ((first-char (read-char_)))
        (cond
          ((eq? first-char #\\)
           (read-string
             (string-append acc
                            (~> (read-char_) 
                                (assoc string-escape-codes)
                                rst
                                char->string))))
          ((eq? first-char #\")
           acc)
          (else (~>> first-char
                     char->string
                     (string-append acc)
                     read-string)))))

(defn read-character ()
      (let ((first-char (read-char_))
            (next-char (peek-char)))
        (cond
          ((and (eq? first-char #\n)
                (eq? next-char #\e))
           (skip-chars 6)
           (fixnum->char 10))
          ((and (eq? first-char #\t)
                (eq? next-char #\a))
           (skip-chars 2)
           (fixnum->char 11))
          ((and (eq? first-char #\s)
                (eq? next-char #\p))
           (skip-chars 4)
           (fixnum->char 32))
          (else first-char))))

(defn read-identifier (first-char)
      (~> first-char
          (cons '())
          read-identifier_
          list->string
          string->symbol))

(defn read-identifier_ (acc)
      (let ((next-char (peek-char)))
        (if (or (char-alphabetic? next-char)
                (char-numeric? next-char)
                (char-special? next-char))
          (read-identifier_ (cons (read-char_) acc))
          (reverse acc))))

(defn read-number (first-char base)
      (~> first-char
          char->fixnum
          (fx- 48)
          (read-number_ base)))

(defn read-number_ (acc base)
      (let ((next-char (peek-char)))
        (if (char-numeric? next-char)
          (read-number_
            (fx+ (fx* acc base)
                 (~> (read-char_) char->fixnum (fx- 48)))
            base)
          acc)))

(defn read_ ()
      (let ((next-token (read-token)))
        (cond
          ((eq? next-token 'left-paren)
           (read-list '()))
          ((eq? next-token 'quote)
           (list 'quote (read_)))
          ((eq? next-token 'quasiquote)
           (list 'quasiquote (read_)))
          ((eq? next-token 'unquote)
           (list 'unquote (read_)))
          (else next-token))))

(defn read-list (acc)
      (let ((next-token (read-token)))
        (cond
          ((eq? next-token 'right-paren)
           (reverse acc))
          ((eq? next-token 'left-paren)
           (read-list (cons (read-list '())
                            acc)))
          ((eq? next-token 'quote)
           (read-list (cons (list 'quote (read_))
                            acc)))
          ((eq? next-token 'quasiquote)
           (read-list (cons (list 'quasiquote (read_))
                            acc)))
          ((eq? next-token 'unquote)
           (read-list (cons (list 'unquote (read_))
                            acc)))
          (else
            (read-list (cons next-token acc))))))

(def string->fixnum string->number)

; (inspect (read))

(defn char-special? (char)
      (or (eq? char #\<)
          (eq? char #\>)
          (eq? char #\=)
          (eq? char #\-)
          (eq? char #\+)
          (eq? char #\*)
          (eq? char #\?)
          (eq? char #\\)
          (eq? char #\_)))
