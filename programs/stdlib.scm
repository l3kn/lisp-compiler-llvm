lib

(defprim symbol? (val) (eq? (__tag val) 1))
(defprim char? (val) (eq? (__tag val) 2))
(defprim closure? (val) (eq? (__tag val) 4))
(defprim string? (val) (eq? (__tag val) 5))
(defprim pair? (val) (eq? (__tag val) 6))
(defprim fixnum? (val) (eq? (__tag val) 0))

(defprim boolean? (val)
         (or (eq? val #t)
             (eq? val #f)))
(defprim null? (val) (eq? val '()))

; For now, the only fixnums are supported
; => just wrap the fixnum functions
(defprim sub1 (val) (fxsub1 val))
(defprim add1 (val) (fxadd1 val))
(defprim + (a b) (fx+ a b))
(defprim - (a b) (fx- a b))
(defprim * (a b) (fx* a b))
(defprim / (a b) (fx/ a b))
(defprim = (a b) (fx=? a b))
(defprim < (a b) (fx<? a b))
(defprim <= (a b) (fx<=? a b))
(defprim > (a b) (fx>? a b))
(defprim >= (a b) (fx>=? a b))

(defprim ffst (e) (fst (fst e)))
(defprim frst (e) (fst (rst e)))
(defprim rfst (e) (rst (fst e)))
(defprim rrst (e) (rst (rst e)))

(defprim fffst (e) (fst (fst (fst e))))
(defprim ffrst (e) (fst (fst (rst e))))
(defprim frfst (e) (fst (rst (fst e))))
(defprim frrst (e) (fst (rst (rst e))))

(defprim rffst (e) (rst (fst (fst e))))
(defprim rfrst (e) (rst (fst (rst e))))
(defprim rrfst (e) (rst (rst (fst e))))
(defprim rrrst (e) (rst (rst (rst e))))

(defprim ffffst (e) (fst (fst (fst (fst e)))))
(defprim fffrst (e) (fst (fst (fst (rst e)))))
(defprim ffrfst (e) (fst (fst (rst (fst e)))))
(defprim ffrrst (e) (fst (fst (rst (rst e)))))
(defprim frffst (e) (fst (rst (fst (fst e)))))
(defprim frfrst (e) (fst (rst (fst (rst e)))))
(defprim frrfst (e) (fst (rst (rst (fst e)))))
(defprim frrrst (e) (fst (rst (rst (rst e)))))

(defprim rfffst (e) (rst (fst (fst (fst e)))))
(defprim rffrst (e) (rst (fst (fst (rst e)))))
(defprim rfrfst (e) (rst (fst (rst (fst e)))))
(defprim rfrrst (e) (rst (fst (rst (rst e)))))
(defprim rrffst (e) (rst (rst (fst (fst e)))))
(defprim rrfrst (e) (rst (rst (fst (rst e)))))
(defprim rrrfst (e) (rst (rst (rst (fst e)))))
(defprim rrrrst (e) (rst (rst (rst (rst e)))))

(defprim fixnum->string_ (fx)
         (if (fxzero? fx)
           ""
           (string-append
             (fixnum->string_ (fx/ fx 10))
             (digit->string (fxrem fx 10)))))
(defprim fixnum->string (fx)
         (cond
           ((fxzero? fx) "0")
           ((fx<? fx 0) (string-append "-" (fixnum->string_ (fx- 0 fx))))
           (else (fixnum->string_ fx))))

(defprim string->fixnum (str)
         (let ((digit (string-ref str 0)))
           (cond
             ((eq? digit #\0) 0)
             ((eq? digit #\-)
              (fxneg (string->fixnum_ str 1 (string-length str) 0)))
             (else (string->fixnum_ str 0 (string-length str) 0)))))

(defprim string->fixnum_ (str idx len acc)
         (if (eq? idx len)
             acc
             (let ((digit
                     (~> (string-ref str idx)
                         char->fixnum
                         (fx- 48))))
               (string->fixnum_ str (fxadd1 idx) len
                                (fx+ (fx* 10 acc)
                                     digit)))))

(defprim string->list (str)
         (string->list_ str 0 (string-length str)))

(defprim string->list_ (str idx len)
         (if (eq? idx len)
           '()
           (cons (string-ref str idx)
                 (string->list_ str (fxadd1 idx) len))))

(defprim any->string (val)
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
           ((closure? val)
            (format "<closure/~A>" (list (closure-arity val))))
           (else "unknown")))

(defprim list->string (lst)
         (if (null? lst)
             ""
             (string-append
               (~> lst fst char->string)
               (list->string (rst lst)))))

(defprim pair->string (pair)
         (format "(~A . ~A)" (list (any->string (fst pair))
                                   (any->string (rst pair)))))

(defprim inspect (val)
         (puts (inspect_ val)))
(defprim inspect_ (val)
         (cond
           ((symbol? val) (string-append "'" (symbol->string val)))
           (else (any->string val))))

(defprim format (str vars) (format_ str vars 0 (string-length str) ""))

; TODO: This will break if there are not enough vars
(defprim format_ (str vars idx len res)
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

(defprim puts (val)
         (print val)
         (newline))

(defprim char-numeric? (char)
         (let ((ord (char->fixnum char)))
           (and (fx<=? 48 ord) (fx>=? 57 ord))))

(defprim char-whitespace? (char)
         (or (eq? char #\space)
             (eq? char #\newline)))

(defprim char-alphabetic? (char)
         (let ((ord (char->fixnum char)))
           (or (and (fx<=? 65 ord) (fx>=? 90 ord))
               (and (fx<=? 97 ord) (fx>=? 122 ord)))))

(defprim char-special? (char)
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


(defprim map (f lst)
         (if (null? lst)
           lst
           (cons (f (fst lst))
                 (map f (rst lst)))))

(defprim for-each (f lst)
         (if (null? lst)
           'done
           (begin
             (f (fst lst))
             (for-each f (rst lst)))))

(defprim filter (pred lst)
         (cond ((null? lst) '())
               ((eq? (pred (fst lst)) #t)
                (cons (fst lst)
                      (filter pred (rst lst))))
               (else (filter pred (rst lst)))))

(defprim alist-cons (var val alist)
         (cons (cons var val) alist))

(defprim assoc (var alist)
         (cond ((null? alist) #f)
               ((eq? (ffst alist) var) (fst alist))
               (else (assoc var (rst alist)))))

(defprim member? (var lst)
         (cond ((null? lst) #f)
               ((eq? var (fst lst)) #t)
               (else (member? var (rst lst)))))

(defprim append (a b)
         (cond ((null? a) b)
               ((null? b) a)
               (else
                 (cons (fst a)
                       (append (rst a) b)))))

(defprim join (sep lst)
         (cond
           ((null? lst) "")
           ((null? (rst lst)) (any->string (fst lst)))
           (else (string-append
                   (string-append (any->string (fst lst)) sep)
                   (join sep (rst lst))))))

(defprim reverse (lst)
         (reverse_ lst '()))

(defprim reverse_ (lst res)
         (if (null? lst)
             res
             (reverse_ (rst lst)
                       (cons (fst lst) res))))

(defprim list? (lst)
         (if (pair? lst)
           (list?_ lst)
           #f))

(defprim length (lst)
         (if (null? lst)
           0
           (fx+ 1 (length (rst lst)))))

(defprim list?_ (pair)
         (cond
           ((null? (rst pair)) #t)
           ((not (pair? (rst pair))) #f)
           (else (list?_ (rst pair)))))


(defprim tagged-list? (lst tag)
         (and (not (null? lst))
              (eq? (fst lst) tag)))

(defprim gensym ()
         (~>> (__symbol-table-index)
              fixnum->string
              (string-append "g")
              string->symbol))

(defprim error (msg val)
         (print "ERROR: ")
         (print msg)
         (print " ")
         (inspect val))

