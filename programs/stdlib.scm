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

(defprim string->list (str)
         (string->list_ str 0 (string-length str)))

(defprim string->list_ (str idx len)
         (if (eq? idx len)
           '()
           (cons (string-ref str idx)
                 (string->list_ str (fxadd1 idx) len))))

(defprim string-append* (lst)
         (if (null? lst)
           ""
           (string-append (fst lst) (string-append* (rst lst)))))

(defprim any->string (val)
         (cond
           ((eq? val #t) "#t")
           ((eq? val #f) "#f")
           ((null? val) "()")
           ((fixnum? val)
            (fixnum->string val))
           ((pair? val)
            (pair->string val))
           ((char? val)
            (string-append "#\\" (char->string val)))
           ((string? val)
            val)
           ((symbol? val)
            (symbol->string val))
           ((closure? val)
            (string-append* (list "<closure/" (fixnum->string (closure-arity val)) ">")))
           (else "unknown")))

(defprim pair->string (pair)
         (string-append* (list "(" (any->string (fst pair)) " . " (any->string (rst pair)) ")")))

(defprim inspect (val)
         (puts (inspect_ val)))
(defprim inspect_ (val)
         (cond
           ((symbol? val) (string-append "'" (symbol->string val)))
           (else (any->string val))))

(defprim puts (val)
         (print val)
         (newline))

(defprim read (str)
         (rst (read_ str 0 (string-length str))))

(defprim char-is-digit? (char)
         (let ((ord (char->fixnum char)))
           (and (fx<=? 48 ord) (fx>=? 57 ord))))

(defprim char-is-letter? (char)
         (let ((ord (char->fixnum char)))
           (or (and (fx<=? 65 ord) (fx>=? 90 ord))
               (and (fx<=? 97 ord) (fx>=? 122 ord)))))

(defprim char-is-special? (char)
         (or (eq? char #\<)
             (eq? char #\>)
             (eq? char #\=)
             (eq? char #\-)
             (eq? char #\+)
             (eq? char #\*)
             (eq? char #\\)
             (eq? char #\_)))

(defprim char-is-valid-symbol? (char)
         (or (char-is-letter? char)
             (char-is-special? char)
             (char-is-digit? char)))

(defprim read_ (str idx len)
         ; (puts "reading")
         ; (inspect str)
         ; (inspect idx)
         (if (fx>=? idx len)
           (cons idx "EOF")
           (let ((char (string-ref str idx)))
             (cond 
               ; Skip whitespace
               ((or (eq? char #\tab)
                    (eq? char #\newline)
                    (eq? char #\return)
                    (eq? char #\space))
                (read_ str (fxadd1 idx) len))
               ((eq? char #\;)
                (read_ str
                       (skip-comment str (fxadd1 idx) len)
                       len))
               ((and (eq? char #\-)
                     (char-is-digit? (string-ref str (fxadd1 idx))))
                (read-fixnum str (fxadd1 idx) len 0 #t))
               ((eq? char #\()
                (read-list str (fxadd1 idx) len))
               ((char-is-digit? char)
                (read-fixnum str idx len 0 #f))
               ; this would allow symbols like '0foo, too,
               ; but because of the `char-is-digit?` clause above
               ; this should never happen
               ((char-is-valid-symbol? char)
                (let ((end-of-symbol (find-end-of-symbol str idx len)))
                  (cons end-of-symbol
                        (string->symbol (string-substring str idx end-of-symbol)))))
               (else
                 (cons idx "<unknown>"))))))

(defprim find-end-of-symbol (str idx len)
         (if (fx>=? idx len)
           idx
           (let ((char (string-ref str idx)))
             (if (char-is-valid-symbol? char)
               (find-end-of-symbol str (fxadd1 idx) len)
               idx))))

(defprim read-list (str idx len)
         ; (puts "reading list")
         ; (inspect idx)
         (let ((char (string-ref str idx)))
           (cond 
             ((eq? char #\)) (cons (fxadd1 idx) '()))
             (else
               (let* ((res (read_ str idx len))
                      (new-idx (fst res))
                      (val (rst res)))
                 (let* ((rest (read-list str new-idx len))
                        (new-idx2 (fst rest))
                        (val2 (rst rest)))
                   (cons new-idx2
                         (cons val val2))))))))

(defprim read-fixnum (str idx len acc neg)
         (if (fx>=? idx len)
           (cons idx (if neg (fxneg acc) acc))
           (let* ((char (string-ref str idx)))
             (if (char-is-digit? char)
               (let ((val (fx- (char->fixnum char) 48)))
                 (read-fixnum str
                              (fxadd1 idx)
                              len
                              (fx+ val (fx* acc 10))
                              neg))
               (cons idx (if neg (fxneg acc) acc))))))

(defprim skip-comment (str idx len)
         (if (fx>=? idx len)
           idx
           (let* ((char (string-ref str idx)))
             (if (or (eq? char #\newline) (eq? char #\return))
               (fxadd1 idx)
               (skip-comment str (fxadd1 idx) len)))))

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

(defprim join (lst sep)
         (cond
           ((null? lst) "")
           ((null? (rst lst)) (any->string (fst lst)))
           (else (string-append
                   (string-append (any->string (fst lst)) sep)
                   (join (rst lst) sep)))))

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

