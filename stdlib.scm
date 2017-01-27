(def stdlib '(
  (defn fixnum? (val) (eq? (__tag val) 0))
  (defn symbol? (val) (eq? (__tag val) 1))
  (defn char? (val) (eq? (__tag val) 2))
  (defn closure? (val) (eq? (__tag val) 4))
  (defn string? (val) (eq? (__tag val) 5))
  (defn pair? (val) (eq? (__tag val) 6))

  (defn null? (val) (eq? val (list)))

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

  (defn string-append* (lst)
        (if (null? lst)
            ""
            (string-append (fst lst) (string-append* (rst lst)))))

  (defn any->string (val)
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
           (string-append "'" (symbol->string val)))
          ((closure? val)
           (string-append* (list "<closure/" (fixnum->string (closure-arity val)) ">")))
          (else "unknown")))

  (defn pair->string (pair)
        (string-append* (list "(" (any->string (fst pair)) " . " (any->string (rst pair)) ")")))

  (defn inspect (val)
        (print (any->string val))
        (newline))

  (defn puts (val)
        (print val)
        (newline))

  (defn read (str)
        (rst (read_ str 0 (string-length str))))

  (defn char-is-digit? (char)
    (let ((ord (char->fixnum char)))
      (and (fx<=? 48 ord) (fx>=? 57 ord))))

  (defn char-is-letter? (char)
    (let ((ord (char->fixnum char)))
      (or (and (fx<=? 65 ord) (fx>=? 90 ord))
          (and (fx<=? 97 ord) (fx>=? 122 ord)))))

  (defn char-is-special? (char)
        (or (eq? char #\<)
            (eq? char #\>)
            (eq? char #\=)
            (eq? char #\-)
            (eq? char #\+)
            (eq? char #\*)
            (eq? char #\\)
            (eq? char #\_)))

  (defn char-is-valid-symbol? (char)
        (or (char-is-letter? char)
            (char-is-special? char)
            (char-is-digit? char)))

  (defn read_ (str idx len)
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

  (defn find-end-of-symbol (str idx len)
        (if (fx>=? idx len)
            idx
            (let ((char (string-ref str idx)))
              (if (char-is-valid-symbol? char)
                  (find-end-of-symbol str (fxadd1 idx) len)
                  idx))))

  (defn read-list (str idx len)
        ; (puts "reading list")
        ; (inspect idx)
        (let ((char (string-ref str idx)))
          (cond 
            ((eq? char #\)) (cons (fxadd1 idx) (list)))
            (else
              (let* ((res (read_ str idx len))
                     (new-idx (fst res))
                     (val (rst res)))
                (let* ((rest (read-list str new-idx len))
                       (new-idx2 (fst rest))
                       (val2 (rst rest)))
                  (cons new-idx2
                        (cons val val2))))))))

  (defn read-fixnum (str idx len acc neg)
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

  (defn skip-comment (str idx len)
        (if (fx>=? idx len)
            idx
            (let* ((char (string-ref str idx)))
              (if (or (eq? char #\newline) (eq? char #\return))
                  (fxadd1 idx)
                  (skip-comment str (fxadd1 idx) len)))))

  (defn map (f lst)
        (if (null? lst)
            lst
            (cons (f (fst lst))
                  (map f (rst lst)))))
))

