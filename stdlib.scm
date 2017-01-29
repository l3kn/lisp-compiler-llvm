(defn primitive? (name)
      (member name '(not eq? char->string char->fixnum fixnum->char digit->string
                    fx+ fx- fxneg fxrem fx/ fx* fxadd1 fxsub1
                    fx= fxzero? fx=? fx<? fx>? fx<=? fx>=?
                    cons fst rst
                    newline putchar print
                    string->symbol symbol->string
                    __tag __value __heap-index
                    string-length string-append
                    string=? string-ref string-substring
                    closure-arity
                    list-ref
                    ; TODO: some preprocessing step does not handle begin
                    begin if
                    )))

(def stdlib '(
  (defprim symbol? (val) (eq? (__tag val) 1))
  (defprim char? (val) (eq? (__tag val) 2))
  (defprim closure? (val) (eq? (__tag val) 4))
  (defprim string? (val) (eq? (__tag val) 5))
  (defprim pair? (val) (eq? (__tag val) 6))
  (defprim fixnum? (val) (eq? (__tag val) 0))

  (defprim null? (val) (eq? val (list)))


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
           (string-append "'" (symbol->string val)))
          ((closure? val)
           (string-append* (list "<closure/" (fixnum->string (closure-arity val)) ">")))
          (else "unknown")))

  (defprim pair->string (pair)
        (string-append* (list "(" (any->string (fst pair)) " . " (any->string (rst pair)) ")")))

  (defprim inspect (val)
        (print (any->string val))
        (newline))

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
))
