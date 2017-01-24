(def stdlib '(
  (defn fixnum? (val) (eq? (__tag val) 0))
  (defn symbol? (val) (eq? (__tag val) 1))
  (defn char? (val) (eq? (__tag val) 2))
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

  (defn pair->string (pair)
        (string-append* (list "(" (any->string (fst pair)) " . " (any->string (rst pair)) ")")))

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
          (else "unknown")))

  (defn inspect (val)
        (print (any->string val))
        (newline))

  (defn read (str)
        (read_ str 0 (string-length str) 0))

  (defn char-is-digit? (char)
    (let ((ord (char->fixnum char)))
      (and (fx<=? 48 ord) (fx>=? 57 ord))))

  (defn read_ (str idx len acc)
        (let ((char (string-ref str idx)))
          (cond ((eq? char #\-)
                 (fxneg (read-fixnum str (fxadd1 idx) len 0)))
                ((char-is-digit? char)
                 (read-fixnum str idx len 0))
                (else "<unknown>"))))

  (defn read-fixnum (str idx len acc)
        (if (fx>=? idx len)
            acc
            (let* ((char (string-ref str idx)))
              (if (char-is-digit? char)
                  (let ((val (fx- (char->fixnum char) 48)))
                    (read-fixnum str (fxadd1 idx) len (fx+ val (fx* acc 10))))
                  acc))))
))

