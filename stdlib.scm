(def stdlib '(
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
          ((string? val)
           val)
          ((symbol? val)
           (string-append "'" (symbol->string val)))
          (else "unknown")))

  (defn inspect (val)
        (print (any->string val))
        (newline))
))
