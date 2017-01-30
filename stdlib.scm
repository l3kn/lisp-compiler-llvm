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
                    inspect assoc alist-cons
                    char->string
                    ; TODO: some preprocessing step does not handle begin
                    begin if
                    )))
