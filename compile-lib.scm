(include "compile.scm")

(defn emit-lib (exprs)
  (let ((preprocessed (map (fn (expr) (~> expr syntax-desugar alpha-convert-expr closure-convert)); normalize-term))
                           exprs)))
    (for-each emit-global-var global-vars)
    (for-each emit-lambda lambdas)
    (for-each emit-toplevel-expr preprocessed))
)

(defn debug (expr)
      (print (~> expr))
      (let ((e2 (~> expr syntax-desugar)))
        (print e2)
        (let ((e3 (~> e2 alpha-convert-expr)))
          (print e3)
          (let ((e4 (~> e3 closure-convert)))
            (print e4)
            (let ((e5 (~> e3 normalize-term)))
              (print e5)))))
      (set! lambdas '())
      (set! global-vars '()))

(def infile (fst (command-line-arguments)))
(def exprs (read-file infile))

; (emit-lib (fst exprs))

; (def input
; '(defprim inspect (val)
;          (print (any->string val))
;          (newline))
; )

; (debug input)

; (emit-lib (list input))

(emit-lib exprs)
