(include "compile.scm")

(defn emit-program (exprs)
  (let ((preprocessed (map (fn (expr) (~> expr syntax-desugar alpha-convert-expr closure-convert normalize-term))
                           exprs)))
    (for-each emit-global-var global-vars)
    (for-each emit-lambda lambdas)
    (print "define i64 @prim_main() {")
    (for-each (fn (expr) (emit-expr (generate-var) empty-env expr)) preprocessed)
    (emit-ret (fixnum->string 0))
    (print "}"))
)


(def infile (fst (command-line-arguments)))
(emit-program (read-file infile))
