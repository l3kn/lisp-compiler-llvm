(include "compile.scm")

(defn emit-program (exprs)
  (let ((preprocessed (map (fn (expr) (~> expr desugar alpha-convert-expr closure-convert)); normalize-term))
                           exprs)))
    (for-each emit-global-var global-vars)
    (for-each (fn (expr)
                  (emit-lambda (global-var-env) expr))
              lambdas)
    (puts "define i64 @prim_main() {")
    (for-each (fn (expr) (emit-expr (generate-var) (global-var-env) expr)) preprocessed)
    (emit-ret (fixnum->string 0))
    (puts "}"))
)

(def infile (fst (command-line-arguments)))
(emit-program (read-file infile))

; (defn debug (expr)
;       (puts (~> expr))
;       (let ((e2 (~> expr desugar)))
;         (puts e2)
;         (let ((e3 (~> e2 alpha-convert-expr)))
;           (puts e3)
;           (let ((e4 (~> e3 closure-convert)))
;             (puts e4)))))


; (for-each debug 
;           (read-file infile))

; (puts ">> Global vars")
; (puts global-vars)
; (puts ">> Lambdas")
; (puts lambdas)
; (set! lambdas '())
; (set! global-vars '())
