(include "compile.scm")

(defn emit-program (exprs)
  (let ((preprocessed (map (fn (expr) (~> expr syntax-desugar alpha-convert-expr closure-convert normalize-term))
                           exprs)))
    (for-each emit-global-var global-vars)
    (for-each (fn (expr)
                  (emit-lambda (global-var-env) expr))
              lambdas)
    (print "define i64 @prim_main() {")
    (for-each (fn (expr) (emit-expr (generate-var) (global-var-env) expr)) preprocessed)
    (emit-ret (fixnum->string 0))
    (print "}"))
)


(defn global-var-env ()
      (extend-env* global-vars
                   (map (fn (var) (string-append "@var_" (escape var)))
                        global-vars)
                   empty-env))

(def infile (fst (command-line-arguments)))
(emit-program (read-file infile))

; (defn debug (expr)
;       (print (~> expr))
;       (let ((e2 (~> expr syntax-desugar)))
;         (print e2)
;         (let ((e3 (~> e2 alpha-convert-expr)))
;           (print e3)
;           (let ((e4 (~> e3 closure-convert)))
;             (print e4)
;             (let ((e5 (~> e3 normalize-term)))
;               (print e5))))))


; (for-each debug 
;           (read-file infile))

; (print ">> Global vars")
; (print global-vars)
; (print ">> Lambdas")
; (print lambdas)
; (set! lambdas '())
; (set! global-vars '())
