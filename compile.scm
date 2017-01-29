(include "compatibility.scm")
(include "stdlib.scm")
(include "syntax.scm")
(include "helper.scm")
(include "llvm.scm")
(include "syntax/if.scm")
(include "syntax/begin.scm")
(include "syntax/let.scm")
(include "syntax/string.scm")
(include "preprocessing/syntax-desugar.scm")
(include "preprocessing/alpha-convert.scm")
(include "preprocessing/a-normalize.scm")
(include "preprocessing/closure-convert.scm")

(def tag_mask #b111)
(def fixnum_tag #b000)

(def char_tag #b010)
(def constant_tag #b111)
(def true  #b111111)
(def false #b011111)
(def null  #b000111)

(defn immediate? (x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(defn atomic? (x)
  (or (immediate? x) (symbol? x) (string? x))) 

(defn immediate-rep (x)
  (cond
    ((fixnum? x) 
     (fxshl x 3))
    ((char? x)
     (+ (fxshl (char->integer x) 3)
        char_tag))
    ((boolean? x)
     (if x true false))
    ((null? x) null)
    (else (error "Invalid expression: " x))))

(defn variable? (expr) (symbol? expr))

(defn emit-immediate (var expr)
      (~>> expr
           immediate-rep
           fixnum->string
           (emit-copy var)))

(defn emit-toplevel-expr (expr)
  (cond
    ((defprim? expr)
     (let* ((name (defprim-name expr))
            (args (defprim-args expr))
            (args-with-vars (map (fn (arg) (cons arg (generate-var))) args))
            (args-string
              (string-join2 (map (fn (a) (string-append "i64 " (rst a))) args-with-vars) ", ")))
       (print (string-append* (list "define i64 @" (escape name) "(" args-string ") {")))
       (emit-expr "%res" args-with-vars (defn-body expr))
       (emit-ret "%res")
       (print "}")))
     (else (error "Invalid toplevel expression: " expr))))

(defn emit-lambda (expr)
  (let* ((name (fst expr))
         (lmbda (frst expr))
         (args (fn-params lmbda))
         (arity (length args))
         (body (fn-body lmbda))
         (prep-body (~> body normalize-term))
         (args-with-vars (map (fn (arg) (cons arg (generate-var))) args))
         (args-string
           (string-join2 (map (fn (a) (string-append "i64 " (rst a))) args-with-vars) ", ")))
    (print (string-append* (list "define i64 @lambda_" (symbol->string name) "(" args-string ") {")))
    (emit-expr "%res" args-with-vars prep-body)
    (emit-ret "%res")
    (print "}")))

(defn emit-global-var (var)
  (print (string-append* (list "@var_" (escape var) " = weak global i64 0"))))

(defn emit-expr (var env expr)
  (cond
    ((immediate? expr) (emit-immediate var expr))
    ((string? expr) (emit-string var expr))
    ((tagged-list? expr 'quote) (emit-symbol var (frst expr)))
    ((if? expr) (emit-if var env expr))
    ((begin? expr) (emit-begin var env expr))
    ((let? expr) (emit-let var env expr))
    ((tagged-list? expr 'make-closure)
     (let ((tmp1 (generate-var))
           (tmp2 (generate-var))
           (name (frst expr))
           (arity (frrst expr))
           (env_ (frrrst expr)))
       (print (string-append* (list "  " tmp1 " = ptrtoint i64 (" (arg-str (add1 arity)) ")* @lambda_" (symbol->string name) " to i64")))
       (emit-expr tmp2 env env_)
       (emit-call3 var "@internal_make-closure" tmp1 (fixnum->string (immediate-rep arity)) tmp2)
       ))
    ((def? expr)
     (let ((tmp (generate-var)))
       (emit-expr tmp env (def-value expr))
       (emit-store tmp (string-append "@var_" (escape (def-name expr))))))
    ; For now, `set!` is just converted to `def` in `closure-convert.scm`,
    ; the only difference is, that `def` adds an element to the list of global vars
    ; ((tagged-list? expr 'set!)
    ;  (let ((tmp (generate-var))
    ;        (name (frst expr))
    ;        (value (frrst expr)))
    ;    (emit-expr tmp env value)
    ;    (emit-store tmp (string-append "@var_" (escape name)))))
    ((list? expr)
     (let* ((name (fst expr))
            (args (rst expr))
            (vars (map (fn (arg)
                         (string-append
                           "i64 "
                           ; TODO: why can't this be replaced w/ `lookup`?
                           (let ((res (assoc arg env)))
                             (if res (rst res) (fixnum->string (immediate-rep arg))))))
                       args)))
       ; TODO: call emit-env with an initial env where all closures are bound to ther @var_...
       ; if a value is not in the env, assume it to be primitive
       (if (symbol? name)
         (print (string-append* (list "  " var " = call i64 @" (escape name) "(" (string-join2 vars ", ") ")" )))
         (begin
           (let ((tmp1 (generate-var))
                 (tmp2 (generate-var))
                 (tmp3 (generate-var))
                 (tmp4 (generate-var))
                 (arity (length args)))
             (emit-variable-ref tmp1 env name)
             (emit-call1 tmp2 "@internal_closure-function" tmp1)
             (emit-call1 tmp4 "@prim_closure-env" tmp1)
             (print (string-append* (list "  " tmp3 " = inttoptr i64 " tmp2 " to i64 (" (arg-str (add1 arity)) ")*")))  
             (print (string-append* (list "  " var  " = call i64 " tmp3 "(i64 " tmp4 ", " (string-join2 vars ", ") ")"))))))))
    ((variable? expr) (emit-variable-ref var env expr))
    (else
      (error "Unknown expression: " expr))))

(defn emit-variable-ref (var env expr)
  (let ((var_ (lookup-or expr #f env)))
    (if var_
      (emit-copy var var_)
      (emit-load var (string-append "@var_" (escape expr))))))

(defn emit-symbol (var expr)
  (let ((tmp (generate-var)))
    (emit-string tmp (symbol->string expr))
    (emit-call1 var "@prim_string-_greater_symbol" tmp)))

(defn debug-program (exprs)
  (let ((preprocessed (map (fn (expr)
                               (~> expr syntax-desugar alpha-convert-expr closure-convert normalize-term))
                           (append stdlib exprs))))
    (print ">>> Global vars")
    (for-each print global-vars)
    (print ">>> Lambdas")
    (for-each print lambdas)
    (print ">>> Expressions")
    (for-each print preprocessed)
))
