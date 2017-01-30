(include "compatibility.scm")
(include "syntax.scm")
(include "helper.scm")
(include "llvm.scm")
(include "preprocessing/desugar.scm")
(include "preprocessing/alpha-convert.scm")
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
     (+ (fxshl (char->fixnum x) 3)
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

(defn emit-lambda (env expr)
  (let* ((name (fst expr))
         (lmbda (frst expr))
         (args (fn-params lmbda))
         (arity (length args))
         (body (fn-body lmbda))
         (args-with-vars (map (fn (arg) (cons arg (generate-var))) args))
         (args-string
           (string-join2 (map (fn (a) (string-append "i64 " (rst a))) args-with-vars) ", ")))
    (print (string-append* (list "define i64 @lambda_" (symbol->string name) "(" args-string ") {")))
    (emit-expr "%res" (append args-with-vars env) body)
    (emit-ret "%res")
    (print "}")))

(defn emit-global-var (var)
  (print (string-append* (list "@var_" (escape var) " = weak global i64 0"))))

(defn emit-expr (var env expr)
  ; (print "  ;" var " = " expr)
  (cond
    ((immediate? expr) (emit-immediate var expr))
    ((string? expr) (emit-string var expr))
    ((tagged-list? expr 'quote)
     (if (eq? (frst expr) '())
         (emit-immediate var '())
         (emit-symbol var (frst expr))))
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
     (emit-expr var env (def-value expr))
     (emit-store var (string-append "@var_" (escape (def-name expr)))))
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
            (args-with-vars (map (fn (a) (cons a (generate-var))) args)))
       (for-each (fn (av)
                     (emit-expr (rst av) env (fst av)))
                 args-with-vars)
       (let ((vars
               (map (fn (av) (string-append "i64 " (rst av)))
                    args-with-vars)))
       ; TODO: call emit-env with an initial env where all closures are bound to ther @var_...
       ; if a value is not in the env, assume it to be primitive
       (if (assoc name env)
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
               (if (> (length vars) 0)
                   (print (string-append* (list "  " var  " = call i64 " tmp3 "(i64 " tmp4 ", " (string-join2 vars ", ") ")")))
                   (print (string-append* (list "  " var  " = call i64 " tmp3 "(i64 " tmp4 ")"))))))
           (print (string-append* (list "  " var " = call i64 @" (escape name) "(" (string-join2 vars ", ") ")" )))))))
    ((variable? expr) (emit-variable-ref var env expr))
    (else
      (error "Unknown expression: " expr)))
  )
  ; (print "  ; end of " var " = " expr))

(defn emit-variable-ref (var env expr)
  ; (print ">>> emit-var-ref " expr)
  (let ((res (assoc expr env)))
    (if res
      (if (eq? (string-ref (rst res) 0) #\@)
          (emit-load var (rst res))
          (emit-copy var (rst res)))
      ; (emit-load var (string-append "@var_" (escape expr))))))
      (error "can't find " expr " in env"))))

(defn emit-symbol (var expr)
  (let ((tmp (generate-var)))
    (emit-string tmp (symbol->string expr))
    (emit-call1 var "@prim_string-_greater_symbol" tmp)))

(defn emit-if (var env expr)
  (let ((true-label (unique-label "true"))
        (false-label (unique-label "false"))
        (end-label (unique-label "end"))
        (test-var (generate-var))
        (test-res-var (generate-var))
        (res-var (generate-var))
        (res-var1 (generate-var))
        (res-var2 (generate-var)))
    (emit-expr test-var env (if-test expr))
    (print (string-append* (list "  " test-res-var " = icmp eq i64 " test-var ", " (fixnum->string (immediate-rep #t)))))
    (emit-alloca res-var)
    (print (string-append* (list "  br i1 " test-res-var ", label %" true-label ", label %" false-label)))

    (emit-label true-label)
    (emit-expr res-var1 env (if-consequent expr))
    (emit-store res-var1 res-var)
    (emit-br1 end-label)

    (emit-label false-label)
    (emit-expr res-var2 env (if-alternative expr))
    (emit-store res-var2 res-var)
    (emit-br1 end-label)

    (emit-label end-label)
    (emit-load var res-var)))

(defn emit-begin (var env expr)
  (emit-begin_ var env (begin-expressions expr)))
(defn emit-begin_ (var env lst)
  (cond ((null? lst) (error "Empty begin"))
        ((null? (rst lst)) (emit-expr var env (fst lst)))
        (else
          (emit-expr (generate-var) env (fst lst))
          (emit-begin_ var env (rst lst)))))

(defn emit-let (var env expr)
  (process-let var expr (let-bindings expr) env))
(defn process-let (var expr bindings new-env)
  (if (null? bindings)
      (emit-expr var new-env (let-body expr))
      (let ((b (fst bindings))
            (var_ (generate-var)))
        ; It's ok to use new-env instead of env here,
        ; because all variable conflicts have been resolved
        ; during the alpha-conversion step
        (emit-expr var_ new-env (let-binding-value b))
        (process-let var expr
                     (rst bindings)
                     (extend-env (let-binding-variable b) var_ new-env)))))

(defn emit-string (var str)
  (let ((len (string-length str))
        (tmp (generate-var)))
    (emit-call0 tmp "@internal_heap-current-pointer")
    (emit-string_ str 0 len)
    (print "  call i64 @internal_heap-store-byte(i8 0)")
    (print "  call void @internal_heap-align-index()")
    (print (string-append* (list "  " var " = or i64 " tmp ", 5")))))
(defn emit-string_ (str idx len)
  ; TODO: Rewrite this once one-armed ifs are supported
  (if (< idx len)
      (begin
        (print (string-append* (list "  call i64 @internal_heap-store-byte(i8 "
                                     (~>> idx
                                          (string-ref str)
                                          char->fixnum
                                          fixnum->string)
                                     ")")))
        (emit-string_ str (add1 idx) len))
      'ok))

(defn debug-program (exprs)
  (let ((preprocessed (map (fn (expr)
                               (~> expr desugar alpha-convert-expr closure-convert))
                           (append stdlib exprs))))
    (print ">>> Global vars")
    (for-each print global-vars)
    (print ">>> Lambdas")
    (for-each print lambdas)
    (print ">>> Expressions")
    (for-each print preprocessed)
))
