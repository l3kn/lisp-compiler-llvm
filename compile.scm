(defn immediate-rep (x)
      (cond
        ((fixnum? x) 
         (+ (fxshl x 3) 0))
        ((char? x)
         (+ (fxshl (char->fixnum x) 3) 2))
        ((boolean? x)
         (if x 63 31))
        ((null? x) 7)
        (else (error "Invalid expression: " x))))

(defn emit-immediate (var expr)
      (~>> expr immediate-rep fixnum->string (emit-copy var)))

(defn emit-toplevel-expr (expr)
      (cond
        ((defprim? expr)
         (let* ((name (defprim-name expr))
                (args (defprim-args expr))
                (vars (map (fn (arg) (generate-var)) args)))
           (puts (format "define i64 @~A(~A) {" (list (escape name) (var-str vars))))
           (emit-expr "%res" (extend-env* args vars empty-env) (defn-body expr))
           (emit-ret "%res")
           (puts "}")))
        (else (error "Invalid toplevel expression: " expr))))

(defn emit-lambda (env lmbda)
      (let* ((name (fst lmbda))
             (expr (frst lmbda))
             (args (fn-params expr))
             (vars (map (fn (arg) (generate-var)) args)))
        ; No need to use `(escape name)` here, the result of `(gensym)`
        ; doesn't contain any special characters
        (puts (format "define i64 @lambda_~A(~A) {" (list name (var-str vars))))
        (emit-expr "%res" (extend-env* args vars env) (fn-body expr))
        (emit-ret "%res")
        (puts "}")))

(defn emit-global-var (var)
      (puts (format "@var_~A = weak global i64 0" (list (escape var)))))

(defn emit-expr (var env expr)
      (cond
        ((immediate? expr) (emit-immediate var expr))
        ((string? expr) (emit-string var expr))
        ((quote? expr)
         (if (eq? (frst expr) '())
           (emit-immediate var '())
           (emit-symbol var (frst expr))))
        ((if? expr) (emit-if var env expr))
        ((begin? expr) (emit-begin var env expr))
        ((let? expr) (emit-let var env expr))
        ((tagged-list? expr 'make-closure)
         (let ((tmp1 (generate-var)) (tmp2 (generate-var))
               (name (frst expr))
               (arity (frrst expr))
               (env_ (frrrst expr)))
           (puts (format "  ~A = ptrtoint i64 (~A)* @lambda_~A to i64" (list tmp1 (~> arity add1 arg-str) name)))
           (emit-expr tmp2 env env_)
           (emit-call3 var "@internal_make-closure" tmp1 (immediate-rep arity) tmp2)))
        ((def? expr)
         (emit-expr var env (def-value expr))
         (emit-store var (~>> expr def-name escape (string-append "@var_"))))
        ; For now, `set!` is just converted to `def` in `closure-convert.scm`,
        ; the only difference is, that `def` adds an element to the list of global vars
        ((list? expr)
         (let* ((name (fst expr))
                (args (rst expr))
                (args-with-vars (map (fn (a) (cons a (generate-var))) args)))
           (for-each (fn (av) (emit-expr (rst av) env (fst av)))
                     args-with-vars)
           (let ((vars
                   (map (fn (av) (string-append "i64 " (rst av)))
                        args-with-vars)))
             ; TODO: call emit-env with an initial env where all closures are bound to ther @var_...
             ; if a value is not in the env, assume it to be primitive
             (if (assoc name env)
               (begin
                 (let ((tmp1 (generate-var)) (tmp2 (generate-var)) (tmp3 (generate-var)) (tmp4 (generate-var))
                       (arity (length args)))
                   (emit-variable-ref tmp1 env name)
                   (emit-call1 tmp2 "@internal_closure-function" tmp1)
                   (emit-call1 tmp4 "@prim_closure-env" tmp1)
                   (puts (format "  ~A = inttoptr i64 ~A to i64 (~A)*" (list tmp3 tmp2 (arg-str (add1 arity)))))
                   (if (> (length vars) 0)
                     (puts (format "  ~A = call i64 ~A(i64 ~A, ~A)" (list var tmp3 tmp4 (join ", " vars))))
                     (puts (format "  ~A = call i64 ~A(i64 ~A)" (list var tmp3 tmp4))))))
               (puts (format "  ~A = call i64 @~A(~A)" (list var (escape name) (join ", " vars))))))))
        ((symbol? expr) (emit-variable-ref var env expr))
        (else
          (error "Unknown expression: " expr)))
      )

(defn emit-variable-ref (var env expr)
      (let ((res (assoc expr env)))
        (if res
          (if (eq? (string-ref (rst res) 0) #\@)
            (emit-load var (rst res))
            (emit-copy var (rst res)))
          (error "can't find in env: " expr))))

(defn emit-symbol (var expr)
      (let ((tmp (generate-var)))
        (emit-string tmp (symbol->string expr))
        (emit-call1 var "@prim_string-_greater_symbol" tmp)))

(defn emit-if (var env expr)
      (let ((true-label (unique-label "true"))
            (false-label (unique-label "false"))
            (end-label (unique-label "end"))
            (test-var (generate-var)) (test-res-var (generate-var))
            (res-var (generate-var))
            (res-var1 (generate-var)) (res-var2 (generate-var)))
        (emit-expr test-var env (if-test expr))
        ; TODO: Right now all values but #f are treated as true
        (puts (format "  ~A = icmp eq i64 ~A, ~A" (list test-res-var test-var (immediate-rep #f))))
        (emit-alloca res-var)
        (puts (format "  br i1 ~A, label %~A, label %~A" (list test-res-var false-label true-label)))

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
      (cond ((null? lst) (error "Empty begin" lst))
            ((null? (rst lst)) (emit-expr var env (fst lst)))
            (else
              (emit-expr (generate-var) env (fst lst))
              (emit-begin_ var env (rst lst)))))

(defn emit-let (var env expr) (process-let var expr (let-bindings expr) env))
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
      (let ((tmp (generate-var)))
        (emit-call0 tmp "@internal_heap-current-pointer")
        (emit-string_ str 0 (string-length str))
        (puts "  call i64 @internal_heap-store-byte(i8 0)")
        (puts "  call void @internal_heap-align-index()")
        (puts (format "  ~A = or i64 ~A, 5" (list var tmp)))))

(defn emit-string_ (str idx len)
      ; TODO: Rewrite this once one-armed ifs are supported
      (if (< idx len)
        (begin
          (puts (format "  call i64 @internal_heap-store-byte(i8 ~A)"
                        (list (~>> idx (string-ref str) char->fixnum fixnum->string))))
          (emit-string_ str (add1 idx) len))
        'ok))

(defn emit-main (exprs)
  (for-each emit-global-var global-vars)
  (for-each (fn (expr)
                (emit-lambda (global-var-env) expr))
            lambdas)
  (puts "define i64 @prim_main() {")
  (for-each (fn (expr) (emit-expr (generate-var) (global-var-env) expr))
            exprs)
  (emit-ret (fixnum->string 0))
  (puts "}"))

(defn emit-lib (exprs)
      ; Variables & lambdas are not allowed in libs
      (for-each emit-toplevel-expr exprs))

(defn preprocess (expr) (~> expr desugar alpha-convert closure-convert))

; lib files start w/ a single symbol "lib"
(let ((exprs (read-all)))
  (if (eq? (fst exprs) 'lib)
      (~>> exprs rst (map preprocess) emit-lib)
      (~>> exprs (map preprocess) emit-main)))
