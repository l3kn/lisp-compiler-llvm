(include "compatibility.scm")
(include "stdlib.scm")

(def tag_mask #b111)
(def fixnum_tag #b000)

(def char_tag #b010)
(def constant_tag #b111)
(def true  #b111111)
(def false #b011111)
(def null  #b000111)

(defn arg-str (arity)
      (cond 
        ((eq? arity 0) "()")
        ((eq? arity 1) "(i64)")
        ((eq? arity 2) "(i64, i64)")
        ((eq? arity 3) "(i64, i64, i64)")
        ((eq? arity 4) "(i64, i64, i64, i64)")
        ((eq? arity 5) "(i64, i64, i64, i64, i64)")
        ((eq? arity 6) "(i64, i64, i64, i64, i64, i64)")
        ((eq? arity 7) "(i64, i64, i64, i64, i64, i64, i64)")
                       ))

(defn immediate? (x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(defn atomic? (x)
  (or (immediate? x)
      (symbol? x)
      (string? x)))

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

(include "environment.scm")
(include "helper.scm")
(include "label.scm")
(include "syntax/if.scm")
(include "syntax/begin.scm")
(include "syntax/let.scm")
(include "syntax/string.scm")
(include "preprocessing/syntax-desugar.scm")
(include "preprocessing/alpha-convert.scm")
(include "preprocessing/a-normalize.scm")
(include "preprocessing/closure-convert.scm")

(defn variable? (expr) (symbol? expr))

(defn emit-label (label)
  (emit label ":"))

(defn emit-comment args
  (apply emit (cons "  # " args)))

(defn emit output
  (apply print output))

(defn emit-immediate (var expr)
  (let ((tmp (generate-var)))
    (emit (format "  ~A = alloca i64" tmp))
    (emit (format "  store i64 ~A, i64* ~A" (immediate-rep expr) tmp))
    (emit (format "  ~A = load i64, i64* ~A" var tmp))))

;;;

(defn defn? (expr) (tagged-list? expr 'defprim))
(def defn-name frst)
(def defn-args frrst)
(defn defn-body (expr)
  (if (= 1 (length (rrrst expr)))
      (frrrst expr)
      (cons 'begin (rrrst expr))))

(defn make-defn (name args body)
  (cons 'defprim
        (cons name
              (cons args body))))

(defn emit-toplevel-expr (expr)
  ; (pp expr))
  (cond
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            (args-with-vars (map (fn (arg) (cons arg (generate-var))) args))
            (args-string
              (string-join2 (map (fn (a) (string-append "i64 " (rst a))) args-with-vars) ", ")))
       (emit (format "define i64 @~A(~A) {" (escape name) args-string))
       (emit-expr "%res" args-with-vars (defn-body expr))
       (emit (format "  ret i64 %res"))
       (emit (format "}"))))
     (else (error "Invalid toplevel expression: " expr))))

(defn emit-lambda (expr)
  (let* ((name (fst expr))
         (lmbda (frst expr))
         (args (lambda-args lmbda))
         (body (lambda-body lmbda))
         (prep-body (pipe body alpha-convert-expr normalize-term))
         (args-with-vars (map (fn (arg) (cons arg (generate-var))) args))
         (args-string
           (string-join2 (map (fn (a) (string-append "i64 " (rst a))) args-with-vars) ", ")))
    (emit (format "define i64 @lambda_~A(~A) {" name args-string))
    (emit-expr "%res" args-with-vars prep-body)
    (emit (format "  ret i64 %res"))
    (emit (format "}"))))

(defn emit-global-var (var)
      (emit (format "@var_~A = weak global i64 0" (escape var))))

(defn emit-expr (var env expr)
  (cond
    ((immediate? expr)
     (emit-immediate var expr))
    ((string? expr)
     (emit-string var expr))
    ((tagged-list? expr 'quote)
     (emit-symbol var (frst expr)))
    ((if? expr)
     (emit-if var env expr))
    ((begin? expr)
     (emit-begin var env expr))
    ((let? expr) (emit-let var env expr))
    ((tagged-list? expr 'closure)
     (let* ((tmp (generate-var))
            (arity (frrst expr)))
       (emit (format "  ~A = ptrtoint i64 ~A* @lambda_~A to i64" tmp (arg-str arity) (frst expr)))
       (emit (format "  ~A = call i64 @internal_make-closure(i64 ~A, i64 ~A)" var tmp (immediate-rep arity)))
       ))
    ((tagged-list? expr 'def)
     (let ((tmp (generate-var))
           (name (frst expr))
           (value (frrst expr)))
       (emit-expr tmp env value)
       (emit (format "store i64 ~A, i64* @var_~A" tmp (escape name)))))
    ((list? expr)
     (let* ((name (fst expr))
            (args (rst expr))
            (vars (map (fn (arg)
                         (string-append
                           "i64 "
                           (let ((res (assoc arg env)))
                             (if res
                                 (rst res)
                                 (show (immediate-rep arg))))))
                       args)))
       (if (primitive? name)
           (emit (format "  ~A = call i64 @~A(~A)" var (escape name) (string-join2 vars ", ")))
           (begin
             (let* ((tmp1 (generate-var))
                   (tmp2 (generate-var))
                   (tmp3 (generate-var))
                   (arity (length args)))
                   (emit-variable-ref tmp1 env name)
                   (emit (format "  ~A = call i64 @internal_closure-function(i64 ~A)" tmp2 tmp1))
                   (emit (format "  ~A = inttoptr i64 ~A to i64 ~A*" tmp3 tmp2 (arg-str arity)))
                   (emit (format "  ~A = call i64 ~A(~A)" var tmp3 (string-join2 vars ", ")))
                   )
             ))
       ))
    ((variable? expr)
     (emit-variable-ref var env expr))
    (else
      (error "Unknown expression: " expr))))

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
                    begin
                    )))

(defn emit-variable-ref (var env expr)
  (let ((var_ (lookup-or expr #f env)))
    (if var_
      (let ((tmp (generate-var)))
        (emit (format "  ~A = alloca i64" tmp))
        (emit (format "  store i64 ~A, i64* ~A" var_ tmp))
        (emit (format "  ~A = load i64, i64* ~A" var tmp)))
      (let ((tmp (generate-var)))
        (emit (format "  ~A = load i64, i64* @var_~A" var (escape expr)))))))
      ; (error "Reference to unbound variable: " var))))

(defn emit-symbol (var expr)
  (let ((tmp (generate-var)))
    (emit-string tmp (symbol->string expr))
    (emit (format "  ~A = call i64 @prim_string-_greater_symbol(i64 ~A)" var tmp))))

(defn string-join2 (lst sep)
  (cond
    ((null? lst) "")
    ((null? (rst lst)) (fst lst))
    (else (string-append
            (string-append (fst lst) sep)
            (string-join2 (rst lst) sep)))))

(defn emit-program (exprs)
  ; (for-each (fn (expr)
  ;               (pipe expr syntax-desugar closure-convert alpha-convert-expr normalize-term emit-toplevel-expr))
  ;           stdlib)
  (let ((preprocessed (map (fn (expr)
                               (pipe expr syntax-desugar closure-convert alpha-convert-expr normalize-term))
                               ; (pipe expr syntax-desugar closure-convert))
                           (append stdlib exprs))))
    (for-each emit-global-var global_vars)
    (for-each emit-lambda lambdas)
    (emit "define i64 @prim_main() {")

    ; (for-each (fn (expr) (print expr)) preprocessed)
    (for-each (fn (expr) (emit-expr (generate-var) empty-env expr)) preprocessed)
    (emit "  ret i64 0")
    (emit "}")
    ; (for-each (fn (expr) (emit-toplevel-expr (preprocess expr)))
              ; (append stdlib exprs))
              ))

(defn preprocess (expr)
  (pipe expr
        alpha-convert-expr
        normalize-term
        ))

(def var-counter 0)
(defn generate-var ()
  (begin
    (set! var-counter (add1 var-counter))
    (format "%tmp~A" (sub1 var-counter))))

(emit-program '(
  (def a 10)
  (defn fib (n)
        (if (fx<=? n 1) n (fx+ (fib (fx- n 1))
                               (fib (fx- n 2)))))
  (inspect fib)
))


; (print lambdas)

; (emit-program '(
;   (defn main ()
;           ; (inspect (__heap-index))
;           (inspect (read "-12"))
;           ; (inspect (eq? (string->symbol "foo") (string->symbol "foo")))
;           ; (inspect (string-length (symbol->string (string->symbol str))))
;           ; (inspect (__heap-index))
;   )))
