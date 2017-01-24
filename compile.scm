(include "macros.scm")
(include "stdlib.scm")

(def tag_mask #b111)
(def fixnum_tag #b000)

(def constant_tag #b111)
(def true  #b111111)
(def false #b011111)
(def null  #b000111)

(def wordsize 8)

(def primitives '())

(defn register-primitive (name code)
  (let ((old primitives))
    (set! primitives (cons (list name code) old))))

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
    ((boolean? x)
     (if x true false))
    ((null? x) null)
    (else (error "Invalid expression: " x))))

(include "environment.scm")
(include "helper.scm")
(include "label.scm")
; (include "syntax/derived/and.scm")
; (include "syntax/derived/or.scm")
(include "syntax/if.scm")
(include "syntax/begin.scm")
(include "syntax/cond.scm")
(include "syntax/let.scm")
(include "syntax/string.scm")
(include "syntax/pipe.scm")
(include "syntax/list.scm")

(include "preprocessing/syntax-desugar.scm")
(include "preprocessing/alpha-convert.scm")
(include "preprocessing/a-normalize.scm")
; (include "primitives.scm")
; (include "procedures.scm")
; (include "features/booleans.scm")
; (include "features/pairs.scm")
; (include "features/vectors.scm")
(include "features/fixnums.scm")
; (include "features/chars.scm")
; (include "features/type_conversions.scm")
; (include "features/system.scm")
; (include "features/string.scm")

(defn variable? (expr) (symbol? expr))

(defn emit-label (label)
  (emit label ":"))

(defn emit-comment args
  (apply emit (cons "  # " args)))

(defn emit-stack-load_ (stack-index)
  (emit (mov rax (offset rsp (- stack-index)))))

(defn emit output
  (apply print output))

(defn emit-immediate (var expr)
  (emit (format "  ~A = add i64 ~A, 0" var (immediate-rep expr))))

;;;

(defn defn? (expr) (tagged-list? expr 'defn))
(def defn-name frst)
(def defn-args frrst)
(defn defn-body (expr)
  (if (= 1 (length (rrrst expr)))
      (frrrst expr)
      (cons 'begin (rrrst expr))))

(defn make-defn (name args body)
  (cons 'defn
        (cons name
              (cons args body))))

(defn emit-toplevel-expr (expr)
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

(defn emit-expr (var env expr)
  (cond
    ((immediate? expr)
     (emit-immediate var expr))
    ((string? expr)
     (emit-string var expr))
    ((if? expr)
     (emit-if var env expr))
    ((begin? expr)
     (emit-begin var env expr))
    ; ((and? expr) (emit-expr stack-index env (and->if expr) tail))
    ; ((or? expr) (emit-expr stack-index env (or->if expr) tail))
    ((let? expr) (emit-let var env expr))
    ((let*? expr) (emit-let* var env expr))
    ; ((string? expr) (emit-string stack-index env expr))
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
       (emit (format "  ~A = call i64 @~A(~A)" var (escape name) (string-join2 vars ", ")))
       ))
    ((variable? expr)
     (emit-variable-ref var env expr))
    (else
      (error "Unknown expression: " expr))))


(defn emit-variable-ref (var env expr)
  (let ((var_ (lookup expr env)))
    (if var_
      (emit (format "  ~A = add i64 ~A, 0" var var_))
      (else (error "Reference to unbound variable: " var)))))

(defn string-join2 (lst sep)
  (cond
    ((null? lst) "")
    ((null? (rst lst)) (fst lst))
    (else (string-append
            (string-append (fst lst) sep)
            (string-join2 (rst lst) sep)))))


(defn emit-program (exprs)
  (for-each (fn (expr) (emit-toplevel-expr (preprocess expr)))
            (append stdlib exprs))
)

(defn preprocess (expr)
  (pipe expr
        syntax-desugar
        alpha-convert-expr
        normalize-term))

(def var-counter 0)
(defn generate-var ()
  (begin
    (set! var-counter (add1 var-counter))
    (format "%tmp~A" (sub1 var-counter))))

(emit-program '(
  (defn main ()
        (let ((str "test"))
          (inspect str)
          (inspect (string-length (symbol->string (string->symbol str))))
          (inspect str)
        ))
))
