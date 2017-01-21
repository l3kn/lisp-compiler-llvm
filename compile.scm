(define tag_mask #b111)
(define fixnum_tag #b000)

(define constant_tag #b111)
(define true  #b111111)
(define false #b011111)
(define null  #b000111)

(define wordsize 8)

(define primitives '())

(define (register-primitive name code)
  (let ((old primitives))
    (set! primitives (cons (list name code) old))))

(define (tagged-list? expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))

(define (string-join lst) (foldl string-append "" lst))
(define (show . args) (string-join (map ->string args)))

(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(define (immediate-rep x)
  (cond
    ((fixnum? x) 
     (fxshl x 3))
    ((boolean? x)
     (if x true false))
    ((null? x) null)
    (else (error "Invalid expression: " x))))

(include "environment.scm")
; (include "asm.scm")
(include "label.scm")
; (include "syntax/derived/and.scm")
; (include "syntax/derived/or.scm")
(include "syntax/if.scm")
(include "syntax/begin.scm")
(include "syntax/let.scm")
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

(define (variable? expr) (symbol? expr))

(define (emit-label label)
  (emit label ":"))

(define (emit-comment . args)
  (apply emit (cons "  # " args)))

(define (emit-stack-load_ stack-index)
  (emit (mov rax (offset rsp (- stack-index)))))

(define (emit . output)
  (apply print output))

(define (emit-immediate var expr)
  (emit (format "  ~A = add i64 ~A, 0" var (immediate-rep expr))))

(define (emit-expr var env expr)
  (cond
    ((immediate? expr)
     (emit-immediate var expr))
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
     (let* ((name (car expr))
            (args (cdr expr))
            (args_with_vars (map (lambda (arg) (cons arg (generate-var))) args))
            (vars (map cdr args_with_vars)))
       (for-each (lambda (arg_var)
                   (let ((arg (car arg_var))
                         (var (cdr arg_var)))
                     (emit-expr var env arg)))
                  args_with_vars)
       (emit (format "  ~A = call i64 @~A(i64 ~A)" var (escape name) (string-join2 vars ", i64 ")))
       ))
    ((variable? expr)
     (emit-variable-ref var env expr))
    (else
      (error "Unknown expression: " expr))))


(define (emit-variable-ref var env expr)
  (let ((var_ (lookup expr env)))
    (if var_
      (emit (format "  ~A = add i64 ~A, 0" var var_))
      (else (error "Reference to unbound variable: " var)))))

(define (string-join2 lst sep)
  (cond
    ((null? lst) "")
    ((null? (cdr lst)) (car lst))
    (else (string-append
            (string-append (car lst) sep)
            (string-join2 (cdr lst) sep)))))

(define (emit-program expr)
  (emit "define i64 @scheme_body() {")
  (emit-expr "%res" empty-env expr)
  (emit "  ret i64 %res")
  (emit "}"))

(define var-counter 0)
(define (generate-var)
  (begin
    (set! var-counter (add1 var-counter))
    (format "%tmp~A" (sub1 var-counter))))

(define (escape str)
  (let ((parts (map ->string (string->list (->string str)))))
    (string-join
      (cons "prim_"
        (map
          (lambda (part)
            (cond
              ((equal? part "+") "_plus_")
              ((equal? part "-") "_minus_")
              ((equal? part ">") "_greater_")
              ((equal? part "<") "_less_")
              ((equal? part "=") "_equal_")
              ((equal? part "*") "_times_")
              ((equal? part "?") "_questionmark_")
              (else part)))
          parts)))))

; (emit-program '(fx+ (fx+ 1 2) (fx+ 3 4)))
; (emit-program '(if (fx= 2 1) 10 20))
; (emit-program '(let ((x 1)) (fx+ x y)))
