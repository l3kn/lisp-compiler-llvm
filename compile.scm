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

;;;

(define (defn? expr) (tagged-list? expr 'defn))
(define defn-name cadr)
(define defn-args caddr)
(define (defn-body expr)
  (cons 'begin (cdddr expr)))

(define (emit-toplevel-expr expr)
  (cond
    ((defn? expr)
     (let* ((name (defn-name expr))
            (args (defn-args expr))
            (args-with-vars (map (lambda (arg) (cons arg (generate-var))) args))
            (args-string
              (string-join2 (map (lambda (a) (string-append "i64 " (cdr a))) args-with-vars) ", ")))
       (emit (format "define i64 @~A(~A) {" (escape name) args-string))
       (emit-expr "%res" args-with-vars (defn-body expr))
       (emit (format "  ret i64 %res"))
       (emit (format "}"))))
     (else (error "Invalid toplevel expression: " expr))))

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
            (args-with-vars (map (lambda (arg) (cons arg (generate-var))) args))
            (vars (map cdr args-with-vars)))
       (for-each (lambda (arg_var)
                   (let ((arg (car arg_var))
                         (var (cdr arg_var)))
                     (emit-expr var env arg)))
                  args-with-vars)
       ; TODO: this will fail if the fn has no args
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
  ; Allocate 10k 64bit cells to use as heap 
  ; and store the pointer to the base in "@heap_base"
  (emit "  %raw_cells = call i8* @calloc(i32 80000, i32 2)")
  (emit "  store i8* %raw_cells, i8** @raw_heap_base, align 8")
  (emit "  %cells = bitcast i8* %raw_cells to i64*")
  (emit "  store i64* %cells, i64** @heap_base, align 8")

  (emit "  %res = call i64 @prim_main()")

  ; (emit-expr "%res" empty-env expr)

  ; Free the heap
  (emit "  call void @free(i8* %raw_cells)")
  (emit "  %foo = call i64 @prim_puts(i64 %res)")
  (emit "  ret i64 %res")
  (emit "}")

  (for-each emit-toplevel-expr expr)
)

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

(emit-program '(
  (defn fib (n)
        (if (fx<=? n 1)
            n
            (fx+ (fib (fx- n 1))
                 (fib (fx- n 2)))))
  (defn main ()
    (fib 40))
))
; (emit-program '(
;   (defn fib (n)
;         (fib_helper 0 1 0 n))
;   (defn fib_helper (a b cur goal)
;         (if (fx=? cur goal)
;             a
;             (fib_helper b (fx+ a b) (fxadd1 cur) goal)))
;   (defn main ()
;     (fib 40))
; ))

; (emit-program '(fx+ (fx+ 1 2) (fx+ 3 4)))
; (emit-program '(begin (puts 1) (puts 2) (puts 3)))
; (emit-program '(if (fx= 2 1) 10 20))
; (emit-program '(let ((x 1)) (fx+ x y)))
