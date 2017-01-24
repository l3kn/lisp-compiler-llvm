(define fixnum_bits 61)
(define fixnum_lower (- (expt 2 (- fixnum_bits 1))))
(define fixnum_upper (sub1 (expt 2 (- fixnum_bits 1))))

(define (fixnum? x)
  (and (integer? x)
       (<= fixnum_lower x fixnum_upper)))

; (register-primitive 'fxlognot
;       (lambda (stack-index env args)
;         (let ((arg (car args)))
;           (emit-expr stack-index env arg #f)
;           (print "  shr rax, " fixnum_shift)
;           (print "  not rax")
;           (print "  shl rax, " fixnum_shift))))

; (register-primitive 'fxlogand
;       (lambda (stack-index env args)
;         (emit-binop stack-index env (car args) (cadr args) "and")))
; (register-primitive 'fxlogor
;       (lambda (stack-index env args)
;         (emit-binop stack-index env (car args) (cadr args) "or")))

; (register-primitive 'fx*
;       (lambda (stack-index env args)
;         (let ((arg1 (car args))
;               (arg2 (cadr args)))
;           (emit-expr stack-index env arg2 #f)
;           (print "  shr rax, " fixnum_shift)
;           (print "  push rax")
;           (emit-expr (next-stack-index stack-index) env arg1 #f)
;           (print "  shr rax, " fixnum_shift)
;           (print "  pop r11")
;           (print "  imul rax, r11")
;           (print "  shl rax, " fixnum_shift))))

; ; TODO: implement division the right way

; (register-primitive 'fx=?
;       (lambda (stack-index env args)
;         (emit-comparison stack-index env (car args) (cadr args))
;         (emit-test stack-index env "je")))
; (register-primitive 'fx<?
;       (lambda (stack-index env args)
;         (emit-comparison stack-index env (car args) (cadr args))
;         (emit-test stack-index env "jl")))
; (register-primitive 'fx<=?
;       (lambda (stack-index env args)
;         (emit-comparison stack-index env (car args) (cadr args))
;         (emit-test stack-index env "jlt")))
; (register-primitive 'fx>?
;       (lambda (stack-index env args)
;         (emit-comparison stack-index env (car args) (cadr args))
;         (emit-test stack-index env "jg")))
; (register-primitive 'fx>=?
;       (lambda (stack-index env args)
;         (emit-comparison stack-index env (car args) (cadr args))
;         (emit-test stack-index env "jgt")))
