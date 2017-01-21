(use shell)
(include "compile.scm")

(run "make primitives")

(define test-count 0)
(define test-success 0)
(define test-fail 0)

(define (test-program program expected-result)
  (with-output-to-file "body.ll" (lambda () (emit-program program)))
  (run "make link compile")
  (let* ((raw-result (capture "./output"))
         (result (if (>= (string-length raw-result) 1)
                     (string-copy raw-result 0 (sub1 (string-length raw-result)))
                     "")))
    (set! test-count (add1 test-count))
    (if (equal? result expected-result)
        (set! test-success (add1 test-success))
        (begin
          (print "Test " program " failed: expected " expected-result ", got " result)
          (set! test-fail (add1 test-fail))))))

(define (display-test-stats)
  (print "Ran " test-count " tests, " test-fail " failed"))

(define (test-programs lst)
  (for-each (lambda (test-case)
              (test-program (car test-case)
                            (cadr test-case)))
            lst))

; Integer tests

(test-programs
  (list
    (list 0 "0")
    (list 1 "1")
    (list -1 "-1")
    (list -10 "-10")
    (list 123456 "123456")))

; Immediates

(test-programs
  (list
    (list #t "#t")
    (list #f "#f")
    (list '() "()")))

; Fixnums

(test-programs
  (list
    '((fxadd1 0) "1")
    '((fxsub1 0) "-1")
    '((fxzero? 0) "#t")
    '((fxzero? -100) "#f")
    '((fxzero? 100) "#f")
    ))

; Booleans

(test-programs
  (list
    '((not #t) "#f")
    '((not #f) "#t")
    ))

; LET

(test-programs
  (list
    '((let ((a 1) (b (fxadd1 1)))
           (fx+ a b))
      "3")
    '((let ((x 2))
        (let ((x (fx+ x x)))
          (let ((x (fx+ x x)))
            (let ((x (fx+ x x)))
              (fx+ x x)))))
      "32")
    '((let ((x 2) (y 2) (z 2))
        (let ((y (fx+ x x))
              (z (fx+ y y)))
          z))
      "4")
    '((let ((x 2) (y 2) (z 2))
        (let* ((y (fx+ x x))
               (z (fx+ y y)))
          z))
      "8")
    ))
;     '((let* ((a 1)
;              (b (fxadd1 a))
;              (c (fxadd1 b)))
;              (fx+ (fx+ a b) c))
;       "6")
;     ))

; IF

; (test-programs
;   (list
;     '((if #t 1 2) "1")
;     '((if #f 1 2) "2")
;     '((if #t (if #f 0 1) 2) "1")
;     '((if (fxzero? (fx- 5 5)) 0 1) "0")))

; BEGIN

; (test-programs
;   (list
;     '((begin (fx+ 1 2)
;              (fx+ 2 3)
;              (fx+ 3 4))
;       "7")))

; Fixnums

; (test-programs
;   (list 
;     '((fx+ (fx- 1 2) 3) "2")
;     '((fx+ 1 (fx+ 2 3)) "6")))

; Pairs

; (test-programs
;   (list
;     '((cons 1 2) "(1 . 2)")
;     '((cons 1 (cons 2 3)) "(1 . (2 . 3))")
;   ))

(display-test-stats)
