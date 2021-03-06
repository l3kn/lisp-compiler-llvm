(use shell)
(include "compile.scm")

; (run "make primitives")

(def test-count 0)
(def test-success 0)
(def test-fail 0)

(defn test-program (program expected-result)
  (print "Running test " program)
  (with-output-to-file "body.ll"
                       ; Wrap the program in a main-clause
                       (fn () 
                         (emit-program `((defn main ()
                                           (print (any->string ,program))
                                           (newline)
                                         )))))
  (run "make -s link compile")
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

(defn display-test-stats ()
  (print "Ran " test-count " tests, " test-fail " failed"))

(defn test-programs (lst)
  (for-each (fn (test-case)
              (test-program (fst test-case)
                            (frst test-case)))
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
    '((fx+ 10 20) "30")
    '((fx+ 10 -20) "-10")
    '((fx- 10 20) "-10")
    '((fx- 10 -20) "30")
    '((fx* 1 0) "0")
    '((fx* 12 11) "132")
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
    '((let* ((a 1)
             (b (fxadd1 a))
             (c (fxadd1 b)))
             (fx+ (fx+ a b) c))
      "6")
    ))

; IF

(test-programs
  (list
    '((if #t 1 2) "1")
    '((if #f 1 2) "2")
    '((if #t (if #f 0 1) 2) "1")
    '((if (fxzero? (fx- 5 5)) 0 1) "0")))

; BEGIN

(test-programs
  (list
    '((begin (fx+ 1 2)
             (fx+ 2 3)
             (fx+ 3 4))
      "7")))

; Fixnums

(test-programs
  (list 
    '((fx+ (fx- 1 2) 3) "2")
    '((fx+ 1 (fx+ 2 3)) "6")))

; Pairs & lists

(test-programs
  (list
    '((cons 1 2) "(1 . 2)")
    '((cons 1 (cons 2 3)) "(1 . (2 . 3))")
    '((fst (cons (cons 1 2) (cons 3 4))) "(1 . 2)")
    '((rst (cons (cons 1 2) (cons 3 4))) "(3 . 4)")
  ))

; Strings

(test-programs
  (list
    '((string-length "hello world") "11")
    '((string-eq? "foo" "foo") "#t")
    '((string-eq? "foo" "bar") "#f")
    '((string-eq? "fooo" "foo") "#f")
    '((string-eq? "foo" "fooo") "#f")
    '((eq? "foo" "foo") "#f")
    '((let ((s "foo")) (eq? s s)) "#t")
    '((string-substring "hello world" 0 3) "hel")
    '((string-substring "hello world" 1 3) "el")
    '((string-substring "hello world" 3 3) "")
  ))

; Symbols

(test-programs
  (list
    '((string->symbol "test") "'test")
    '((symbol->string (string->symbol "test")) "test")
    ; Test that symbols have a max length of 31 chars
    '((symbol->string (string->symbol "01234567890123456789012345678901234567890")) "0123456789012345678901234567890")
    '((eq? (string->symbol "foo") (string->symbol "foo")) "#t")

    ; TODO: currently symbols that are longer than 31 chars are broken
    ;'((eq? 'fooooooooooooooooooooooooooooooooooooooooooooooooooooo)
    ;       'fooooooooooooooooooooooooooooooooooooooooooooooooooooo)) "#t")
    '((eq? (string->symbol "foo") (string->symbol "bar")) "#f")
    '((eq? 'foo 'foo) "#t")
    '((eq? 'foo 'bar) "#f")
    ))

; and / or

(test-programs
  (list
    '((and #t #t #t) "#t")
    '((and #t #t #f) "#f")
    '((and #f #f #f) "#f")
    '((or #t #t #t) "#t")
    '((or #t #t #f) "#t")
    '((or #f #f #f) "#f")))

; Chars

(test-programs
  (list
    '((char? #\A) "#t")
    '((eq? #\A #\A) "#t")
    '((eq? #\A #\B) "#f")
    '(#\A "#\\A")
    '((char->fixnum #\A) "65")
    '((fixnum->char 65) "#\\A")
    '((string-ref "hello" 0) "#\\h")
    '((string-ref "hello" 1) "#\\e")
    '((string-ref "hello" 2) "#\\l")
    ))

; read

(test-programs
  (list
    '((read "0") "0")
    '((read "-0") "0")
    '((read "1234") "1234")
    '((read "-1234") "-1234")
    '((read "       \t\n\r  \t-1234") "-1234")
    '((read ";commentfoobar\t\n\r  \t-1234;comment") "-1234")
    '((read "(1 2 3)") "(1 . (2 . (3 . ())))")
    '((read "(foo bar baz)") "('foo . ('bar . ('baz . ())))")
    '((eq? (read "foo") (read "foo")) "#t")
    ))

(display-test-stats)
