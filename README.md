# LISP to LLVM-IR compiler

## Design Decisions

* 64bit only
* Tagged Pointers for values, 4bit tag, 60bit value
  * 000: Integer
  * 101: Pair
  * 110: Pair
  * 111: Hardcoded primitives, #t, #f, '() 

## Special Syntax

* `(pipe var fn1 fn2 fn3)` = `(fn3 (fn2 (fn1 var)))`
* `(rpipe var fn1 fn2 fn3)` = `(fn1 (fn2 (fn3 var)))`

## Preprocessing

### TODO: Convert multi-expr body to `begin`

### Syntax Desugaring

#### Convert `let*` to nested `let`s

In:

``` lisp
(let* ((x 1)
       (y (+ x x))
       (z (+ y y)))
 (+ z z))))
```

Out:

``` lisp
(let ((x 1))
  (let ((y (+ x x)))
    (let ((z (+ y y)))
     (+ z z))))
```

### Alpha-Conversion

Rename variables to unique symbols

In:

``` lisp
(let ((x 1)
      (y 2))
 (let ((x (+ x x)))
  (* x y)))))
```

Out:

``` lisp
(let ((g246 1)
      (g247 2))
  (let ((g248 (+ g246 g246)))
    (* g248 g247)))
```

### A-Normalization 

Convert functions applications to use vars only.

### Full example

In:

``` lisp
(defn fib (n)
      (if (fx<=? n 1)
          n
          (fx+ (fib (fx- n 1))
               (fib (fx- n 2)))))))
```

Out:

``` lisp
(defn fib (n)
      (let ((test (fx<=? n 1)))
        (if test
            n
            (let ((a (fx- n 1)))
              (let ((fib_a (fib a)))
                (let ((b (fx- n 2)))
                  (let ((fib_b (fib b)))
                    (fx+ fib_a fib_b))))))))
```

## Code sample

``` lisp
  (defn fib (n)
        (if (fx<=? n 1)
            n
            (fx+ (fib (fx- n 1))
                 (fib (fx- n 2)))))
  (defn main ()
    (fib 40))
```

## Functions

* `(string-length str)`
* `(string-append str1, str2)`
* `(digit->string fx)` (`0` -> `"0", ..., `9` -> `"9"`)
* `(fixnum->string fx)`

* `(fx+ fx1, fx2)`
* `(fx- fx1, fx2)`
* `(fxadd1 fx)`
* `(fxsub1 fx)`
* `(fxzero? fx)`

* `(print str)`
* `(inspect value)`
* `(newline)`

* `(eq? v1 v2)` (test equivalence of immediate values (`#t`, `#f`, `()`, fixnums) and pointers)

## Links

* <http://matt.might.net/articles/a-normalization/>
