# LISP to LLVM-IR compiler

## Design Decisions

* 64bit only
* Tagged Pointers for values, 4bit tag, 60bit value
  * 000: Integer
  * 111: Hardcoded primitives, #t, #f, '() 

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

## Links

* <http://matt.might.net/articles/a-normalization/>
