# LISP to LLVM-IR compiler

## Design Decisions

* 64bit only
* Tagged Pointers for values, 4bit tag, 60bit value
  * 000: Integer
  * 111: Hardcoded primitives, #t, #f, '() 

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
