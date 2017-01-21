# LISP to LLVM-IR compiler

## Design Decisions

* 64bit
* Tagged Pointers for values, 4bit tag, 60bit value
  * 000: Integer
  * 111: Hardcoded primitives, #t, #f, '() 

  1. Fixnum
  2. String
  3. Pair
  4. Vector
  5. Primitives
  6. Closure

