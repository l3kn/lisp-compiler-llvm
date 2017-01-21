# LISP to LLVM-IR compiler

## Design Decisions

* 64bit
* Tagged Pointers for values, 4bit tag, 60bit value
  * 0000: Integer
  * 1111: Hardcoded primitives, #t, #f, '() 

