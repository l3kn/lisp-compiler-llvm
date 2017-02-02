# LISP Compiler

## Example Code

``` scheme
(defn range (from to)
      (if (eq? from to)
          (list from)
          (cons from
                (range (fxadd1 from) to))))
(defn fizzbuzz (n)
      (cond
        ((eq? (fxrem n 15) 0) (puts "FizzBuzz"))
        ((eq? (fxrem n 3) 0) (puts "Fizz"))
        ((eq? (fxrem n 5) 0) (puts "Buzz"))
        (else (inspect n))))

(for-each fizzbuzz (range 1 100))
```

## Getting Started

1. Use chicken-scheme to build the compiler & the stdlib
2. Compile some program to LLVM-IR
3. Combine it w/ the stdlib files
4. Run it

``` bash
make bootstrap
./compiler < programs/fizzbuzz.csm > fizzbuzz-body.ll 
cat stdlib-ll/*.ll stdlib.ll fizzbuzz-body.ll > fizzbuzz.ll
lli fizzbuzz.ll
```

## Design Decisions

* 64bit only
* Tagged Pointers for values, 3bit tag, 61bit value
  * 000: Integer
  * 001: Symbol
  * 010: Char
  * 100: Closure
  * 101: String
  * 110: Pair
  * 111: Hardcoded primitives, #t, #f, '() 
* Symbols are limited to 31 chars

## Limitations

* Not compliant to any standart
* No GC => compiling itself uses ~1GB of RAM

## Project Structure

* `compiler.scm`
  Main compiler logic
* `helper.scm`
  Helper functions that are not part of the stdlib
* `llvm.scm`
  Helper functions for outputting LLVM-IR code
* `reader.scm`
  Recursive descent parser for the input language
* `syntax.scm`
  Defines the syntax of the input language
* `preprocessing/
  * `desugar.scm`
    Convert `cond` to `if` etc.
  * `alpha-convert.scm`
    Rename variables to prevent naming collisions
  * `closure-convert.scm`
    Convert `fn`s to closures & lambdas w/o free variables
* `compatibility.scm`
  Some macros & functions to make chicken scheme understand the compiler source code
* `stdlib-ll/`
  Collection of low-level stdlib functions written directly in LLVM-IR

## Special Syntax

* `~>` Thread-first macro like in clojure
* `~>>` Thread-last macro like in clojure

## Links

* <http://matt.might.net/articles/a-normalization/>
* <http://matt.might.net/articles/desugaring-scheme/>
* <http://matt.might.net/articles/closure-conversion/>
* <http://www.wilfred.me.uk/blog/2015/02/21/my-first-llvm-compiler/>
