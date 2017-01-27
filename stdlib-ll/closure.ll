define i64 @internal_make-closure(i64 %function, i64 %arity) {
  %start = call i64 @internal_heap-store(i64 %function)
  call i64 @internal_heap-store(i64 %arity)

  %res = or i64 %start, 4
  ret i64 %res
}

define i64 @prim_closure-arity(i64 %a) {
  %raw_pointer = xor i64 %a, 4
  %pointer = inttoptr i64 %raw_pointer to i64*

  %heap_ptr = getelementptr i64, i64* %pointer, i64 1
  %res = load i64, i64* %heap_ptr

  ret i64 %res
}

define i64 @internal_closure-function(i64 %a) {
  %raw_pointer = xor i64 %a, 4
  %pointer = inttoptr i64 %raw_pointer to i64*

  %heap_ptr = getelementptr i64, i64* %pointer, i64 0
  %res = load i64, i64* %heap_ptr

  ret i64 %res
}
