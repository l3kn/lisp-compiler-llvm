define i64 @prim_heap-index() {
  %heap_index = load i64, i64* @heap_index
  %res = shl i64 %heap_index, 3
  ret i64 %res
}

define i64 @prim_cons(i64 %a, i64 %b) {
  %start = call i64 @internal_heap-store(i64 %a)
  call i64 @internal_heap-store(i64 %b)

  %res = or i64 %start, 6
  ret i64 %res
}

define i64 @prim_pair_questionmark_(i64 %a) {
  %tag = and i64 %a, 7
  %tmp = icmp eq i64 %tag, 6
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fst(i64 %a) {
  ; Remove the pair tag 0b110
  %raw_pointer = xor i64 %a, 6
  %pointer = inttoptr i64 %raw_pointer to i8*

  %raw_heap_ptr = getelementptr i8, i8* %pointer, i64 0
  %heap_ptr = bitcast i8* %raw_heap_ptr to i64*
  %res = load i64, i64* %heap_ptr

  ret i64 %res
}

define i64 @prim_rst(i64 %a) {
  ; Remove the pair tag 0b110
  %raw_pointer = xor i64 %a, 6
  %pointer = inttoptr i64 %raw_pointer to i8*

  %raw_heap_ptr = getelementptr i8, i8* %pointer, i64 8
  %heap_ptr = bitcast i8* %raw_heap_ptr to i64*
  %res = load i64, i64* %heap_ptr

  ret i64 %res
}
