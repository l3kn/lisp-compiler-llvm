define i64 @prim_cons(i64 %a, i64 %b) {
  %start = call i64 @internal_heap-store(i64 %a)
  call i64 @internal_heap-store(i64 %b)

  %res = or i64 %start, 6
  ret i64 %res
}

define i64 @prim_fst(i64 %a) {
  ; Remove the pair tag 0b110
  %raw_pointer = xor i64 %a, 6
  %pointer = inttoptr i64 %raw_pointer to i64*

  %heap_ptr = getelementptr i64, i64* %pointer, i64 0
  %res = load i64, i64* %heap_ptr

  ret i64 %res
}

define i64 @prim_rst(i64 %a) {
  ; Remove the pair tag 0b110
  %raw_pointer = xor i64 %a, 6
  %pointer = inttoptr i64 %raw_pointer to i64*

  %heap_ptr = getelementptr i64, i64* %pointer, i64 1
  %res = load i64, i64* %heap_ptr

  ret i64 %res
}

define i64 @prim_list-ref(i64 %idx, i64 %lst) {
  %tmp1 = icmp eq i64 %idx, 0
  br i1 %tmp1, label %exit, label %next
  exit:
    %tmp2 = call i64 @prim_fst(i64 %lst)
    ret i64 %tmp2
  next:
    %tmp3 = call i64 @prim_rst(i64 %lst)
    ; -8 because of the 3 tag bits
    %tmp4 = call i64 @prim_fxsub1(i64 %idx)
    %tmp5 = call i64 @prim_list-ref(i64 %tmp4, i64 %lst)
    ret i64 %tmp5
}
