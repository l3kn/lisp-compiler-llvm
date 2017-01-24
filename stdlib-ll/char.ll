define i64 @prim_char-_greater_string(i64 %char) {
  %tmp1 = xor i64 %char, 2
  %tmp2 = lshr i64 %tmp1, 3
  %tmp3 = trunc i64 %tmp2 to i8

  %tmp4 = call i64 @internal_heap-store-byte(i8 %tmp3)
  call i64 @internal_heap-store-byte(i8 0)
  call void @internal_heap-align-index()

  %res = xor i64 %tmp4, 5
  ret i64 %res
}

define i64 @prim_char-_greater_fixnum(i64 %char) {
  %tmp1 = xor i64 %char, 2
  ret i64 %tmp1
}

define i64 @prim_fixnum-_greater_char(i64 %char) {
  %tmp1 = xor i64 %char, 2
  ret i64 %tmp1
}
