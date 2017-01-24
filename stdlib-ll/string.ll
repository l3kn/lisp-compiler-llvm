define i64 @prim_string-length(i64 %a) {
  ; Remove the string tag 0b101
  %raw_pointer = xor i64 %a, 5
  %str_ptr = inttoptr i64 %raw_pointer to i8*

  %len = alloca i64
  store i64 0, i64* %len
  br label %loop
loop:
  %cur_index = load i64, i64* %len
  %char_ptr = getelementptr i8, i8* %str_ptr, i64 %cur_index
  %char = load i8, i8* %char_ptr
  %res = icmp eq i8 %char, 0
  br i1 %res, label %end, label %cont
cont:
  %tmp = add i64 %cur_index, 1
  store i64 %tmp, i64* %len
  br label %loop
end:
  %tmp2 = load i64, i64* %len
  %tmp3 = shl i64 %tmp2, 3
  ret i64 %tmp3
}

define i64 @prim_string-append(i64 %a, i64 %b) {
  %raw_pointer1 = xor i64 %a, 5
  %raw_pointer2 = xor i64 %b, 5

  %str_ptr1 = inttoptr i64 %raw_pointer1 to i8*
  %str_ptr2 = inttoptr i64 %raw_pointer2 to i8*

  %new_ptr = call i64 @internal_heap-current-pointer()
  call i64 @internal_heap-store-string(i8* %str_ptr1)
  call i64 @internal_heap-store-string(i8* %str_ptr2)
  call i64 @internal_heap-store-byte(i8 0)
  call void @internal_heap-align-index()

  %res = or i64 %new_ptr, 5
  ret i64 %res
}

define i64 @prim_print(i64 %a) {
  %raw_pointer = xor i64 %a, 5
  %str_ptr = inttoptr i64 %raw_pointer to i8*

  %index = alloca i64
  store i64 0, i64* %index

  br label %loop
loop:
  %cur_index = load i64, i64* %index
  %char_ptr = getelementptr i8, i8* %str_ptr, i64 %cur_index
  %char = load i8, i8* %char_ptr
  %res = icmp eq i8 %char, 0
  br i1 %res, label %end, label %cont
cont:
  call i8 @putchar(i8 %char)
  %tmp2 = add i64 %cur_index, 1
  store i64 %tmp2, i64* %index
  br label %loop
end:
  ret i64 0
}

define i64 @prim_digit-_greater_string(i64 %digit) {
  %ptr = call i64 @internal_heap-current-pointer()
  %tmp = lshr i64 %digit, 3
  %tmp2 = add i64 %tmp, 48 
  %tmp3 = trunc i64 %tmp2 to i8

  call i64 @internal_heap-store-byte(i8 %tmp3)
  call i64 @internal_heap-store-byte(i8 0)
  call void @internal_heap-align-index()

  %res = or i64 %ptr, 5
  ret i64 %res
}

define i64 @prim_string-eq_questionmark_(i64 %s1, i64 %s2) {
  %s1_raw = xor i64 %s1, 5
  %s2_raw = xor i64 %s2, 5

  %s1_ptr = inttoptr i64 %s1_raw to i8*
  %s2_ptr = inttoptr i64 %s2_raw to i8*

  %index = alloca i64
  store i64 0, i64* %index
  br label %loop
loop:
  %cur_index = load i64, i64* %index

  %char1_ptr = getelementptr i8, i8* %s1_ptr, i64 %cur_index
  %char1 = load i8, i8* %char1_ptr
  %char2_ptr = getelementptr i8, i8* %s2_ptr, i64 %cur_index
  %char2 = load i8, i8* %char2_ptr

  ; c1 == c2 == 0 => end, true 
  %any = or i8 %char1, %char2
  %tmp1 = icmp eq i8 %any, 0
  br i1 %tmp1, label %true, label %cont1
cont1:
  ; c1 == c2 => continue
  %tmp2 = icmp eq i8 %char1, %char2
  br i1 %tmp2, label %cont2, label %cont3
cont2:
  %tmp3 = add i64 %cur_index, 1
  store i64 %tmp3, i64* %index
  br label %loop
cont3:
  ; else => false
  ret i64 31 ; false
true:
  ret i64 63
}
