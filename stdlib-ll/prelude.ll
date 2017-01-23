declare i8* @calloc(i32, i32)
declare void @free(i8*)
declare i8 @putchar(i8)

declare void @print_ptr(i64)
declare void @puts_ptr(i64)

@heap_base = global i8* zeroinitializer, align 8
@heap_index = global i64 0, align 8

define i64 @prim_inspect(i64 %a) {
  call void @print_ptr(i64 %a)
  ; TODO: Return empty list or undefined
  ret i64 0
}

define i64 @prim_newline() {
  call i8 @putchar(i8 10)
  ; TODO: Return empty list or undefined
  ret i64 0
}

define i64 @prim_putchar(i64 %a) {
  %raw_value = lshr i64 %a, 3
  %char = trunc i64 %raw_value to i8

  %raw_res = call i8 @putchar(i8 %char)
  %res = zext i8 %raw_res to i64  
  %res2 = shl i64 %res, 3

  ret i64 %res2
}

define i64 @internal_heap-store(i64 %val) {
  %heap_base = load i8*, i8** @heap_base
  %heap_index = load i64, i64* @heap_index

  %raw_heap_ptr = getelementptr i8, i8* %heap_base, i64 %heap_index
  %heap_ptr = bitcast i8* %raw_heap_ptr to i64*

  store i64 %val, i64* %heap_ptr
  %int_ptr = ptrtoint i8* %raw_heap_ptr to i64

  %new_heap_index = add i64 %heap_index, 8
  store i64 %new_heap_index, i64* @heap_index, align 8

  ret i64 %int_ptr
}

define i64 @internal_heap-store-byte(i8 %val) {
  %heap_base = load i8*, i8** @heap_base
  %heap_index = load i64, i64* @heap_index

  %raw_heap_ptr = getelementptr i8, i8* %heap_base, i64 %heap_index

  store i8 %val, i8* %raw_heap_ptr
  %int_ptr = ptrtoint i8* %raw_heap_ptr to i64

  %new_heap_index = add i64 %heap_index, 1
  store i64 %new_heap_index, i64* @heap_index, align 8

  ret i64 %int_ptr
}

define void @internal_heap-align-index() {
  %heap_index = load i64, i64* @heap_index
  ; Get the last 3 bytes of the index
  %rem = and i64 %heap_index, 7
  ; If it is a multiple of 8, those bytes are 0b000

  %test = icmp eq i64 %rem, 0
  br i1 %test, label %true, label %false
  true:
  ret void

  false:
  %tmp = sub i64 %heap_index, %rem
  %new_heap_index = add i64 %tmp, 8
  store i64 %new_heap_index, i64* @heap_index, align 8
  ret void
}
