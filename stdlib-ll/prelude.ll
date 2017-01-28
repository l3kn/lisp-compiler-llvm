declare i8* @calloc(i32, i32)
declare void @free(i8*)
declare i8 @putchar(i8)
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1)

@heap_base = global i8* zeroinitializer, align 8
@heap_index = global i64 0, align 8

@symbol_table_base = global i8* zeroinitializer, align 8
@symbol_table_index = global i64 0, align 8

define void @main() {
  %cells = call i8* @calloc(i32 200000, i32 8)
  store i8* %cells, i8** @heap_base, align 8

  %symbols = call i8* @calloc(i32 200000, i32 256)
  store i8* %symbols, i8** @symbol_table_base, align 8

  call i64 @prim_main()

  call void @free(i8* %cells)
  call void @free(i8* %symbols)
  ret void
}

; Wrappers for external functions

define i64 @prim_newline() {
  call i8 @putchar(i8 10)
  ret i64 7
}

define i64 @prim_putchar(i64 %a) {
  %raw_value = lshr i64 %a, 3
  %char = trunc i64 %raw_value to i8

  %raw_res = call i8 @putchar(i8 %char)
  %res = zext i8 %raw_res to i64  
  %res2 = shl i64 %res, 3

  ret i64 7
}

; Internal helper functions for working w/ the symbol table

define i64 @internal_symbol-create(i8* %string) {
  %symbol_table_base = load i8*, i8** @symbol_table_base
  %symbol_table_index = load i64, i64* @symbol_table_index

  ; Each entry is 2^5 = 32 bytes long
  %next_symbol_index = shl i64 %symbol_table_index, 5
  %next_symbol_ptr = getelementptr i8, i8* %symbol_table_base, i64 %next_symbol_index

  ; TODO: This is pretty hacky,
  ; if the symbol is < 31 chars long,
  ; we might copy some of the memory after it.
  ; This does not matter to much though,
  ; because there is a \0 somewhere in the middle
  ; TODO: Only works if the symbol table / heap memory is initialized to 0...
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %next_symbol_ptr, i8* %string, i64 31, i32 8, i1 1)

  %tmp = add i64 %symbol_table_index, 1
  store i64 %tmp, i64* @symbol_table_index

  ; All entries in the symbol table are 32=4*8 bytes long => the last bits of the ptr are 0b000

  %res = ptrtoint i8* %next_symbol_ptr to i64
  ret i64 %res
}

define i64 @prim_string-_greater_symbol(i64 %string) {
  %symbol_table_base = load i8*, i8** @symbol_table_base
  %symbol_table_index = load i64, i64* @symbol_table_index

  %tmp1 = xor i64 %string, 5
  %str_ptr = inttoptr i64 %tmp1 to i8*

  %search_index = alloca i64
  store i64 0, i64* %search_index

  ; TODO: This doesn't handle the case where %search_index > %symbol_table_size
  ; or len(%string) > 31
  br label %loop
  loop:
    %cur_search_index = load i64, i64* %search_index
    %reached_end = icmp sge i64 %cur_search_index, %symbol_table_index
    br i1 %reached_end, label %create_new, label %cont
  cont:
    %symbol_index = shl i64 %cur_search_index, 5
    %symbol_ptr = getelementptr i8, i8* %symbol_table_base, i64 %symbol_index

    %tmp2 = add i64 %cur_search_index, 1
    store i64 %tmp2, i64* %search_index

    %found = call i1 @internal_string-cmp(i8* %str_ptr, i8* %symbol_ptr)
    br i1 %found, label %ret_found, label %loop
  ret_found:
    %tmp6 = ptrtoint i8* %symbol_ptr to i64
    %tmp7 = xor i64 %tmp6, 1
    ret i64 %tmp7
  create_new:
    %tmp4 = call i64 @internal_symbol-create(i8* %str_ptr)
    %tmp5 = xor i64 %tmp4, 1
    ret i64 %tmp5
}

define i64 @prim_symbol-_greater_string(i64 %symbol) {
  ; Symbols and strings are both just pointers
  ; to some bytes, only the tags are different
  %tmp = xor i64 %symbol, 1
  %res = xor i64 %tmp, 5
  ret i64 %res
}

; Misc internal helper functions

define i1 @internal_string-cmp(i8* %s1_ptr, i8* %s2_ptr) {
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
  ret i1 false
true:
  ret i1 true
}

; Internal helper functions for working w/ the heap

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

define i64 @internal_heap-current-pointer() {
  %heap_base = load i8*, i8** @heap_base
  %heap_index = load i64, i64* @heap_index

  %raw_heap_ptr = getelementptr i8, i8* %heap_base, i64 %heap_index
  %int_ptr = ptrtoint i8* %raw_heap_ptr to i64
  ret i64 %int_ptr
}

define i64 @internal_heap-store-string(i8* %str_ptr) {
  %index = alloca i64
  store i64 0, i64* %index
  %ptr = call i64 @internal_heap-current-pointer()

  br label %loop
loop:
  %cur_index = load i64, i64* %index
  %char_ptr = getelementptr i8, i8* %str_ptr, i64 %cur_index
  %char = load i8, i8* %char_ptr
  %res = icmp eq i8 %char, 0
  br i1 %res, label %end, label %cont
cont:
  call i64 @internal_heap-store-byte(i8 %char)
  %tmp = add i64 %cur_index, 1
  store i64 %tmp, i64* %index
  br label %loop
end:
  ret i64 %ptr
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

; Type predicates

define i64 @prim___tag(i64 %a) {
  %tag = and i64 %a, 7
  %res = shl i64 %tag, 3
  ret i64 %res
}

define i64 @prim___value(i64 %a) {
  %tmp = lshr i64 %a, 3
  %res = shl i64 %tmp, 3
  ret i64 %res
}

define i64 @prim___heap-index() {
  %heap_index = load i64, i64* @heap_index
  %res = shl i64 %heap_index, 3
  ret i64 %res
}
