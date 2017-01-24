declare i8* @calloc(i32, i32)
declare void @free(i8*)
declare i8 @putchar(i8)
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1)

declare void @print_ptr(i64)
declare void @puts_ptr(i64)

@heap_base = global i8* zeroinitializer, align 8
@heap_index = global i64 0, align 8

@symbol_table_base = global i8* zeroinitializer, align 8
@symbol_table_index = global i64 0, align 8

define i64 @scheme_body() {
  %cells = call i8* @calloc(i32 10000, i32 8)
  store i8* %cells, i8** @heap_base, align 8

  %symbols = call i8* @calloc(i32 10000, i32 256)
  store i8* %symbols, i8** @symbol_table_base, align 8

  %res = call i64 @prim_main()
  call void @free(i8* %cells)
  ret i64 %res
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
  %next_symbol_ptr = getelementptr i8, i8* %symbol_table_base, i64 %symbol_table_index

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
  %tmp1 = xor i64 %string, 5
  %tmp2 = inttoptr i64 %tmp1 to i8*
  %tmp3 = call i64 @internal_symbol-create(i8* %tmp2)

  %res = xor i64 %tmp3, 1
  ret i64 %res
}

define i64 @prim_symbol-_greater_string(i64 %symbol) {
  ; Symbols and strings are both just pointers
  ; to some bytes, only the tags are different
  %tmp = xor i64 %symbol, 1
  %res = xor i64 %tmp, 5
  ret i64 %res
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

define i64 @prim_type-tag(i64 %a) {
  %tag = and i64 %a, 7
  %res = shl i64 %tag, 3

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

define i64 @prim_string_questionmark_(i64 %a) {
  %tag = and i64 %a, 7
  %tmp = icmp eq i64 %tag, 5
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_symbol_questionmark_(i64 %a) {
  %tag = and i64 %a, 7
  %tmp = icmp eq i64 %tag, 1
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fixnum_questionmark_(i64 %a) {
  %tag = and i64 %a, 7
  %tmp = icmp eq i64 %tag, 0
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_null_questionmark_(i64 %a) {
  %tmp = icmp eq i64 %a, 7 
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_heap-index() {
  %heap_index = load i64, i64* @heap_index
  %res = shl i64 %heap_index, 3
  ret i64 %res
}
