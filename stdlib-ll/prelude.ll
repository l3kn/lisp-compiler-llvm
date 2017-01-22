declare i8* @calloc(i32, i32)
declare void @free(i8*)
declare void @print_ptr(i64, i8*)
declare void @puts_ptr(i64, i8*)

@heap_base = global i64* zeroinitializer, align 8
@raw_heap_base = global i8* zeroinitializer, align 8
@heap_index = global i64 0, align 8

define i64 @prim_print(i64 %a) {
  %base = load i8*, i8** @raw_heap_base
  call void @print_ptr(i64 %a, i8* %base)
  ; TODO: Return empty list or undefined
  ret i64 0
}

define i64 @prim_puts(i64 %a) {
  %base = load i8*, i8** @raw_heap_base
  call void @puts_ptr(i64 %a, i8* %base)
  ; TODO: Return empty list or undefined
  ret i64 0
}
