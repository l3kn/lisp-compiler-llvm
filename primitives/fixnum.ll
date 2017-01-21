@TRUE = constant i64 63, align 8
@FALSE = constant i64 31, align 8

define i64 @prim_fx_plus_(i64 %a, i64 %b) {
  %tmp = add i64 %a, %b
  ret i64 %tmp
}

define i64 @prim_fx_minus_(i64 %a, i64 %b) {
  %tmp = sub i64 %a, %b
  ret i64 %tmp
}

define i64 @prim_fxadd1(i64 %a) {
  %tmp = add i64 %a, 16
  ret i64 %tmp
}

define i64 @prim_fxsub1(i64 %a) {
  %tmp = sub i64 %a, 16
  ret i64 %tmp
}

define i64 @prim_fx_equal_(i64 %a, i64 %b) {
  %tmp = icmp eq i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    %res1 = load i64, i64* @TRUE, align 8
    ret i64 %res1
  false:
    %res2 = load i64, i64* @FALSE, align 8
    ret i64 %res2
}
