define i64 @prim_not(i64 %a) {
  %tmp = icmp eq i64 %a, 31
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_eq_questionmark_(i64 %a, i64 %b) {
  %tmp = icmp eq i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}
