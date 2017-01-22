define i64 @prim_fx_plus_(i64 %a, i64 %b) {
  %tmp = add i64 %a, %b
  ret i64 %tmp
}

define i64 @prim_fx_minus_(i64 %a, i64 %b) {
  %tmp = sub i64 %a, %b
  ret i64 %tmp
}

define i64 @prim_fxadd1(i64 %a) {
  %tmp = add i64 %a, 8
  ret i64 %tmp
}

define i64 @prim_fxsub1(i64 %a) {
  %tmp = sub i64 %a, 8
  ret i64 %tmp
}

define i64 @prim_fx_equal_(i64 %a, i64 %b) {
  %tmp = icmp eq i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fxzero_questionmark_(i64 %a) {
  %tmp = icmp eq i64 %a, 0
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fx_equal__questionmark_(i64 %a, i64 %b) {
  %tmp = icmp eq i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fx_less__questionmark_(i64 %a, i64 %b) {
  %tmp = icmp slt i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fx_greater__questionmark_(i64 %a, i64 %b) {
  %tmp = icmp sgt i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fx_less__equal__questionmark_(i64 %a, i64 %b) {
  %tmp = icmp sle i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}

define i64 @prim_fx_greater__equal__questionmark_(i64 %a, i64 %b) {
  %tmp = icmp sge i64 %a, %b
  br i1 %tmp, label %true, label %false
  true:
    ret i64 63
  false:
    ret i64 31
}
