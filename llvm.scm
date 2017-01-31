(defn emit-alloca (var)
  (puts (string-append* (list "  " var " = alloca i64"))))

(defn emit-store (value in)
  (puts (string-append* (list "  store i64 " (any->string value) ", i64* " in))))

(defn emit-load (var from)
  (puts (string-append* (list "  " var " = load i64, i64* " from))))

(defn emit-copy (var var_)
  (puts (string-append* (list "  " var " = add i64 " var_ ", 0"))))
      ; (let ((tmp (generate-var)))
      ;   (emit-alloca tmp)
      ;   (emit-store var_ tmp)
      ;   (emit-load var tmp)))

(defn emit-label (name)
  (puts (string-append name ":")))

(defn emit-br1 (label)
  (puts (string-append "  br label %" label)))

(defn emit-ret (val)
  (puts (string-append "  ret i64 " val)))

(defn emit-call0 (var name)
  (puts (string-append* (list "  " var " = call i64 " name "()"))))

(defn emit-call1 (var name arg)
  (puts (string-append* (list "  " var " = call i64 " name "(i64 " arg ")"))))

(defn emit-call2 (var name arg1 arg2)
  (puts (string-append* (list "  " var " = call i64 " name "(i64 " arg1 ", i64 " arg2 ")"))))

(defn emit-call3 (var name arg1 arg2 arg3)
  (puts (string-append* (list "  " var " = call i64 " name "(i64 " arg1 ", i64 " arg2 ", i64 " arg3 ")"))))
