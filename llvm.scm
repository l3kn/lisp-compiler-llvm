(defn emit-alloca (var)
  (puts (format "  ~A = alloca i64" (list var))))

(defn emit-store (value in)
  (puts (format "  store i64 ~A, i64* ~A" (list value in))))

(defn emit-load (var from)
  (puts (format "  ~A = load i64, i64* ~A" (list var from))))

(defn emit-copy (var var_)
  (puts (format "  ~A = add i64 ~A, 0" (list var var_))))

(defn emit-label (name)
  (puts (string-append name ":")))

(defn emit-br1 (label)
  (puts (string-append "  br label %" label)))

(defn emit-ret (val)
  (puts (string-append "  ret i64 " val)))

(defn emit-call0 (var name)
  (puts (format "  ~A = call i64 ~A()" (list var name))))

(defn emit-call1 (var name arg)
  (puts (format "  ~A = call i64 ~A(i64 ~A)" (list var name arg))))

(defn emit-call2 (var name arg1 arg2)
  (puts (format "  ~A = call i64 ~A(i64 ~A, i64 ~A)" (list var name arg1 arg2))))

(defn emit-call3 (var name arg1 arg2 arg3)
  (puts (format "  ~A = call i64 ~A(i64 ~A, i64 ~A, i64 ~A)" (list var name arg1 arg2 arg3))))
