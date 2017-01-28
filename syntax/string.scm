(defn emit-string (var str)
  (let ((len (string-length str))
        (tmp (generate-var)))
    (print (format "  ~A = call i64 @internal_heap-current-pointer()" tmp))
    (emit-string_ str 0 len)
    (print (format "  call i64 @internal_heap-store-byte(i8 0)"))
    (print (format "  call void @internal_heap-align-index()"))
    (print (format "  ~A = or i64 ~A, 5" var tmp))))

(defn emit-string_ (str idx len)
  (if (< idx len)
      (begin
        (print (format "  call i64 @internal_heap-store-byte(i8 ~A)" (char->integer (string-ref str idx))))
        (emit-string_ str (add1 idx) len))))
