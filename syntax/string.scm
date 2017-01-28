(defn emit-string (var str)
  (let ((len (string-length str))
        (tmp (generate-var)))
    (emit-call0 tmp "@internal_heap-current-pointer")
    (emit-string_ str 0 len)
    (print "  call i64 @internal_heap-store-byte(i8 0)")
    (print "  call void @internal_heap-align-index()")
    (print (string-append* (list "  " var " = or i64 " tmp ", 5")))))

(defn emit-string_ (str idx len)
  (if (< idx len)
      (begin
        (print (string-append* (list "  call i64 @internal_heap-store-byte(i8 "
                                     (fixnum->string (char->integer (string-ref str idx)))
                                     ")")))
        (emit-string_ str (add1 idx) len))))
