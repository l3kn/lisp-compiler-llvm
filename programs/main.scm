(defn format (str vars) (format_ str vars 0 (string-length str) ""))

; TODO: This will break if there are not enough vars
(defn format_ (str vars idx len res)
      (cond
        ((eq? idx len) res)
        ((and (fx<=? idx (fx- len 2))
              (eq? (string-ref str idx) #\~)
              (eq? (string-ref str (fxadd1 idx)) #\A))
         (format_ str (rst vars) (fx+ idx 2) len
                  (string-append
                    res
                    (any->string (fst vars)))))
        (else
          (format_ str vars (fxadd1 idx) len
                   (string-append
                     res
                     (char->string (string-ref str idx)))))))

(print (format "a + b = ~A" (list (fx+ 1 2))))
