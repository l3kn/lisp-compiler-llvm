(define (escape str)
  (let ((parts (map ->string (string->list (->string str)))))
    (string-join
      (cons "prim_"
        (map
          (lambda (part)
            (cond
              ((equal? part "+") "_plus_")
              ((equal? part ">") "_greater_")
              ((equal? part "<") "_less_")
              ((equal? part "=") "_equal_")
              ((equal? part "*") "_times_")
              ((equal? part "/") "_slash_")
              ((equal? part "?") "_questionmark_")
              (else part)))
          parts)))))

(define (tagged-list? expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))

(define (string-join lst) (foldl string-append "" lst))
(define (show . args) (string-join (map ->string args)))

