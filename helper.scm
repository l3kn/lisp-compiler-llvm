(defn escape (str)
  (let ((parts (map ->string (string->list (->string str)))))
    (string-join
      (cons "prim_"
        (map
          (fn (part)
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

(defn tagged-list? (expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))

(defn string-join (lst) (foldl string-append "" lst))
(define (show . args) (string-join (map ->string args)))

(defn map-with-index (f start lst)
  (if (null? lst)
      lst
      (cons
        (f (fst lst) start)
        (map-with-index f (add1 start) (rst lst)))))

(defn empty-set ()
      (list))

(defn set (expr)
      (list expr))

(defn set-subtract (a b)
      (filter (fn (x)
                  (not (member x b)))
              a))
(defn set-union (a b)
      (append a
              (set-subtract b a)))

(defn set-union* (sets)
      (cond
        ((null? sets) sets)
        ((null? (rst sets)) (fst sets))
        (else (set-union*
                (cons (set-union (fst sets)
                                 (frst sets))
                      (rrst sets))))))

(defn filter (pred lst)
      (cond
        ((null? lst) lst)
        ((pred (fst lst))
         (cons (fst lst)
               (filter pred (rst lst))))
        (else (filter pred (rst lst)))))

