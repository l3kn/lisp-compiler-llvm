(def var-counter 0)
(defn generate-var ()
  (begin
    (set! var-counter (add1 var-counter))
    (format "%tmp~A" (sub1 var-counter))))

(def label-count 0)
(defn unique-label (name)
  (set! label-count (add1 label-count))
  (string-append "L" (number->string label-count) "_" name))

(defn arg-str (arity)
  (cond
    ((eq? arity 0) "")
    ((eq? arity 1) "i64")
    (else
      (string-append "i64, "
                     (arg-str (sub1 arity))))))

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

(defn empty-set () (list))
(defn set (expr) (list expr))
(defn set-union (a b) (lset-union eq? a b))
(defn set-subtract (a b) (lset-difference eq? a b))

(defn set-union* (sets)
      (cond
        ((null? sets) sets)
        ((null? (rst sets)) (fst sets))
        (else (set-union*
                (cons (set-union (fst sets) (frst sets))
                      (rrst sets))))))
