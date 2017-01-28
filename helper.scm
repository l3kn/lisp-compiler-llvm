(def var-counter 0)
(defn generate-var ()
  (begin
    (set! var-counter (add1 var-counter))
    (string-append "%tmp"
                   (fixnum->string var-counter))))

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
  (~>> str any->string string->list
       (map any->string)
       (map escape-char)
       (cons "prim_")
       string-join))

(defn escape-char (str)
      (cond
        ((equal? str "+") "_plus_")
        ((equal? str ">") "_greater_")
        ((equal? str "<") "_less_")
        ((equal? str "=") "_equal_")
        ((equal? str "*") "_times_")
        ((equal? str "/") "_slash_")
        ((equal? str "?") "_questionmark_")
        (else str)))

(defn tagged-list? (expr tag)
      (and (pair? expr)
           (eq? (car expr) tag)))

(defn string-join (lst) (foldl string-append "" lst))

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

(def empty-env (list))

(defn extend-env (var val env)
      (alist-cons var val env))

(defn extend-env* (vars vals env)
      (if (null? vars)
        env
        (extend-env* (rst vars) (rst vals)
                     (extend-env (fst vars) (fst vals) env))))

(defn lookup (var env)
      (let ((res (assoc var env)))
        (if res
          (rst res)
          (error "Trying to lookup unbound variable: " var))))

(defn lookup-or (var alt env)
      (let ((res (assoc var env)))
        (if res (rst res) alt)))
