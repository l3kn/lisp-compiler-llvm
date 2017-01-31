(def var-counter 0)
(defn generate-var ()
  (begin
    (set! var-counter (add1 var-counter))
    (string-append "%tmp"
                   (fixnum->string var-counter))))

(def label-count 0)
(defn unique-label (name)
  (set! label-count (add1 label-count))
  (string-append* (list "L" (fixnum->string label-count) "_" name)))

(defn arg-str (arity)
  (cond
    ((eq? arity 0) "")
    ((eq? arity 1) "i64")
    (else
      (string-append "i64, "
                     (arg-str (sub1 arity))))))

(defn escape-char (char) char
      (cond
        ((eq? char #\+) "_plus_")
        ((eq? char #\>) "_greater_")
        ((eq? char #\<) "_less_")
        ((eq? char #\=) "_equal_")
        ((eq? char #\*) "_times_")
        ((eq? char #\/) "_slash_")
        ((eq? char #\?) "_questionmark_")
        (else (char->string char))))

(defn escape (str)
  (~>> str any->string string->list
       (map escape-char)
       (cons "prim_")
       string-append*))

(defn tagged-list? (expr tag)
      (and (pair? expr)
           (eq? (fst expr) tag)))

(defn map-with-index (f start lst)
      (if (null? lst)
        lst
        (cons
          (f (fst lst) start)
          (map-with-index f (add1 start) (rst lst)))))

(defn empty-set () '())
(defn singleton-set (expr) (list expr))

(defn set-subtract (a b)
      (filter (fn (e) (not (member? e b)))
              a))

(defn set-union (a b)
      (cond
        ((null? a) b)
        ((null? b) a)
        ((member? (fst b) a)
         (set-union a (rst b)))
        (else
          (set-union (cons (fst b) a) (rst b)))))

(defn set-union* (sets)
      (cond
        ((null? sets) sets)
        ((null? (rst sets)) (fst sets))
        (else (set-union*
                (cons (set-union (fst sets) (frst sets))
                      (rrst sets))))))

(def empty-env '())

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
