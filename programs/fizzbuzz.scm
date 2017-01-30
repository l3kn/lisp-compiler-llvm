(defn range (from to)
      (if (eq? from to)
          (list from)
          (cons from
                (range (fxadd1 from) to))))

(defn fizzbuzz (n)
      (cond
        ((eq? (fxrem n 15) 0) (puts "FizzBuzz"))
        ((eq? (fxrem n 3) 0) (puts "Fizz"))
        ((eq? (fxrem n 5) 0) (puts "Buzz"))
        (else (inspect n))))

(for-each fizzbuzz (range 1 100))
