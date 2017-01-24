(defn pipe? (expr) (tagged-list? expr 'pipe))
(defn rpipe? (expr) (tagged-list? expr 'rpipe))

(defn pipe->nested-calls (expr)
      (pipe->nested-calls_ (frst expr)
                           (rrst expr)))

(defn pipe->nested-calls_ (var fns)
  (if (null? fns)
      var
      (pipe->nested-calls_
        (list (fst fns) var)
        (rst fns))))

(defn rpipe->nested-calls (expr)
      (rpipe->nested-calls_ (frst expr)
                            (rrst expr)))

(defn rpipe->nested-calls_ (var fns)
  (if (null? fns)
      var
        (list (fst fns)
              (rpipe->nested-calls_ var (rst fns)))))
