(defn list_? (expr) (tagged-list? expr 'list))

(defn list->nested-cons (expr)
      (list->nested-cons_ (rst expr)))

(defn list->nested-cons_ (elems)
      (if (null? elems)
          '()
          (list 'cons (fst elems)
                      (list->nested-cons_ (rst elems)))))
