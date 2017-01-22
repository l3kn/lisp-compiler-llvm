(def label-count 0)

(defn unique-label (name)
  (let ((label (string-append
                 "L"
                 (number->string label-count)
                 "_"
                 name)))
    (set! label-count (add1 label-count))
    label))
