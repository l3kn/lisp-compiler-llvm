(def lambdas (list))

; (defn register-lamdba (name body)
;       (let ((old-lambdas lambdas))
;         (set! lambdas
;           (cons (list name body)
;                 old-lambdas))))

(inspect lambdas)
(defn reg ()
  (set! lambdas (cons (list 1 2) lambdas)))
(reg)
(inspect lambdas)
