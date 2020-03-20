(defn generate-diagonals [max]
  (loop [num 1
         side 0
         delta 2
         result '()]
    (cond
      (> num max) result
      (= side 3) (recur (+ num delta) 0 (+ delta 2) (cons num result))
      :else (recur (+ num delta) (inc side) delta (cons num result)))))
