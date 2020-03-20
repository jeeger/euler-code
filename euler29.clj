(defn calc [a b]
  (bigint (Math/pow a b)))

(defn calc-range [max]
  (loop [result #{}
         a 2
         b 2]
    (cond
      (and (> a max) (> b max)) (sort (into [] result))
      (> a max) (recur result 2 (inc b))
      :else (recur (conj result (calc a b)) (inc a) b))))
