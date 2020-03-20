(defn divisors [num]
  (filter #(= (rem num %) 0) (range 1 num)))

(defn abundant? [num]
  (> (->> (divisors num)
          (reduce +)) num))

(def largest-nonabundant-sum 20161)

(def abundants (into [] (filter abundant? (range largest-nonabundant-sum))))

(defn sum-of-abundants [num]
  (map #(+ num %) (take-while #(<= % num) abundants)))

(defn sum-of-nonsums [max]
  (let [abundant-sums (into #{} (mapcat sum-of-abundants (take-while #(<= % max) abundants)))]
    (->> (range 1 max)
         (filter #(not (abundant-sums %)))
         (reduce +))))
