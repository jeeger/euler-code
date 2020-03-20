

(defn to-digits [num]
  (loop [num num
         result []]
    (if (> num 0)
      (recur (quot num 10) (conj result (mod num 10)))
      result)))

(defn fifth-power [num]
  (apply *' (repeat 5 num)))

(defn fourth-power [num]
  (apply *' (repeat 4 num)))

(defn satisfies [power-fn num]
  (= num (reduce + (map power-fn (to-digits num)))))

(defn satisfies-fast [expt num]
  (loop [digits (to-digits num)
         running-sum 0]
    (cond
      (and (empty? digits) (= running-sum num)) true
      (> running-sum num) false
      (empty? digits) false
      :else (recur (rest digits) (+ running-sum (reduce * (repeat expt (first digits))))))))

(defn solution []
  (->> (range 2 (inc 999999))
       (filter (partial satisfies fifth-power))
       (rest)
       (rest)
       (reduce +)))
