(def to-digits (memoize (fn [num]
  (loop [num num
         result '()]
    (if (> num 0)
      (recur (quot num 10) (conj result (mod num 10)))
      result)))))


(defn factorial [num]
  (reduce * (range 1 (inc num))))

(defn valid [num]
  (->> (to-digits num)
       (map factorial)
       (reduce +)
       (= num)))

(defn find-nums [max]
  (filter valid (range max)))
