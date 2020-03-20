(defn collatz [n]
  (cond
    (even? n) (/ n 2)
    (odd? n) (+ (* 3 n) 1)))

(defn collatz-seq [val]
  (loop [val (bigint val)
         result []]
    (if (= val 1)
      (conj result 1)
      (recur (collatz val) (conj result val)))))

(defn collatz-length [start]
  (loop [val (bigint start)
         count 1]
    (if (= val 1)
      count
      (recur (collatz val) (inc count)))))

(defn pair-collatz [start]
  {:start start
   :length (collatz-length start)})

(defn max-collatz-length [start end]
  (reduce (fn [cur-pair start]
            (let [next-pair (pair-collatz start)]
              (if (> (:length next-pair) (:length cur-pair))
                next-pair
                cur-pair))) {:start 0 :length 0} (range start end)))
                  
