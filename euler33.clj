(def to-digits (memoize (fn [num]
  (loop [num num
         result '()]
    (if (> num 0)
      (recur (quot num 10) (conj result (mod num 10)))
      result)))))


(defn build-fraction [n1 n2 d1 d2]
  (/ (+ (* n1 10) n2) (+ (* d1 10) d2)))

(defn cancels-correctly [numer1 numer2 denom1 denom2]
  (cond
    (= numer1 numer2 denom1 denom2) false ;Trivial
    (= numer1 denom2) (= (/ numer2 denom1) (build-fraction numer1 numer2 denom1 denom2))
    (= numer2 denom1) (= (/ numer1 denom2) (build-fraction numer1 numer2 denom1 denom2))
    :else false))

(defn find-nontrivial []
  (for [n1 (range 1 10)
        n2 (range 1 10)
        d1 (range 1 10)
        d2 (range 1 10)
        :when (and (< (build-fraction n1 n2 d1 d2) 1) (cancels-correctly n1 n2 d1 d2))]
    (build-fraction n1 n2 d1 d2)))
        
