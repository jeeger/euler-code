(defn bigpow10 [expt]
  (loop [result (bigint 1)
         expt expt]
    (if (zero? expt)
      result
      (recur (* result 10) (dec expt)))))

(defn repeating-reciprocal-length-fast [n]
  (loop [expt 1]
    (cond
      (= (mod (- (bigpow10 expt) 1) n) 0) expt
      (> expt n) 0
      :else (recur (inc expt)))))

(defn find-max-reciprocal-length [n]
  (->> (range 1 n)
       (map repeating-reciprocal-length)
       (zipmap (range 1 n))
       (reduce (fn [it elt]
                 (if (> (val elt) (val it))
                   elt
                   it)))))
