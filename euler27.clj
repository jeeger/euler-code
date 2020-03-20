(defn is-prime [n]
  (. (biginteger n) isProbablePrime 20))

(defn has-n-primes [seq]
  (count (take-while is-prime seq)))

(defn gen-fn [a b]
  (fn [n]
    (+ (* n n)
       (* a n)
       b)))

(defn find-length [a b]
  (let [fn (gen-fn a b)]
    {:a a :b b :primes (has-n-primes (map fn (range)))}))

(defn find-max-length [start end]
  (loop [a start
         b start
         maxentry {:a 0 :b 0 :primes 0}]
    (let [length (find-length a b)]
      (cond
        (> (:primes length) (:primes maxentry)) (recur (inc a) b length)
        (>= a (dec end)) (recur start (inc b) maxentry)
        (>= b end) maxentry
        :else (recur (inc a) b maxentry)))))
         
