(defn bigfac [n]
  (loop [result (bigint 1)
         n n]
    (if (= n 0)
      result
      (recur (* result n) (dec n)))))

(defn digit-sum [n]
  (loop [n n
         cursum 0]
    (if (zero? n)
      cursum
      (recur (quot n 10) (+ cursum (rem n 10))))))
