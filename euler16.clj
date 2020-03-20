(defn bigpow [base expt]
  (loop [expt expt
         val (bigint 1)]
    (if (zero? expt)
      val
      (recur (dec expt) (* val base)))))

(defn digit-sum [n]
  (loop [n n
         cursum 0]
    (if (zero? n)
      cursum
      (recur (quot n 10) (+ cursum (rem n 10))))))
