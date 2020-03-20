(def fib-seq
  (lazy-cat [(bigint 0) (bigint 1)] (map + (rest fib-seq) fib-seq)))

(defn bigpow10 [expt]
  (loop [result (bigint 1)
         expt expt]
    (if (zero? expt)
      result
      (recur (* result 10) (dec expt)))))

(def thousand-digits (bigpow10 999))

(def indexed-fib-seq (partition 2 (interleave fib-seq (range))))
