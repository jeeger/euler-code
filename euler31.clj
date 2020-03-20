
(def coin-sizes '(1 2 5 10 20 50 100 200))

(def ways-to-make-amount (memoize
                          (fn [amount]
                            (cond
                              (= amount 1) 1
                              (= amount 0) 0
                              :else (let [possible-coins (take-while #(<= % amount) coin-sizes)]
                                      (->>
                                       (map #(- amount %) possible-coins)
                                       (map ways-to-make-amount)
                                       (map inc)
                                       (reduce +)))))))


(def ways-to-change (memoize (fn [amount]
                               (cond
                                 (= amount 1) '((1))
                                 (= amount 0) '(())
                                 :else
                                 (->> coin-sizes
                                      (take-while #(<= % amount))
                                      (map (fn [arg] {:coin arg :left (- amount arg)}))
                                      (map (fn [{coin :coin rest :left}] {:coin coin :ways (ways-to-change rest)}))
                                      (mapcat (fn [{coin :coin ways :ways}] (map #(cons coin %) ways)))
                                      (map sort)
                                      (dedupe))))))



(def ways-to-change-dynamic (memoize (fn [amount coins]
  (cond
    (= amount 0) 1
    (< amount 0) 0
    (empty? coins) 0
    :else (+ (ways-to-change-dynamic amount (rest coins))
             (ways-to-change-dynamic (- amount (first coins)) coins))))))
