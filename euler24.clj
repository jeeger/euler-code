(def numbers [0 1 2 3 4 5 6 7 8 9])

(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))

(defn find-k [seq]
  (loop [seq seq
         ind 0
         largest -1]
    (cond
      (< (count seq) 2) largest
      (< (first seq) (second seq)) (recur (rest seq) (inc ind) ind)
      :else (recur (rest seq) (inc ind) largest))))

(defn find-l [k seq]
  (let [kelt (seq k)]
    (loop [seq (drop k seq)
           ind k
           largest -1]
      (cond
        (empty? seq) largest
        (< kelt (first seq)) (recur (rest seq) (inc ind) ind)
        :else (recur (rest seq) (inc ind) largest)))))
            
(defn reverse-rest [seq k]
  (concat (take (inc k) seq) (reverse (drop (inc k) seq))))

(defn permute [seq]
  (let [k (find-k seq)]
    (if (= k -1)
      nil
      (reverse-rest (swap seq k (find-l k seq)) k))))

(defn permute-times [times seq]
  (loop [seq seq
         times times]
    (cond
      (zero? times) seq
      (nil? seq) nil
      :else  (recur (permute seq) (dec times)))))

