(require '(clojure [set :as set]))

(defn over [n k]
  (->>
   (for [i (range 1 (inc k))]
     (/ (- (inc n) i) i))
   (reduce *)))

(def to-digits (memoize (fn [num]
  (loop [num num
         result '()]
    (if (> num 0)
      (recur (quot num 10) (conj result (mod num 10)))
      result)))))

(def all-digits (set (range 1 10)))

(defn is-pandigital-product? [one two]
  (let [num (* one two)
        digprod (to-digits num)
        digone (to-digits one)
        digtwo (to-digits two)]
    (if (not= (reduce + (map count (list digprod digone digtwo))) 9)
      false
      (empty? (set/difference all-digits (set digprod) (set digone) (set digtwo))))))

(defn is-suitable? [num]
  (let [digits (to-digits num)]
    (and (not ((set digits) 0))
         (= (count digits)
            (count (set digits))))))

(defn find-pandigital-products [max]
  (let [candidates (filter is-suitable? (range 1 max))]
    (for [one candidates
          two candidates
          :when (and (<= (* one two) 99999) (is-pandigital-product? one two))]
      (* one two))))


(def generate-subsets (memoize (fn [s size]
  (cond
    (empty? s) '()
    (= size 1) (map hash-set s)
    (= size (count s)) (list (set s))
    :else (concat (generate-subsets (rest s) size)
                  (map #(conj % (first s)) (generate-subsets (rest s) (dec size))))))))
