(use '[clojure.string :only (join)])

(def number-name-map
  (sorted-map
   0 "zero"
   1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"))

(def corrections
  {"twoty" "twenty"
   "threety" "thirty"
   "fivety" "fifty"
   "eightty" "eighty"
   "fourty" "forty"
   "onety" "ten"
   "zero" ""
   "zeroty" ""
   "zerohundred" ""
   "zerothousand" ""})

(def multicorrections
  {"tenone" "eleven"
   "tentwo" "twelve"
   "tenthree" "thirteen"
   "tenfour" "fourteen"                 ;Good buddy.
   "tenfive" "fifteen"
   "tensix" "sixteen"
   "tenseven" "seventeen"
   "teneight" "eighteen"
   "tennine" "nineteen"})

(def exponents-spoken ["" "ty" "hundred" "thousand"])

(defn to-digits [n]
  (loop [n n
         result []]
    (if (= 0 n)
      result
      (recur (quot n 10) (conj result (rem n 10))))))



(defn basic-spell [n]
  (loop [digits (to-digits n)
         exponents-spoken exponents-spoken
         result []]
    (if (empty? digits)
      (->>
       (reverse result)
       (map #(or (get corrections %) %)))
      (recur (rest digits)
             (rest exponents-spoken)
             (conj result
                   (str (get number-name-map (first digits)) (first exponents-spoken)))))))


(defn extend-list [spoken]
  (cond
    (= (count spoken) 1) (concat '("" "" "") (list (first spoken)))
    (= (count spoken) 2) (concat '("" "") (list (first spoken) (second spoken)))
    (= (count spoken) 3) (cons "" spoken)
    :else spoken))

(defn hundred [spoken]
  (nth spoken 1))

(defn ten [spoken]
  (nth spoken 2))

(defn one [spoken]
  (nth spoken 3))

(defn thousand [spoken]
  (first spoken))
  

(defn add-and [spoken]
  (cond
    (and (not= (thousand spoken) "")
         (= (hundred spoken) "")
         (or (not= (ten spoken) "")
             (not= (one spoken) ""))) (list (thousand spoken) "and" (ten spoken) (one spoken)) ; No hundred
    (and (not= (hundred spoken) "")
         (or (not= (ten spoken) "")
             (not= (one spoken) ""))) (list (thousand spoken) (str (hundred spoken) "and") (ten spoken) (one spoken))
    :else spoken))


(defn correct-lower-twenty [spoken]
  (let [key (str (ten spoken) (one spoken))]
    (list (thousand spoken)
          (hundred spoken)
          (get multicorrections key key))))


(defn full-spell [number]
  (->> number
       (basic-spell)
       (extend-list)
       (add-and)
       (correct-lower-twenty)
       (join)))

(defn number-of-letters [until]
  (reduce + (map (comp count full-spell) (range 1 (inc until)))))
