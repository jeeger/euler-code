(require '(clojure [string :as str]))

(defn read-names []
  (-> (slurp "names.txt")
      (str/replace "\"" "")
      (str/split #",")
      (sort)))

(def names (read-names))

(defn numeric-score [name]
  (->> name
       (map int)
       (map #(- %1 64))
       (reduce +)))

(defn calculate-name-score [names]
  (->> names
       (map-indexed (fn [index item]
                      (* (inc index) (numeric-score item))))
       (reduce + (bigint 0))))
