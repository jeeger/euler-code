;; X * X square.

(def search-start '(0 0))

(defn search-stop [size]
  (list size size))

(defn make-right? [size]
  (fn [[x y]]
    (<= x size)))

(defn make-down? [size]
  (fn [[x y]]
    (<= y size)))

(defn make-finished? [size]
  (fn [[x y]]
    (and (= x size)
         (= y size))))

(defn make-valid? [size]
  (fn [[x y]]
    (and (<= x size)
         (<= y size))))

(defn step-down [[x y]]
  (list x (inc y)))

(defn step-right [[x y]]
  (list (inc x) y))

(defn make-graph [size]
  (let [valid? (make-valid? size)]
    (reduce #(into %1 %2)
            {}
            (for [x (range (inc size))
                  y (range (inc size))
                  :let [pos (list x y)]]
              {pos (->> (list (step-right pos) (step-down pos))
                        (filter valid?)
                        (set))}))))

(defn find-paths [size]
  (let [graph (make-graph size)
        finished? (make-finished? size)
        search (fn search [path]
                 (let [current (peek path)]
                   (if (finished? current)
                     1
                     (->> (graph current) ; Get all neighbors
                          (remove (set path)) ; Remove those we've already visited
                          (map #(search (conj path %)))
                          (reduce + 0)))))]
    (search [search-start])))

(defn fact [n]
  (loop [cur n
         result (bigint 1)]
    (if (= cur 0)
      result
      (recur (dec cur) (* result cur)))))

(defn count-paths [size]
  (/ (fact (* 2 size)) (* (fact size) (fact size))))
