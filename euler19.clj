(import '(java.time LocalDate DayOfWeek)
        '(java.time.temporal ChronoField))

(defn is-leap-year? [year]
  (and (= (rem year 4) 0)
       (or (not (= (rem year 100) 0))
           (= (rem year 400) 0))))

(defn days-in-year [year]
  (if (is-leap-year? year)
    366
    365))


(defn find-first-sunday [year]
  (- 7 (.. (LocalDate/of year 1 1)
         (getDayOfWeek)
         (getValue))))

(def month-list '(:january :february :march :april :may :june :july :august :september :october :november :december))

(defn month-name [month]
  (nth month-list (dec month)))

(defn month-length [year month]
  (case (if-not (keyword? month) (month-name month) month)
    :september 30
    :april 30
    :june 30
    :november 30
    :february (if (is-leap-year? year) 29 28)
    :january 31
    :march 31
    :may 31
    :july 31
    :august 31
    :october 31
    :december 31))

(defn first-days-of-year [year]
  (->> (map #(month-length year %) month-list)
       (reduce (fn [cur elem]
                 (conj cur (+ (last cur) elem))) [0])
       (butlast)
       (into #{})))


(defn count-sundays-in-year [year]
  (let [firsts (first-days-of-year year)
        days (days-in-year year)]
    (loop [curday (find-first-sunday year)
           sundays 0]
      (if (> curday days)
        sundays
        (recur (+ curday 7) (if (contains? firsts curday) (inc sundays) sundays))))))
