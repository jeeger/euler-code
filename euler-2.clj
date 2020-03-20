(ns clojure-test.euler2
  (:require [clojure.core.reducers :as reducers]
            [clojure.math.numeric-tower :as math]))

(defn tri [n]
  (/ (+ (* n n) n) 2))

(def tris (map tri (range)))

(defn is-tri [num]
  (* 0.5 (dec (math/sqrt (+ (* 8 num) 1)))))

(defn pow [num exp]
  (loop [result (bigint 1)
         count exp]
    (if (> count 0)
      (recur (* result num) (dec count))
      result)))

(defn divisors-naive [num]
  (filter #(= (mod num %) 0) (range 1 (inc num))))

(defn divisors-parallel [num]
  (into []
        (reducers/filter #(= (mod num %) 0) (into [] (range 1 (inc num))))))

;; (defn primes
;;   ([num] (primes num 1 []))
;;   ([num cur acc]
;;    (if (< cur num)
;;      (
   
