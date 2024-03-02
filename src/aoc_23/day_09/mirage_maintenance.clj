(ns aoc-23.day-09.mirage-maintenance
  (:require [clojure.string :as str]))

(def input-string (slurp "inputs/09"))

(defn parse-oasis-readings
  "Take string s of OASIS readings and return as vectors of longs."
  [s]
  (letfn [(split-spaced [s] (str/split s #" "))
          (parse-longsv [coll] (mapv parse-long coll))]
    (->> s
         (str/split-lines)
         (map split-spaced)
         (map parse-longsv))))

(defn find-difference-series
  "Take vector v of numbers and return vector of effects of repeatedly finding
  differences between numbers in list, until just list of zeros or empty."
  [v]
  (letfn [(difference [[x y]] (- y x))]
    (loop [current v 
           history []]
      (if (every? zero? current)
        (conj history current)
        (recur (mapv difference (partition 2 1 current))
               (conj history current))))))

(defn find-next-in-series
  "Take vector v of numbers and find next in series, assuming that the series'
  differences reduces to a list of just zeros, see `find-difference-series`."
  [v]
  (->> v
       (find-difference-series)
       (map peek)
       (reduce +)))

(def star-1 (->> input-string
                 (parse-oasis-readings)
                 (map find-next-in-series)
                 (reduce +)))
;; (= 1882395907 star-1)

(defn find-prior-in-series
  "Take vector v of numbers and find prior number in series, assuming that the
  series' differences reduces to a list of just zeros, see
  `find-difference-series`."
  [v]
  (->> v
       (find-difference-series)
       (rseq) ;Faster than reverse for vectors.
       (map #(nth % 0))
       (reduce #(- %2 %1))))

(def star-2 (->> input-string
                 (parse-oasis-readings)
                 (map find-prior-in-series)
                 (reduce +)))
;; (= 1005 star-2)
