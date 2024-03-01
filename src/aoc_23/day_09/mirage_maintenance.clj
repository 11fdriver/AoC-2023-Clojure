(ns aoc-23.day-09.mirage-maintenance
  (:require [clojure.string :as str]))

(def input-string (slurp "inputs/09"))

(def test-input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn parse-oasis-readings
  [s]
  (letfn [(split-spaced [s] (str/split s #" "))
          (parse-longsv [coll] (mapv parse-long coll))]
    (->> s
         (str/split-lines)
         (map split-spaced)
         (map parse-longsv))))

(def star-1 nil)

(def star-2 nil)

(comment
  (map (fn [[x y]] (- y x))
       (partition 2 1
                  (first (parse-oasis-readings test-input))))
  )
