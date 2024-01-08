(ns aoc-23.day-06.wait-for-it
  (:require [clojure.string :as str]))

(def input-string (slurp "inputs/06"))

(defn parse-numbers-at-str
  "Return vector of numbers taken from string s appearing after \"identifier:\"."
  [s identifier]
  (let [re (re-pattern (str "(?<=" identifier ":).*"))
        strs (-> (re-find re s)
                 (str/triml)
                 (str/split #" +"))]
    (mapv parse-long strs)))

(defn parse-times
  "Return vector of numbers from string s after \"Time:\". See
  `parse-numbers-at-str`."
  [s]
  (parse-numbers-at-str s "Time"))

(defn parse-distance-records
  "Return vector of numbers from string s after \"Distance:\". See
  `parse-numbers-at-str`."
  [s]
  (parse-numbers-at-str s "Distance"))

(defn distances-travelled-within
  "Return collection of distances that can be travelled within time in no. of
  milliseconds."
  [time]
  (let [speed->distance #(* % (- time %))]
    (->> (range 1 time)
         (map speed->distance))))

(defn new-distance-records
  "Return collection of record distances that can be travelled within time (no. of
  ms) given a record (number)."
  [time record]
  (->> (distances-travelled-within time)
       (filter #(< record %))))

(defn str->new-distance-records
  "Return collection of record distances given string s of times and corresponding
  records."
  [s]
  (map new-distance-records
       (parse-times s)
       (parse-distance-records s)))

(def star-1 (->> (str->new-distance-records input-string)
                 (map count)
                 (reduce *)))
;; (= 2449062 star-1)

(def star-2 (->> (str->new-distance-records (str/replace input-string #" +" ""))
                 (apply count)))
;; (= 33149631 star-2)
