(ns aoc-23.day-04.scratchcards
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input-string (slurp "inputs/04"))

(defn card-table->vector
  "Return vector of scratchcards as ([winning numbers] [chosen numbers])."
  [s]
  (->> (str/split-lines s)
       (map #(re-find #"(?<=:).*" %))
       (mapcat #(str/split % #"\|"))
       (map str/triml)
       (map #(str/split % #" +"))
       (map #(mapv parse-long %))
       (partition 2)))

(defn scratchcard-wins
  "Return number of wins from a scratchcard."
  [cards]
  (->> cards
       (mapv set)
       (apply set/intersection)
       count))

(defn card-table->points
  "Return total number of points from table of scratchcards s, with 0 points for
  no wins or formula '2^(wins-1)'."
  [s]
  (let [max-nums (-> (re-find #"(?<=: +).*(?= \|)" input-string)
                     (str/split #" +")
                     (count))           ;Maximum possible winning numbers
        scores (apply vector 0 (take max-nums (iterate #(* 2 %) 1)))]
    (->> s
         (card-table->vector)
         (map scratchcard-wins)
         (map #(get scores %))
         (reduce +))))

(def star-1 (card-table->points input-string))

(defn card-table->total-cards
  "Convert table of scratchcards as string s by recursively duplicating the next
  cards according to the amount of wins on current card, returning total amount
  of cards encountered."
  [s]
  (let [init-cards (card-table->vector s)
        n-cards (count init-cards)]
    (loop [[c & cards] init-cards
           [r & repeats] (repeat n-cards 1)
           total 0]
      (if c
        (let [wins (scratchcard-wins c)]
          (recur cards (map + repeats (concat (repeat wins r) (repeat 0))) (+ total r))),
        total))))

(def star-2 (card-table->total-cards input-string))

(comment
  (defn card-table->points'
    "Faster way to convert card-table to points than `card-table->points`."
    [s]
    (let [max-nums (-> (re-find #"(?<=: +).*(?= \|)" s)
                       (str/split #" +")
                       (count))
          scores (apply vector 0 (take max-nums (iterate #(* 2 %) 1)))
          xf (comp (map #(re-find #"(?<=:).*" %)) ;Remove start
                   (mapcat #(str/split % #"\|"))  ;Split into winners and 'scratched'
                   (map str/triml)
                   (map #(str/split % #" +")) ;Individual numbers
                   (map #(mapv parse-long %))
                   (map set)
                   (partition-all 2)                         ;Group into pairs
                   (map #(apply clojure.set/intersection %)) ;Get winning numbers
                   (map count)                               ;Count wins
                   (map #(get scores %)))]                   ;Look up final score
      (transduce xf + (str/split-lines s)))
