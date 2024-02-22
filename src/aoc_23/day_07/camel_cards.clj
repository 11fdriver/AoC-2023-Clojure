(ns aoc-23.day-07.camel-cards
  (:require [clojure.string :as str]))

(def input-string (slurp "inputs/07"))

(defn parse-hand-bid
  "Take string s of form \"CARDS BID\" and return a map of form
  {:type :hand :bid}."
  [s]
  (let [[hand bidstr] (str/split s #" ")
        bid (parse-long bidstr)]
    {:hand hand, :bid bid}))

(def frequency-type
  "Map between descending frequencies of cards and their hand-type."
  {[5]         :five-of-a-kind
   [4 1]       :four-of-a-kind
   [3 2]       :full-house
   [3 1 1]     :three-of-a-kind
   [2 2 1]     :two-pair
   [2 1 1 1]   :one-pair
   [1 1 1 1 1] :high-card})

(defn hand-type
  "Takes string s of a hand of 5 cards, returns a keyword for it's hand type."
  [s]
  (->> s
       (frequencies)
       (vals)
       (sort >)
       (vec)
       frequency-type))

(def hand-type-score
  "Sorting values for types of hand."
  (zipmap [:high-card :one-pair :two-pair :three-of-a-kind
           :full-house :four-of-a-kind :five-of-a-kind] (range)))

(def card-score
  "Face values for playing cards (for sorting)."
  (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range)))

(defn compare-hands
  "Comparator that compares two hands.
  First compares the hand type then the hand itself."
  [hand1 hand2]
  (letfn [(hand->scores [{:keys [type hand]}]
            [(hand-type-score type) (mapv card-score hand)])]
    (compare (hand->scores hand1) (hand->scores hand2))))

(def star-1 (->> input-string
                 (str/split-lines)
                 (map parse-hand-bid)
                 (map #(assoc %1 :type (hand-type (:hand %1))))
                 (sort compare-hands)
                 (map-indexed #(* (inc %1) (:bid %2)))
                 (reduce +)))
;; (= 251545216 star-1)

(defn hand-type-jokerify
  "Takes string s of a hand of 5 cards, maybe containing Jokers, returns a keyword
  for it's hand type."
  [s]
  (let [jokerless (remove #(= \J %) s)
        n (- (count s) (count jokerless))
        add-jokers (fn [[h :as v]] (assoc v 0 (+ h n)))
        vals-or-0 #(or (vals %) '(0))]
    (->> jokerless
         (frequencies)
         (vals-or-0) ;<- For "JJJJJJ"
         (sort >)
         (vec)
         (add-jokers)
         frequency-type)))

(def joker-card-score
  "Face values for playing cards (for sorting) using Joker as \\J."
  (assoc card-score \J -1))

(defn compare-joker-hands
  "Comparator that compares two hands maybe containing Jokers.
  First compares the hand type then the hand itself."
  [hand1 hand2]
  (letfn [(hand->scores [{:keys [type hand]}]
            [(hand-type-score type) (mapv joker-card-score hand)])]
    (compare (hand->scores hand1) (hand->scores hand2))))

(def star-2 (->> input-string
                 (str/split-lines)
                 (map parse-hand-bid)
                 (map #(assoc %1 :type (hand-type-jokerify (:hand %1))))
                 (sort compare-joker-hands)
                 (map-indexed #(* (inc %1) (:bid %2)))
                 (reduce +)))
;; (= 250384185 star-2)
