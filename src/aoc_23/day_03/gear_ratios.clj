(ns aoc-23.day-03.gear-ratios
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-path "inputs/03")

(def input-string (slurp input-path))

(def test-input
  (str/triml "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."))

(defn digit-char?
  "Returns true if c has an ascii value from 48 to 57, inclusive."
  [c]
  (<= 48 (int c) 57))

(defn find-indexed-numbers
  "Get numbers and the indexes of their digits as [[indexes] number] from a
  collection v of form [index digit-character].
  Uses `digit-char?` internally."
  [v]
  (let [[[i1 d1] & idx-digits] (filter #(digit-char? (second %)) v)
        digits->number #(->> %1 (apply str) (read-string))]
    (loop [[[i d] & remaining] idx-digits
           idx-numbers []
           indexes [i1]
           digits [d1]]
      (cond
        (nil? i) (conj idx-numbers [indexes (digits->number digits)])
        (= (dec i) (peek indexes)) (recur remaining
                                          idx-numbers
                                          (conj indexes i)
                                          (conj digits d))
        :otherwise (recur remaining
                          (conj idx-numbers [indexes (digits->number digits)])
                          [i]
                          [d])))))

(defn symbol-char?
  "True if c is not a digit-character, '.', or newline.
  Uses `digit-char?` internally."
  [c]
  (and (not (digit-char? c))
       (not= \newline c)
       (not= \. c)))

(defn find-indexed-symbols
  "Get indexed-symbols of form [index symbol-character] (see `symbol-char?`) from
  collection v of form [index character] likely produced by `map-indexed`."
  [v]
  (filter #(symbol-char? (second %)) v))

(defn adjacent-indexes
  "Find indexes of adjacent points around indexes in a '1-dimensional grid',
  given the width of the grid."
  [indexes width]
  (let [loffsets [-1 (- 0 width 1) (- width 1)] ;up-left, left, down-left
        moffsets [width (- width)]              ;up, down
        roffsets (map - loffsets)]              ;up-right, right, down-right
    (apply concat                               ;Order doesn't matter here
           (map #(+ (first indexes) %) loffsets)
           (map #(+ (last indexes) %) roffsets)
           (for [i indexes] (map #(+ i %) moffsets)))))

(defn find-part-numbers
  "Find numbers in string s that are adjacent (up, down, left, right,
  or diagonally) to a symbol-character (see `symbol-char?`)."
  [s]
  (let [width (count (re-find #".*\n" s))
        indexed-chars (map-indexed vector s)
        sym-indexes (set (map first (find-indexed-symbols indexed-chars)))]
    (for [[is n] (find-indexed-numbers indexed-chars)
          :let [check-indexes (adjacent-indexes is width)]
          :when (some sym-indexes check-indexes)]
      n)))

(def star-1 (reduce + (find-part-numbers input-string)))

(def star-2 nil)

