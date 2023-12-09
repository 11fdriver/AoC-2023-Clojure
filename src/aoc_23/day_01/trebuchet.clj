(ns aoc-23.day-01.trebuchet
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-path "inputs/01")

(defn only-digits
  "Extract digits from string into vector of single-char strings."
  [s]
  (re-seq #"\d" s))

(defn first-and-last
  "Get first & last items from vector as a vector of 2 items."
  [v]
  [(first v) (last v)])

(defn get-calibration-values
  "Get 'calibration value' (first & last digits from mixed alpha+number strings).
  E.g. 1abc2->12 pqr3stu8vwx->38 a1b2c3d4e5f->15 treb7uchet->77."
  [strings]
  (let [xf (binding [*read-eval* false]
             (comp (map only-digits)
                   (map first-and-last)
                   (map #(apply str %))
                   (map read-string)))]
    (sequence xf strings)))

(def star-1
  (with-open [rdr (io/reader input-path)]
    (reduce + (get-calibration-values (line-seq rdr))))) ;=> 54927

;;==============================================================================
;; Solution a bit weird, I wanted to just replace digit-words with the digits,
;; but I have to account for overlapping words (oneightwo -> 182) which took a
;; while to realise. I add the last letter back on to the replacement digit to
;; account for this, and then change only the first and last instances. Weird.

(def digit-words ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def digits-with-suffix (mapv #(str % (last (nth digit-words %))) (range 10)))
(def word->digit-map (zipmap digit-words digits-with-suffix))

(defn first-and-last-words->digits
  [s]
  (let [first-re (reduce #(str %1 \| %2) digit-words)
        last-re (str "(.*)(" first-re ")(.*)")]
    (some-> s
        (s/replace-first (re-pattern first-re) word->digit-map) ;map as function
        (s/replace-first (re-pattern last-re) #(str (% 1) (word->digit-map (% 2)) (% 3))))))

(def star-2
  (with-open [rdr (io/reader input-path)]
    (reduce + (get-calibration-values
               (map first-and-last-words->digits (line-seq rdr)))))) ;=> 54581

;;==============================================================================
;; Had a quick peek online, and found out you can use lookahead re's to allow
;; for overlapping matches. I should really have thought about whether I can
;; change `only-digits` to allow for digits plus overlapping digit words.

(def digit-words' ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def digits' (mapv str (range 10)))
(def word->digit-map' (zipmap digit-words' digits'))

(defn digits-or-digit-words
  "Extract digits or digit-words from string into "
  [s]
  (let [re (re-pattern (str "(?=("      ;Lookahead with matching-group
                            (reduce #(str %1 \| %2) (concat digits' digit-words'))
                            "))"))]
    (->> (re-seq re s)
         (map second)
         (map #(if-some [d (word->digit-map' %)] d %)))))

(defn get-correct-calibration-values
  "Get 'calibration value', i.e. first & last digits from mixed alpha+number
  strings, plus (overlapping) digit-words like 'one'.

  E.g. 1abc2->12 a1b2c3d4e5f->15 treb7uchet->77 1eighthree->13."
  [strings]
  (let [xf (binding [*read-eval* false]
             (comp (map digits-or-digit-words)
                   (map first-and-last)
                   (map #(apply str %))
                   (map read-string)))]
    (sequence xf strings)))

(def star-2'
  (with-open [rdr (io/reader input-path)]
    (reduce + (get-correct-calibration-values (line-seq rdr))))) ;=> 54581
