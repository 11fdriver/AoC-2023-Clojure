(ns aoc-23.day-05.if-you-give-a-seed-a-fertilizer
  (:require [clojure.string :as str]))

(def input-string (slurp "inputs/05"))

(defn parse-mappings
  "Return collection of mappings in format [dest-start src-start range-length]
  given string s of \"number number number\"."
  [s]
  (let [parse-mapping #(mapv parse-long (str/split % #" "))]
    (->> s
         (str/split-lines)
         (rest)                         ;Remove title
         (map #(parse-mapping %)))))

(defn almanac->seeds
  "Return collection of seeds (as numbers) from an almanac string s."
  [s]
  (let [seedstr (re-find #".*(?=\n)" s)]
    (mapv parse-long (rest (str/split seedstr #" ")))))

(defn almanac-mappings->transforms
  "Return collection of functions given collection coll of mappings in format of
  `parse-mappings`. Each function returns an offset number when in range, or nil
  when the value is out of range."
  [coll]
  (letfn [(mapping->f [[dest src length]]
            #(when (<= src % (+ src (dec length)))
               (+ % (- dest src))))]    ;Returns nil if not in range
    (map mapping->f coll)))

(defn condense-transforms
  "Return a final transform function that takes a number & returns the first valid
  transform or the unaltered value. Takes collection coll of transform functions
  given by `almanac-mappings->transforms`."
  [coll]
  #_(first (remove nil? ((apply juxt coll) %)))
  #(or (some identity ((apply juxt coll) %)) ;First non-nil
       %))                              ;N when all not-in-range

(defn almanac->transforms
  "Return collection of transform functions from an almanac string s. See
  `condense-transforms`, `almanac-mappings->transforms` & `parse-mappings`."
  [s]
  (let [[_ & seedmapstrs] (str/split s #"\n\n")]
    (->> seedmapstrs
         (map parse-mappings)
         (map almanac-mappings->transforms)
         (map condense-transforms))))

(defn lowest-seed-location
  "Find smallest final location for any seed in seeds after being progressively
  transformed by functions in collection transforms."
  [seeds transforms]
  (let [thrush (apply comp (reverse transforms))]
    (->> seeds
         (map thrush)
         (apply min))))

(def star-1 (lowest-seed-location
             (almanac->seeds input-string)
             (almanac->transforms input-string)))
;; (= 323142486 star-1)

(defn almanac->seed-ranges
  "Return collection of ranges from an almanac string input s in format [start
  end] (inclusive)."
  [s]
  (letfn [(startlen->range [[start n]]
            [start (+ start (dec n))])]
    (->> (partition 2 (almanac->seeds s))
                      (map startlen->range))))

(defn range-overlap
  "Return the overlap between two ranges given by the start and end values for
  each, or nil if there is no overlap."
  [x1 x2 y1 y2]
  (when (and (<= x1 y2) (<= y1 x2))
    [(max x1 y1) (min x2 y2)]))

"Return collection of functions given collection coll of mappings in format of
  `parse-mappings`. Each function returns an offset number when in range, or nil
  when the value is out of range."

(defn almanac-mappings->range-transforms
  "Return collection of functions from input coll of range mappings in format of
  `parse-mappings`. Each function takes one range vector and returns the
  overlapping section according to `range-overlap`."
  [coll]
  (letfn [(destsrc->rangeoff [[dest src length]]
            [src (+ src (dec length)) (- dest src)])
          (interval-between [[_ x2 _] [y1 _ _]]
            (let [start (inc x2) end (dec y1)]
              (when (<= start end) [start end 0])))
          (mapping->f [[start end offset]]
            (fn [[a b]] (map #(+ % offset)
                             (range-overlap a b start end))))]
    (let [ranges (sort-by #(nth % 0) (mapv destsrc->rangeoff coll))
          high-range [(inc (apply max (map second ranges))) ##Inf 0]
          low-range  [##-Inf (dec (apply min (map first ranges))) 0]
          intervals (keep identity (map interval-between ranges (drop 1 ranges)))]
      (map mapping->f (conj (concat ranges intervals) high-range low-range)))))

(defn condense-range-transforms
  "Return a single function that represents one set of range mappings functions,
  only including the relevant sections from input coll of range-transform
  functions (see `almanac-mappings->range-transforms`). There may be multiple as
  an input range could span across several mapping ranges."
  [coll]
  #(keep seq ((apply juxt coll) %)))

(defn almanac->range-transforms
  "Return collection of functions, each representing one set of maps, from an
  almanac string s."
  [s]
  (let [[_ & seedmapstrs] (str/split s #"\n\n")]
    (->> seedmapstrs
         (map parse-mappings)
         (map almanac-mappings->range-transforms)
         (map condense-range-transforms))))

(defn lowest-seed-range-location
  "Find lowest final seed location, from inputs seed-ranges of range-vectors of
  seeds and range-transforms of range-transform functions. (See
  `almanac->seed-ranges` & `almanac->range-transforms`)."
  [seed-ranges range-transforms]
  (->> (reduce #(mapcat %2 %1) seed-ranges range-transforms)
       (map #(nth % 0))
       (apply min)))

(def star-2 (lowest-seed-range-location
             (almanac->seed-ranges input-string)
             (almanac->range-transforms input-string)))
;; (= 79874951 star-2)

