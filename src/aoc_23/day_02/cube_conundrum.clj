(ns aoc-23.day-02.cube-conundrum
  (:require [clojure.java.io :as io]))

(def input-path "inputs/02")

(def max-cubes-map {:red 12, :green 13, :blue 14})

(defn parse-max-cubes
  "Take game data as string `s`, and collect maximum cubes of each colour along
  with id, returning as a map of format {:id n :colour1 m :colour2 ...}."
  [s]
  (let [init-map (assoc (zipmap (keys max-cubes-map) (repeat 0))
                        :id (read-string (re-find #"(?<=^Game )\d+" s)))
        colour-names (mapv name (keys max-cubes-map))
        re (re-pattern (str "\\d+(?= ("
                            (reduce #(str %1 \| %2) colour-names)
                            "))"))]
    (reduce (fn [m [n colour]] (update m (keyword colour) max (read-string n)))
            init-map
            (re-seq re s))))

(defn valid-game?
  [m]
  (every? #(<= (get m %) (get max-cubes-map %))
          (keys max-cubes-map)))

(def star-1
  (with-open [rdr (io/reader input-path)]
    (let [xf (comp (map parse-max-cubes)
                   (filter valid-game?)
                   (map :id))]
      (transduce xf + (line-seq rdr))))) ;=> 2176

(defn game-power
  [m]
  (reduce * (vals (dissoc m :id))))

(def star-2
  (with-open [rdr (io/reader input-path)]
    (let [xf (comp (map parse-max-cubes)
                   (map game-power))]
      (transduce xf + (line-seq rdr))))) ;=> 63700
