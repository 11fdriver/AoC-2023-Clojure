(ns aoc-23.day-08.haunted-wasteland
  (:require [clojure.string :as str]))

(def input-string (slurp "inputs/08"))

(def direction->fn
  {\L #(nth % 0), \R #(nth % 1)})

(defn parse-desert-map
  [s]
  "Parses string s and returns vector containing:
1. Vector of functions that make one move from a node.
2. Hash map between node names and a list of their two adjacent nodes."
  (let [[directionsstr nodesstr] (str/split s #"\n\n" 2)
        directions (map direction->fn directionsstr)
        nodes (reduce (fn [m [_ k l r]]
                        (assoc m k [l r])) {}
                      (re-seq #"([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)" nodesstr))]
    [directions nodes]))

(def start-node "Name to start from." "AAA")
(def finish-node "Name to end at." "ZZZ")

(defn traverse-desert-map-to-pred
  "Traverses desert map `nodes` according to functions in `moves` from `start`
  node until `end?` has been satisfied, returning path taken as vector of
  nodes."
  [moves nodes start end?]
  (loop [[dirf & fs] (cycle moves)
         visited [start]]
    (let [current (peek visited)]
      (if (end? current)
        visited
        (recur fs (conj visited (dirf (get nodes current))))))))

(defn traverse-desert-map
  "Returns amount of steps taken through desert given a vector of movement
  functions `moves` and a map `nodes` between node-names and their two adjacent
  nodes."
  [moves nodes]
  (->> (traverse-desert-map-to-pred
        moves nodes start-node #(= finish-node %))
       (count)
       (dec)))

(def star-1 (->> input-string
                 (parse-desert-map)
                 (apply traverse-desert-map)))
;; (= 13939 star-1)

(defn ghost-traverse-desert-map
  "Return amount of steps taken to simultaneously travel between all #\"..A\"
  nodes to a #\"..Z\" node, ending at the same time. Exploits that the node
  sequences repeat a predictable number of times, and finds lowest common
  multiple."
  [moves nodes]
  (let [ghost-start-nodes (filterv #(re-matches #"..A" %) (keys nodes))
        ghost-finish-node? #(re-find #"..Z" %)
        gcd #(if (zero? %2) %1 (recur %2 (mod %1 %2)))
        lcm #(* (/ %1 (gcd %1 %2)) %2)]
    (->> ghost-start-nodes
         (map #(traverse-desert-map-to-pred moves nodes % ghost-finish-node?))
         (map count)
         (map dec)
         (reduce lcm))))

(def star-2 (->> input-string
                 (parse-desert-map)
                 (apply ghost-traverse-desert-map)))
;; (= 8906539031197 star-1)
