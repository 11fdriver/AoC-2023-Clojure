(ns aoc-23.main
  (:require [clojure.java.io :refer [as-file]]))

(defn -main
  "Print each src/aoc_23/day_*/*.clj file with results of stuff."
  [& _]
  (do
    (->> (file-seq (as-file "src"))
         (map #(.getPath %1))
         (filter #(re-matches #"src/aoc_23/day_[0-9][0-9]/.*\.clj" %1))
         (sort)
         (run! #(do (print "Loading --" %1) (flush) (load-file %1) (println " -- done."))))
    (println "\nSolutions:")
    (->> (all-ns)
     (map ns-name)
     (map str)
     (filter #(re-matches #"aoc-23\.day-.*" %1))
     (sort)
     (map symbol)
     (map ns-interns)
     (mapcat #(list (get %1 'star-1) (get %1 'star-2)))
     (map #(list %1 (deref %1)))
     (run! #(printf "%s is %s\n" (first %1) (second %1))))))
