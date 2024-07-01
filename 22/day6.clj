(ns aoc22.day6
  (:require [clojure.contrib.seq]))

(def input (slurp "resources/day6"))

(defn first-distinct-subseq [seq n]
  (first (filter #(= n (count (distinct (second %)))) 
                 (map-indexed vector (partition n 1 seq)))))

(+ 4 (first (first-distinct-subseq input 4)))

;; Part 2

(+ 14 (first (first-distinct-subseq input 14)))