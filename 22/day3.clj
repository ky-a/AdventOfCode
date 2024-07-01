;;; Advent of Code Day 3
(ns aoc22.day3
  (:require [clojure.string :as str] [clojure.set :as set] [clojure.spec.alpha :as alpha]))

(def input (slurp "resources/day3"))

(def rucksacks (
                map #(list (set (first %)) (set (second %)))
                (map #(split-at (/ (count %) 2) %) 
                     (map seq (str/split input #"\n")))))

(def redundant-items (map #(set/intersection (first %) (second %)) rucksacks))

(defn item-priority [item]
  (let [i (int item)]
    (cond
      (alpha/int-in-range? 97 123 i) (- i 96)
      (alpha/int-in-range? 65 91 i) (- i 38))))


(reduce + (map #(item-priority (first %)) redundant-items))

;; Part 2

(def simple-rucksacks (map #(apply set/union %) rucksacks))

(def badges (map #(apply set/intersection %) (partition 3 simple-rucksacks)))

(reduce + (map item-priority (map first badges)))