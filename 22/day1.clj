;;; Advent of Code Day 1
(ns aoc22.day1 
  (:require [clojure.string :as str]))

;; code to safely parse string to int as found on https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number/9190531
(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(defn split-blocks [s]
  (str/split s #"\n\n"))

;; fetch data from file and parse it
(def data (slurp "resources/day1"))

(def foods
  (map 
   #(map parse-int (str/split % #"\n")) 
   (split-blocks data)))

(def food-sums
  (map #( reduce + %) foods))

(reduce max food-sums)

;; part 2
(sort food-sums)

(reduce + (take-last 3 (sort food-sums)))