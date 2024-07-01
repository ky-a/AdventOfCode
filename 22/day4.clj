;;; Advent of Code Day 4
(ns aoc22.day4
  (:require [clojure.string :as str] [clojure.set :as set]))

;; code to safely parse string to int as found on https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number/9190531
(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(def input (map #(str/split % #",") (str/split (slurp "resources/day4") #"\n")))

(defn parse-range [string]
  (let [split (map parse-int (str/split string #"-"))]
    (set (range (first split) (+ 1 (second split))))))

(def ranges (map #(map parse-range %) input))

(defn any-subset? [sets]
  (= (reduce min (map count sets)) (count (apply set/intersection sets))))

(count (filter any-subset? ranges))

;; Part 2

(count (filter #(not-empty (apply set/intersection %)) ranges))