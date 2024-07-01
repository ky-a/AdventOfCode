(ns aoc22.day8
  (:require [clojure.string :as str]))

(def input (map (fn [row] (map #(Integer/parseInt (str %)) row))
                (str/split (slurp "resources/day8") #"\n")))


(defn tree-visible [x y row col]
  (let [top-bot (split-at y col)
        lef-rig (split-at x row)
        tree (nth row x)]
    (let [top (first top-bot)
          bot (drop 1 (second top-bot))
          lef (first lef-rig)
          rig (drop 1 (second lef-rig))
          larger-than-tree? #(>= % tree)]
      (let [lanes (vector top bot lef rig)]
        (some true?
              (map #(empty? (filter larger-than-tree? %)) lanes))))))

(defn iterate-grid [fun w h]
  (map-indexed (fn [y _] (map (fn [x] (fun x y)) (range 0 w))) (range 0 h)))

(defn calculate-on-grid [cell-fun input]
  (let [rows input
        cols (apply mapv vector input)]
    (iterate-grid (fn [x y]
                    (cell-fun x y
                              (nth rows y)
                              (nth cols x)))
                  (count cols) (count cols))))

(count (filter some? (flatten (calculate-on-grid tree-visible input))))

;; Part 2

(defn scenic-score [x y row col]
  (let [top-bot (split-at y col)
        lef-rig (split-at x row)
        tree (nth row x)]
    (let [top (reverse (first top-bot))
          bot (drop 1 (second top-bot))
          lef (reverse (first lef-rig))
          rig (drop 1 (second lef-rig))
          smaller-than-tree #(< % tree)]
      (let [lanes (vector top bot lef rig)]
        (apply * (map
                  (fn [lane]
                    (min (inc (count (take-while smaller-than-tree lane)))
                         (count lane)))
                  lanes))))))

(calculate-on-grid scenic-score (repeat 10 (range 0 10)))
(repeat 10 (range 0 10))
(apply max (flatten (calculate-on-grid scenic-score input)))


