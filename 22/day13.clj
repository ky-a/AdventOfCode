(ns aoc22.day13
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))


(def input (map #(str/split % #"\n") (str/split (slurp "resources/day13") #"\n\n")))

comp
(def data (map #(map edn/read-string %) input))

(defn is-ordered? [left right]
  (case (vector (int? left) (int? right))
    [true true] (if (= left right) nil (< left right))
    [true false] (is-ordered? [left] right)
    [false true] (is-ordered? left [right])
    [false false] (nth (remove nil? (map is-ordered? left right))
                       0
                       (if (= (count left) (count right))
                         nil (< (count left) (count right))))))

(reduce +
        (map first
             (get
              (group-by second
                        (map-indexed
                         (fn [index pair] (vector (inc index) (apply is-ordered? pair)))
                         data))
              true)))

;; Part 2

(def data-p2 (conj (apply concat data) '[[2]] '[[6]]))

(def sorted-by-order (apply sorted-set-by is-ordered? data-p2))

(apply *
       (map first
            (filter #(or (= [[2]] (second %)) (= [[6]] (second %)))
                    (map-indexed #(vector (inc %1) %2) sorted-by-order))))


(reduce (fn [state new] (conj state new)) [0] [0 1 2 3])