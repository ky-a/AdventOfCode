(ns aoc22.day5
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/day5") #"\n\n"))

(def input-config (drop-last (str/split (first input) #"\n")))
(print (first input))

(defn not-space [c] (not (Character/isSpace c)))

(def input-state
  (mapv #(filterv not-space %)
        (apply mapv
               vector (map #(map second (partition 3 4 %))
                           input-config))))

(def instructions
  (map #(apply hash-map %) (map #(str/split % #" ") (str/split (second input) #"\n"))))

(defn get-int [m k] (Integer/parseInt (get m k)))

(defn apply-instruction [state instruction]
  (let [amount (get-int instruction "move")
        source-index (dec (get-int instruction "from"))
        target-index (dec (get-int instruction "to"))]
    (let [source (nth state source-index)
          target (nth state target-index)
          taken (take amount (nth state source-index))]
      (let [new-source (vec (drop amount source))
            new-target (vec (reverse (apply conj (take amount source) target)))]
        (assoc state source-index new-source target-index new-target)))))

(apply str (map first (reduce apply-instruction input-state instructions)))

;; Part 2

(defn apply-instruction-9001 [state instruction]
  (let [amount (get-int instruction "move")
        source-index (dec (get-int instruction "from"))
        target-index (dec (get-int instruction "to"))]
    (let [source (nth state source-index)
          target (nth state target-index)
          taken (take amount (nth state source-index))]
      (let [new-source (vec (drop amount source))
            new-target (apply conj (vec (take amount source)) target)]
        (assoc state source-index new-source target-index new-target)))))

(apply-instruction-9001 input-state (first instructions))

(apply str (map first (reduce apply-instruction-9001 input-state instructions)))
