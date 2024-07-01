(ns aoc22.day10
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/day10") #"\n"))

(defn parse-instruction [line]
  (let [split (str/split line #" ")
        instr (case (first split)
                "addx" :addx
                "noop" :noop)]
    (if (= :addx instr)
      (vector instr (Integer/parseInt (second split)))
      (vector instr))))

(def instructions (map parse-instruction input))

(defn execute [state instr]
  (let [cycles (get state :cycles)
        x (get state :x)]

    (case (first instr)
      :noop (assoc state :cycles (conj cycles x))
      :addx (assoc state
                   :cycles (apply conj cycles (repeat 2 x))
                   :x (+ x (second instr))))))

(def state {:cycles [] :x 1})

(def cycles (get (reduce execute state instructions) :cycles))

(def cycles-of-interest (range 20 221 40))

(filter (cycles-of-interest))

(reduce + (map #(* % (nth cycles (dec %))) cycles-of-interest))


;; Part 2

(defn evaluate-pixel [x cycle]
  (>= 1 (abs (- x cycle))))

(defn evaluate-pixels [cycles width] 
  (map-indexed
   (fn [index cycle] (evaluate-pixel (mod index width) cycle))
   cycles))

(map #(apply str %) 
     (partition 40 (map #(if % \# \.) 
                        (evaluate-pixels cycles 40))))