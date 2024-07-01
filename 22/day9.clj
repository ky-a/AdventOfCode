(ns aoc22.day9
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn parse-instruction [line]
  (vector (keyword (first line)) (Integer/parseInt (second line))))

(def input (map parse-instruction (map #(str/split % #" ") (str/split (slurp "resources/day9") #"\n"))))

(defn add-vec [v0 v1] (mapv #(+ %1 %2) v0 v1))

(def state {:knots (apply vector (repeat 10 '[0 0])) :trail #{}})

(def directions
  {:L '[-1 0]
   :R '[1 0]
   :U '[0 1]
   :D '[0 -1]})

(defn distance [loc0 loc1] (mapv - loc0 loc1))

(defn move-knot [knot head]
  (let [distance (distance head knot)
        want-move (not (empty? (filter #(> (abs %) 1) distance)))
        move-by (mapv #(max -1 (min 1 %)) distance)]
    (if want-move move-by '[0 0])))

;; builds a vector of knots, updating the knots 1-by-1
(defn update-knots [head knots] 
  (reduce (fn [out-knots knot] 
            (let [leader (last out-knots)]
              (conj out-knots (add-vec knot (move-knot knot leader))))) 
          (vector head) knots))

(defn world-update [state instruction]
  (let [knots (get state :knots)
        head (first knots)
        mv-head (get directions instruction)
        n-knots (update-knots (add-vec head mv-head) (next knots))
        trail (get state :trail)]
    (assoc state :knots n-knots :trail (conj trail (last n-knots)))))

(def instructions (flatten (map #(repeat (second %) (first %)) input)))

(count (get (reduce world-update
                    {:knots '[[0 0] [0 0]] :trail #{}}
                    instructions) 
            :trail))

;; Part 2

(count (get (reduce world-update 
                    {:knots (apply vector (repeat 10 '[0 0])) :trail #{}} 
                    instructions)
            :trail))