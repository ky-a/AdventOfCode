(ns aoc22.day15
  (:require [aoc22.day14 :refer [line-to find-and-parse-ints]]
            [clojure.string :as str]
            [aoc22.render :as render]))
  
(def input (map find-and-parse-ints (str/split (slurp "resources/day15") #"\n")))

(defn manhattan-distance [a b]
    (apply + (mapv (comp abs -) a b)))

(comment
  ;; Well, you got me. The naive approach is computationally much too intensive.

  (defn manhattan-half-circle [x y r dy]
    (if (< r 0)
      {}
      (into (set (line-to [(- x r) y] [(+ x r) y]))
            (lazy-seq (manhattan-half-circle x (+ y dy) (- r 1) dy)))))


  (defn manhattan-circle [x y r]
    (into (manhattan-half-circle x y r -1)
          (manhattan-half-circle x y r 1)))


  (def sensor-ranges
    (pmap (fn [[sx sy bx by]]
            (manhattan-circle sx sy (manhattan-distance [sx sy] [bx by])))
          (take 0 input)))
  )


(def sensors (map (fn [[sx sy bx by]] 
                   {:x sx :y sy :r (manhattan-distance [sx sy] [bx by])})
                  input))

(defn horizontal-sensor-slice [sensor y]
  (let [dy (abs (- (:y sensor) y))
        r-at-y (- (:r sensor) dy)
        x (:x sensor)]
    (if (< r-at-y 0) 
      ()
      (line-to [(- x r-at-y) y] [(+ x r-at-y) y]))))

(def combined-sensor-slice 
  (reduce into #{} (pmap #(horizontal-sensor-slice % 2000000) sensors)))

(dec (count combined-sensor-slice))

;; Part 2

(def directions 
  [[0 -1]
   [1 0]
   [0 1]
   [-1 0]])

(defn get-rim [sensor]
  (let [r (inc (:r sensor))
        x (:x sensor)
        y (:y sensor)]
    (apply concat
           (map (fn [[a b]] (line-to a b)) 
                (partition 2 1 (map (fn [d]
                                      (map + [x y] [(* r (nth d 0)) 
                                                    (* r (nth d 1))])) 
                                    (take 5 (cycle directions))))))))

(defn in-range? [p [sxy sr]]
  (<= (manhattan-distance p sxy) sr))

(defn in-any-range? [p sensors]
  (some #(in-range? p %) sensors))

(defn nearby? [[x y]]
  (and (<= 0 x 4000000) (<= 0 y 4000000)))

(def rims (pmap (fn [sensor]
                  (vector sensor
                          (filter nearby? (get-rim sensor)))) 
                sensors))

(def distress
  (pmap (fn [[sensor rim]] 
            (let [sensors-sorted (mapv 
                                  (fn [s] [[(:x s) (:y s)] (:r s)])
                                  (sort-by #(manhattan-distance [(:x sensor) (:y sensor)] [(:x %) (:y %)])
                                           sensors))] 
              (pmap (fn [p] (if (not (in-any-range? p sensors-sorted)) p nil)) rim)))
          (take 1 (drop 6 rims))))

(def distress-signal (first (some (fn [c] (not-empty (filter some? c))) distress)))

(defn tuning-frequency [[x y]] (+ y (* x 4000000)))

(tuning-frequency distress-signal)