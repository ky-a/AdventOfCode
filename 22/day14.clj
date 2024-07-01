(ns aoc22.day14
  (:require [clojure.string :as str]
            [aoc22.render :as render]))

(defn find-and-parse-ints [string] 
  (mapv #(Integer/parseInt %) (re-seq #"[-+]?[0-9]+" string)))

(def input 
  (map #(map find-and-parse-ints (str/split % #" -> ")) 
       (str/split (slurp "resources/day14") #"\n")))

(defn clamp [v min-v max-v] (min (max v min-v) max-v))

(defn line-to [pos target]
  (let [[x y] pos
        [tx ty] target
        dx (clamp (- tx x) -1 1)
        dy (clamp (- ty y) -1 1)]
    (cons pos (lazy-seq (if (= pos target) nil (line-to [(+ x dx) (+ y dy)] target))))))

(defn bounds-of [xys]
  (let [xs (flatten (map first xys))
        ys (flatten (map second xys))
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    {:x min-x
     :y min-y
     :max-x max-x
     :max-y max-y
     :w (- max-x min-x)
     :h (- max-y min-y)}))

(defn build-world [lines]
  (set (mapcat
        (fn [stroke]
          (mapcat
           (fn [[a b]] (line-to a b))
           (partition 2 1 stroke)))
        lines))) 

(def world (build-world input))

(defn ray-intersect [from to intersect?]
  (let [line (line-to from to)
        line-until-hit (take-while (comp not intersect?) line)]
    (if (= (count line) (count line-until-hit)) nil (last line-until-hit))))

(def lower-diagonals [[-1 1] [1 1]])

(defn simulate [state]
  (let [sands (:sand state)
        world (:world state)]
    (loop [sand (:spawn state)]
      (let [sand-fall (ray-intersect sand (mapv + sand [0 1000])
                                     #(or (get sands %) (get world %)))
            sand-shift (nth (filter #(nil? (or (get sands %) (get world %))) 
                                      (map #(mapv + sand-fall %) lower-diagonals))
                            0 sand-fall)]
        (if (or (= nil sand-fall) (contains? sands sand))
          (assoc state :fin true)
          (if (= sand-shift sand-fall)
            (assoc state :sand (conj sands sand-fall))
            (recur sand-shift)))) 
      )))

(comment
  (def states (take-while #(false? (:fin %))
                          (iterate simulate {:world world
                                             :sand #{}
                                             :spawn [500 0]
                                             :fin false})))

  (count (:sand (last states)))

  (render/render-states
   (merge
    (bounds-of world)
    {:world world
     :states (drop 600 states)
     :size 8
     :pixel-at (fn [x y world state]
                 (cond
                   (contains? world [x y]) [0 0 0]
                   (contains? (:sand state) [x y]) [100 100 0]
                   (= [500 0] [x y]) [255 255 0]
                   :else [255 255 255]))
     :y 0
     :h 180})
   render/render-tiles)
  )

;; Part 2

(comment 
  (def infinite-line-y (+ 2 (:max-y (bounds-of world))))

  (def world-p2 (into world (line-to [200 infinite-line-y] [800 infinite-line-y])))

  (def states-p2 (take-while #(false? (:fin %))
                             (iterate simulate {:world world-p2
                                                :sand #{}
                                                :spawn [500 0]
                                                :fin false})))

  (count (:sand (last states-p2)))

  (render/render-states
   (merge
    (bounds-of world-p2)
    {:world world-p2
     :size 2
     :states (drop 26000 states-p2)
     :pixel-at (fn [x y world state]
                 (cond
                   (contains? world [x y]) [0 0 0]
                   (contains? (:sand state) [x y]) [100 100 0]
                   (= [500 0] [x y]) [255 255 0]
                   :else [255 255 255]))
     :y 0
     :h 180})
   render/render-tiles)
  )