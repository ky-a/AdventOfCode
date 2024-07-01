(ns aoc22.day12
  (:require [clojure.string :as str]
            [aoc22.day9 :as day9]
            [clojure.math :as math]
            [aoc22.render :as render]))


(def input (str/split (slurp "resources/day12") #"\n"))

(def world (mapv (fn [line] (mapv (fn [height] (int height)) line)) input))

(defn get-cell-at [world xy]
  (hash-map :xy xy  :height (nth (nth world (second xy) '[]) (first xy) -1)))

(def directions '[[0 1] [0 -1] [1 0] [-1 0]])
(defn get-neighbors [cell]
  (filter (fn [nc] (< 0 (:height nc)))
          (map #(get-cell-at world %) 
                (map #(day9/add-vec (:xy cell) %) directions))))

(defn can-step? [from to]
  (>= (inc (:height from)) (:height to)))

(defn find-in-matrix [matrix value]
  (mapcat concat ; reduce to single-layer list
   (keep-indexed (fn [y row]
                   (keep-indexed (fn [x char] 
                                   (if (= value char) [x y] nil)) row))
                 matrix)))

(def start-loc (first (find-in-matrix input \S)))
(def target-loc (first (find-in-matrix input \E)))

(defn add-key [cell]
  (vector (:xy cell) cell))\

(def initial-state {:open {start-loc (assoc (get-cell-at world start-loc) :cost 0 :height 97)}
                    :closed {}
                    :target target-loc})

(defn expand-open [state expand]
  (let [open (:open state)
        closed (:closed state)
        expand+ (map (fn [cell] 
                       {:e cell 
                        :o (get open (:xy cell)) 
                        :c (get closed (:xy cell))
                        })
                     expand)
        blocked-by-closed (group-by #(>= (:cost (:e %)) (:cost (:c %) Integer/MAX_VALUE)) ; blocked if h(e) >= h(c)
                                    expand+) ; if there is a closed cell
        blocked-by-opened (group-by #(>= (:cost (:e %)) (:cost (:o %) Integer/MAX_VALUE)) 
                                    (get blocked-by-closed false)) 
        expanding (map add-key (map :e (get blocked-by-opened false '[])))
        ]
    (assoc state
           :closed (dissoc closed (map first expanding)) ; re-open closed
           :open (into open expanding))
    ))

(defn close-cell [state cell]
  (let [nopen (dissoc (:open state) (:xy cell))
        nclosed (assoc (:closed state) (:xy cell) cell)]
    (assoc state 
           :closed nclosed
           :open nopen
           :fin (or (empty? nopen) (= (:target state) (:xy cell))))))

(defn add-path [cell prev]
  (assoc cell :prev (dissoc prev :prev)))

(defn add-cost [cell]
  (assoc cell :cost (inc (:cost (:prev cell)))))

(defn distance [v0 v1]
  (math/sqrt (apply + (mapv #(* % %) (mapv - v0 v1)))))

(defn pick-cell [open heuristic]
  (let [sorted-open (into 
                     (sorted-map-by 
                      (fn [k0 k1] 
                        (let [c0 (get open k0) c1 (get open k1)] 
                          (compare [(heuristic c0) k0] [(heuristic c1) k1])))) 
                     open)]
    (second (first sorted-open))))

(defn search-step [state]
  (let [cell (pick-cell (:open state)  (fn [cell] (+ (:cost cell) (* 10 (distance (:xy cell) (:target state))))))
        expand (filter #(can-step? cell %) (get-neighbors cell)) 
        expand+ (map add-cost (map #(add-path % cell) expand))
        ]
    (close-cell (expand-open state expand+) cell)))

(loop [state initial-state]
  (if-not (:fin state)
    (recur (search-step state))
    state))

(defn draw-map [world state pixfun]
  (map-indexed
   (fn [y row] 
     (apply str (map-indexed (fn [x c] 
                               (pixfun state x y c))
                             row)))
   world))

(draw-map world initial-state (fn [_ _ _ c] (char c)))


(defn draw-prevs [state x y default]
  (cond
    (contains? (:open state) [x y])
    \#
    (contains? (:closed state) [x y])
    (let
     [c (get (:closed state) [x y])
      dxy (mapv - (:xy c) (:xy (:prev c) '[0 0]))]
      (case dxy
        [0 -1] \^
        [0 1] \v
        [1 0] \<
        [-1 0] \>
        \.))
    :else default))

(def output (iterate search-step initial-state))

(def final (nth output 4139))

(render/render-states 
 {:world world 
  :states (take 4139 output)  
  :pixel-at (fn [x y world state] 
            (cond
              (contains? (:closed state) [x y]) [100 100 0]
              (contains? (:open state) [x y]) [100 0 100]
              (= target-loc [x y]) [0 255 0]
              (= start-loc [x y]) [0 0 255]
              :else (let [g (* 255 (/ (- (nth (nth world y) x) (int \a)) 24))] (vector g g g))))
  :size 10} 
 render/render-tiles)

(count world)
(count (first world))
(get (:closed final) target-loc)

(defn get-path [cells] 
  (loop [loc target-loc
         path '[]]
    (let [npath (conj path loc)] 
      (if (= loc start-loc)
        npath
        (recur (:xy (:prev (get cells loc))) 
               npath)))))

(get-path (:closed final))

(draw-map input (nth output 4139) draw-prevs)

(draw-map input initial-state (fn [_ _ _ d] d))

(draw-map input (get-path (:closed final)) 
          (fn [s x y d] (if (some #(= (vector x y) %) s) \. d)))

(inc (count (get-path (:closed final))))

;; Part 2

(def initial-state-p2 {:open {target-loc (assoc (get-cell-at world target-loc) :height (int \z) :cost 0)} :closed {}})

(defn calculate-paths [state]
  (let [open (:open state)
        cell (pick-cell open (fn [cell] (:cost cell)))
        nexts (filter #(can-step? % cell) (get-neighbors cell))
        new-nexts (filter  #(not (or (get open (:xy %)) (get (:closed state) (:xy %)))) nexts)
        fin-nexts (map add-key (map #(assoc % :cost (inc (:cost cell))) new-nexts))]
    (close-cell (assoc state :open (into open fin-nexts)) cell))
  )

(not nil)
(nth (iterate calculate-paths initial-state-p2) 30)

(def all-costs (map second (:closed (loop [state initial-state-p2]
                                      (if (true? (:fin state))
                                        state
                                        (recur (calculate-paths state)))))))

(first (sort-by :cost (filter (fn [cell] (= (:height cell) (int \a))) all-costs)))

