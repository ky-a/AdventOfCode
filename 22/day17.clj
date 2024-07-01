(ns aoc22.day17
  (:require [clojure.string :as str]))

(def input (cycle (map #(case %1 \< -1 \> 1) (slurp "resources/day17"))))

(defn mapcat-indexed [f & colls]
  (apply concat (apply map-indexed f colls)))

(def shapes
  (map (fn [form]
         {:w (apply max (map count form))
          :h (count form)
          :blocks (apply vector
                         (filter some?
                                 (mapcat-indexed
                                  (fn [y row]
                                    (map-indexed
                                     (fn [x block]
                                       (if (= \# block) [x y] nil))
                                     row))
                                  form)))})
       (map str/split-lines
            (str/split
             (slurp "resources/day17-shapes")
             #"\n\n"))))

(defn draw-world [world shape]
  (let [s-blocks (map (fn [block]
                        (vector (+ (first block) (:x shape))
                                (- (:y shape) (second block))))
                      (:blocks shape))]
    (doseq [[y row] (map-indexed #(vector %1 %2) world)]
      (println y "|" (apply str
                            (map-indexed
                             (fn [x b]
                               (case [b (not (empty? (some #{[x y]} s-blocks)))]
                                 [true true] \!
                                 [true false] \#
                                 [false true] \@
                                 [false false] \.))
                             row))))))

(defn intersect? [shape world peak]
  (let [sx (:x shape)
        sy (:y shape)
        blocks (:blocks shape)]
    (or (< sx 0)
        (> (+ sx (:w shape)) 7)
        (and (<= (- sy (:h shape)) peak) 
             (some? (some (fn [[bx by]]
                            (nth (nth world (- sy by)) (+ bx sx)))
                          blocks))))
    ))


(defn keep-world-buffer [world peak size]
  (let [world-height (count world)
        buffer (- world-height peak)]
    (if (< buffer size)
      (apply conj world (repeat (- size buffer) (apply vector (repeat 7 false))))
      world)))

(defn spawn-shape [shape peak]
  (into {:x 2 :y (+ peak 3 (:h shape))} shape))

(defn move-shape [world shape [vx vy] peak]
  (let [shape' (assoc shape
                      :x (+ vx (:x shape))
                      :y (+ vy (:y shape)))]
    (if (intersect? shape' world peak) [false shape] [true shape'])))

(defn embed-shape [world shape]
  (let [sx (:x shape)
        sy (:y shape)]
    (reduce (fn [w [bx by]] (assoc-in w [(- sy by) (+ sx bx)] true)) world (:blocks shape))))

(defn step [state vx]
  (let [world (:world state)
        shape (:shape state)
        peak (:peak state 0)
        [[pushed? shape-pushed] 
         [diagon? shape-diagon]] (pmap #(move-shape world shape % peak) [[vx 0] [vx -1]])
        [moving? new-shape] (case [pushed? diagon?]
                                   [true true] [true shape-diagon]
                                   [true false] [false shape-pushed]
                                   [false true] (move-shape world shape [0 -1] peak) ; redo fall straight down
                                   [false false] (move-shape world shape [0 -1] peak))] 
    (if (not moving?)
      (let [new-peak (max (:y new-shape) peak)]
        (assoc state
               :world (keep-world-buffer (embed-shape world new-shape) 10 peak)
               :shape (spawn-shape (first (:shapes state)) new-peak) 
               :shapes (next (:shapes state))
               :fallen (inc (:fallen state 0))
               :peak new-peak))
      (assoc state :shape new-shape))))

(def init-state {:shape (spawn-shape (first shapes) 0)
                 :world (keep-world-buffer [(apply vector (repeat 7 true))] 20 1)
                 :shapes (next (cycle shapes))})

(def out (loop [state init-state
                input input]
           (if (>= (:fallen state 0) 2022)
             state 
             (recur (step state (first input)) (next input)))))

(draw-world (:world out) (:shape out))

(let [state (reduce step init-state (take 100 input))]
  (println (:peak state))
  (draw-world (:world state) (:shape state)))

;; Part 2

(def trunc-amount 10000)

(comment
  (def out-p2 (loop [state init-state
                     input input]
                (if (>= (:fallen state 0) 1000000000000)
                  state
                  (if (>= (:peak state 0) (+ trunc-amount 200)) 
                    (or 
                     (println (* (:trunced state 0) 100.0 (/ trunc-amount 1000000000000)) "%")
                     (recur (step (assoc state
                                         :trunced (inc (:trunced state 0))
                                         :peak (- (:peak state) trunc-amount)
                                         :world (apply vector (drop trunc-amount (:world state)))
                                         :shape (assoc (:shape state) :y (- (:y (:shape state)) trunc-amount)))
                                  (first input)) (next input)))
                    (recur (step state (first input)) (next input))))))
  )

;; parralel mapping doesn't seem to work
;; one might simulate all falling pieces at the same time, in parralel...