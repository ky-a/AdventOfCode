(ns aoc22.render
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; {:world :states :pixel-at :size :x :y :w :h}

(defn render-states [info render-state]
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (let [world (:world info)
        size (:size info 1)
        info+ (assoc info
                     :x (:x info 0)
                     :y (:y info 0)
                     :w (inc (:w info (count (first world))))
                     :h (inc (:h info (count world))))]
    (q/defsketch map-sketch
      :title "Yo"
      :size [(* size (:w info+)) (* size (:h info+))] 
      :setup (fn [] (q/frame-rate 60) info+)
      :update (fn [state]
                (let [index (:index state 0)
                      states (:states state)]
                  (assoc state :index (if (>= index (dec (count states))) 0 (inc index)))))
      :draw (fn [state] 
              (q/background 200 200 200)
              (render-state state))
      :features [:keep-on-top]
      :middleware [m/fun-mode])))

(nth [[1 2] [2 3]] 0)

(defn render-tiles [info]
  (let [state (nth (:states info) (:index info 0))
        world (:world info)
        pixel-at (:pixel-at info (fn [& _] [255 0 0])) 
        s (:size info 1)]
    ;(q/no-stroke)
    (doseq [x (range (:x info) (+ (:w info) (:x info)))
            y (range (:y info) (+ (:h info) (:y info)))]
      (apply q/fill (pixel-at x y world state))
      (q/rect (* s (- x (:x info))) (* s (- y (:y info))) s s))))