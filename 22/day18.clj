(ns aoc22.day18
  (:require [clojure.string :as str]))

(defn parse-coordinates [s]
  (mapv #(Integer/parseInt %) (str/split s #",")))

(defn x [c] (nth c 0))
(defn y [c] (nth c 1))
(defn z [c] (nth c 2))

(def input 
  (mapv #(mapv inc %) ; pad input & assure empty border
        (map parse-coordinates 
             (str/split (slurp "resources/day18") #"\n"))))

(def bounds 
  (reduce
   (fn [b c]
     (let [[min-c max-c] b]
       [(mapv min c min-c) (mapv max c max-c)]))
   [(first input) (first input)]
   input))

(def span 
  (reverse
   (mapv (comp inc inc) ; pad input & assure empty border
         (second bounds))))

(defn matrix [dims init] (reduce #(apply vector (repeat %2 %1)) init dims))

; populate matrix
(def voxels (reduce 
             (fn [s c] 
               (println c)
               (assoc-in s c true)) 
             (matrix span false)
             input))

(assert (= (count input) (count (filter true? (flatten voxels)))))

(def faces [[-1 0 0] [1 0 0]
            [0 -1 0] [0 1 0]
            [0 0 -1] [0 0 1]])

(defn adjacent-voxels [vox] (map (fn [face] (mapv + face vox)) faces))

(reduce (fn [uncovered-faces vox]
          (+ uncovered-faces (->> (adjacent-voxels vox)
                                  (map #(get-in voxels % false))
                                  (remove true?)
                                  (count)))) 
        0
        input)

;; Part 2 - Collect all neighbors in a set, polyfill until reaching a world border or fill exhausts. If exhausts - paint voxels

(defn polyfill [voxels start]
  (loop [open #{start}
         closed #{}]
    (if (empty? open) closed
        (let [vox (first open)
              frontier (->> vox
                            (adjacent-voxels)
                            (remove #(contains? closed %))
                            (remove #(get-in voxels % true)))]
          (recur (-> open (disj vox) (into frontier))
                 (-> closed (conj vox)))))))

; spread steam - polyfill around the voxel formation
(def outside-area (polyfill voxels [0 0 0]))

; set outside-area-voxels to empty
(def voxels-filled (reduce 
                    (fn [s c] 
                      (assoc-in s c false)) 
                    (matrix span true) 
                    outside-area))

; count faces
(reduce (fn [uncovered-faces vox]
          (+ uncovered-faces (->> (adjacent-voxels vox)
                                  (map #(get-in voxels-filled % false))
                                  (remove true?)
                                  (count))))
        0
        input)