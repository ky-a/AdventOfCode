;;; Advent of Code Day 3

;; --Imports--
(require '[clojure.string :as str])

;; code to safely parse string to int as found on https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number/9190531
(defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

;; split string on \n and map to integer
(defn parse [raw-data]
    (str/split raw-data #"\n"))

(defn parse-path [path-data]
    (str/split path-data #","))

;; fetch data from file and parse it
(def data (parse (slurp "data/day3")))

(def path0 (parse-path (nth data 0)))
(def path1 (parse-path (nth data 1)))

;;; ---Part 1---

(defn walk-line 
    "Returns list of positions 'visited' when walking [distance] steps in [direction] (L/R/U/D) starting at [position]" 
    [position direction distance]
    (let [dir (case direction
        \L [1 0]
        \R [-1 0]
        \U [0 1]
        \D [0 -1]
        )]
        (for [step (range 1 (inc distance))]
            [(+ (first position) (* (first dir) step))
              (+ (second position) (* (second dir) step))]
              )))

(defn list-fields [position path]
    "Recoursively 'walks lines' specified in [path]"
    (if (empty? path)
        [] ; recursion default
        (let [line (first path)] 
            (let [[direction distance] [(first line) (parse-int line)]] 
                (let [trace (walk-line position direction distance)] 
                    (concat (list-fields (last trace) (rest path)) trace)
                    )))))

(def trace0 (set (list-fields [0 0] path0)))
(def trace1 (set (list-fields [0 0] path1)))

(def intersect (clojure.set/intersection trace0 trace1))

;; Helper function for Manhattan distance.
(defn add-abs 
    "Add the absolutes of two values."
    [a b]
    (+ (Math/abs a) (Math/abs b)))


(def distances 
    (sort-by first 
        (map 
            #(vec [(reduce add-abs %) %]) ; safe position and its manhattan distance to [0 0]
            intersect)))

(first distances) ;solution to part 1

;;; ---Part 2---

;; add a step/timing value to the [position] field (and call it [progress])
(defn walk-line 
    "Returns list of positions 'visited' when walking [distance] steps in [direction] (L/R/U/D) starting at [progress] ([position] step)"
    [progress direction distance]
    (let [[position step] [(first progress) (second progress)]]
        (let [dir (case direction
            \L [1 0]
            \R [-1 0]
            \U [0 1]
            \D [0 -1]
            )]
            (for [substep (range 1 (inc distance))
                :let [position [(+ (first position) (* (first dir) substep))
                                (+ (second position) (* (second dir) substep))]]
                :let [step (+ step substep)]]
                [position step]
                ))))

(def raw-trace0 (list-fields [[0 0] 0] path0)) ; trace path resulting in [position step] vector
(def raw-trace1 (list-fields [[0 0] 0] path1))

(def pos-trace0 (set (map first raw-trace0))) ; extract only positions and put it in a set
(def pos-trace1 (set (map first raw-trace1)))

(def pos-intersect (clojure.set/intersection pos-trace0 pos-trace1)) ; use position-sets to find intersections of the traces

(defn first-hit 
    "Given some position in a trace, first-hit will find the earliest occurence (lowest step/time) of it in a trace."
    [position trace]
    (reduce min ; get only the lowest (first) hit
        (for [field trace :when (= position (first field))] (second field))))

(def intersect 
    (sort-by second ; sort by combined time
        (map 
            #(vec [% (+ (first-hit % raw-trace0) (first-hit % raw-trace1))]) ; save position and its combined first-hit-times from both traces
            pos-intersect)))

(first intersect) ; solution to part 2