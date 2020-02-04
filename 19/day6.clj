;;; Advent of Code Day 6

;; --Imports--
(require '[clojure.string :as str])

;; split string on ',' and map to integer vector
(defn parse [raw-data]
    (vec 
        (map #(str/split % #"\)")
            (str/split raw-data #"\n"))))

;; fetch data from file and parse it
(def data (parse (slurp "data/day6")))

(print data)

;;; --Part 1---

;; add an orbit to the orbits hashmap
(defn integrate [orbits spec]
    (assoc 
        orbits 
        (first spec) ; the object around which is orbited
        (concat 
            (get orbits (first spec) []) ; already existing objects orbiting around that object
            [(second spec)]              ; the object which orbits around
        )
    )
)

(def orbits (reduce integrate {} data)) ; apply integrate function on all data to build 'orbits' hashmap

(defn count-edges [COM orbits depth]
    (reduce 
        +
        depth ; start with COM's depth (number of COM's total orbits)
        (map  ; add total numbmer of orbits below COM
            #(count-edges % orbits (inc depth)) ; recurse, increasing the depth
            (get orbits COM []) ; objects that orbit COM
        )
    )
)

(count-edges "COM" orbits 0) ; solution part 1

;;; ---Part 2---

(defn find-wider-orbit [orbiting-object orbits]
    (first (first ; extract from nested vec
        (filter 
            (fn [orb] 
                (some #(= orbiting-object %) (val orb))) ; find orbiting-object in the values of an orbit
            orbits))))

(defn lower-path [position destination path orbits]
    (flatten
        (if (= position destination)
            (conj path destination) ; path found, stop recursing
            (map 
                #(lower-path % destination (conj path position) orbits)
                (get orbits position [])
            )
        )
    )
)

(defn find-path [position destination level orbits]
    (def lower (lower-path position destination [] orbits)) ; try to find destination lower in the graph
    (if (empty? lower)
        (concat [position] (find-path (find-wider-orbit position orbits) destination (inc level) orbits)) ; step one up if failed
        lower ; return if succeeded 
    )
)

(def path (find-path "YOU" "SAN" 0 orbits))

(- (count path) 3) ; solution part 2 (- 3 because we count transfers and disregard YOU and SAN node)