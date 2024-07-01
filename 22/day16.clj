(ns aoc22.day16
  (:require [clojure.string :as str]
            [aoc22.day14 :refer [find-and-parse-ints]]
            [clojure.math.combinatorics :as combo]))

(def input (str/split-lines (slurp "resources/day16")))

(def valves 
  (into {}
         (map (fn [v]
                (let [[valve & tunnels] (map keyword (re-seq #"(?<=\s)[A-Z]{2}" v))
                      flow-rate (first (find-and-parse-ints v))]
                  [valve {:flow-rate flow-rate :tunnels tunnels}]))
              input)))

; open {:tunnel [path]}
; 1. target in frontier? return it!
; 2. expand frontier
; 3. call bfs on new frontier
(defn breadth-first-path
  ([valves valve target] 
   (breadth-first-path valves {valve []} #{} target))
  ([valves open closed target]
   (if (contains? open target)
     (conj (get open target) target)
     (let [new-open (into {} (mapcat (fn [[valve-key path]]
                                       (map (fn [t] [t (conj path valve-key)])
                                            (:tunnels (get valves valve-key))))
                                     open))
           new-closed (apply conj closed (keys open))
           unique-open (apply dissoc new-open (filter #(contains? new-closed %) (keys new-open)))]
       (breadth-first-path valves unique-open new-closed target))
     )))

(def valves-of-interest 
  (into {} (filter #(< 0 (:flow-rate (second %))) valves)))

(def valve-network
  (into {} (map 
            (fn [vk]
              [vk (into {}
                        (map (fn [ok]
                               [ok (dec (count (breadth-first-path valves vk ok)))])
                             (filter #(not= vk %) (keys valves-of-interest))))])
            (conj (keys valves-of-interest) :AA))))

(defn step-cost [from to]
  (get (get valve-network from) to))

; value-of-path assumes that we've __just__ opened a and now move down path
(defn value-of-path [[a & bc] remaining-minutes] 
  (+ (* (:flow-rate (get valves a) 0) remaining-minutes)
     (if (empty? bc)
       0
       (value-of-path bc (- remaining-minutes (step-cost a (first bc)) 1)))))

(defn recursive-find-best-path [start open minutes]
  (if (<= minutes 0)
    [start] 
    (reduce (fn [top-path next-valve]
              (let [next-path (apply conj [start]
                                     (recursive-find-best-path next-valve
                                                               (remove #(= next-valve %) open)
                                                               (- minutes (step-cost start next-valve) 1)))]
                (if (> (value-of-path next-path minutes)
                       (value-of-path top-path minutes))
                  next-path
                  top-path)))
            []
            open)))

(def best-path 
  (recursive-find-best-path :AA (keys valves-of-interest) 30))

(value-of-path best-path 30)


; Part 2

; states: {:loc :min}
; path: {:path [] :min :value}
; value-of-path assumes that we've __just__ opened a and now move down path
(defn value-of-paths [paths]
  (apply + (map (fn [path] 
                  (:value path 
                          (value-of-path (:valves path) 
                                         (:min path))))
                paths)))

(defn in? [col el] (some #(= % el) col))

(defn recursive-find-best-paths [paths open]
  (let [agents (group-by #(> (:min %) 2) paths)
        active-agents (get agents true)
        passive-agents (get agents false)]
    (if (< (count (:path (first paths))) 4) (println active-agents) nil)
    (if (empty? active-agents)
      passive-agents
      (apply conj passive-agents
             (reduce (fn [top-paths next-valve-assignment]
                       (let [next-paths 
                             (recursive-find-best-paths (map-indexed
                                                         (fn [index agent]
                                                           (let [next-valve (nth next-valve-assignment index :AA)
                                                                 prev-valve (last (:path agent))
                                                                 remain-min (max 0 (- (:min agent) (step-cost prev-valve next-valve) 1))]
                                                             (assoc agent
                                                                    :path (conj (:path agent) next-valve)
                                                                    :min remain-min
                                                                    :value (+ (:value agent 0) (value-of-path [next-valve] remain-min)))))
                                                         active-agents)
                                                        (remove #(in? next-valve-assignment %) open))]
                         (if (> (value-of-paths next-paths)
                                (value-of-paths top-paths))
                           next-paths
                           top-paths)))
                     []
                     (combo/permuted-combinations open (count active-agents)))))))

(recursive-find-best-paths [{:path [:AA] :min 26} {:path [:AA] :min 26}]
                           (keys valves-of-interest))