;;; Advent of Code Day 2
(ns aoc22.day2
  (:require [clojure.string :as str]))

(def input (slurp "resources/day2"))
(def data (map #(str/split % #" ") (str/split input #"\n")))

(def item-score
  {:rock 1
   :paper 2
   :scissors 3})

;; winstates dict. FIRST beats SECOND
(def win-states (set  '((:rock :scissors)
                        (:paper :rock)
                        (:scissors :paper))))

;; Part 1

(def response-item
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(def opponent-item
  {"A" :rock
   "B" :paper
   "C" :scissors})

;; Parse Data as Opponent-Item : Response-Item
(def d2p1-data
  (map #(hash-map
         :ri (get response-item (second %))
         :oi (get opponent-item (first %)))
       data))

(defn game-score [player-item opponent-item]
  (+ (get item-score player-item)
     (cond
       (= player-item opponent-item) 3
       (contains? win-states (list player-item opponent-item)) 6
       :else 0)))

(reduce + (map #(game-score (get % :ri) (get % :oi)) d2p1-data))

;; Part 2

(def response-strategy
  {"X" :lose
   "Y" :draw
   "Z" :win})

;; Parse Data as Opponent-Item : Response-Strategy
(def d2p2-data (map #(hash-map
                  :oi (get opponent-item (first %))
                  :rs (get response-strategy (second %)))
                    data))

(first d2p2-data)

(defn choose-item [opponent-item response-strategy]
  (case response-strategy
    :draw opponent-item
    :win (first (first (filter #(= opponent-item (second %)) win-states)))
    :lose (second (first (filter #(= opponent-item (first %)) win-states)))))

(map
 #(game-score (choose-item (get % :oi) (get % :rs)) (get % :oi))
 d2p2-data)

(reduce + (map 
  #(game-score (choose-item (get % :oi) (get % :rs)) (get % :oi)) 
  d2p2-data))

(choose-item :scissors :win)