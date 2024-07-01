(ns aoc22.day19
  (:require [clojure.string :as str]
            [aoc22.day14 :refer [find-and-parse-ints]]
            [clojure.walk :refer [postwalk]]
            [clojure.math :as math]))


(defn parse-cost [c] 
  (reverse
   (map #(%1 %2)
        [#(Integer/parseInt %) keyword]
        (str/split c #" "))))

(defn parse-robot [r]
  (let [robot-name (keyword (re-find #"\S+(?=\srobot)" r))
        robot-costs  (apply hash-map (mapcat parse-cost (re-seq #"\d+\s\S+" r)))]
    [robot-name robot-costs]))

  (defn parse-blueprint [bp]
    (apply hash-map (mapcat parse-robot (next (str/split bp #"[\.\:]")))))

(def blueprints-verbose
  (map parse-blueprint
       (str/split-lines (slurp "resources/day19"))))

; a blueprint is a vector of recipes for [ore clay obsidian geode] robots
; a recipe is a vector of [ore clay obsidian geode] requirements
(def resource-order [:ore :clay :obsidian :geode])
(def resource-index {:ore 0 :clay 1 :obsidian 2 :geode 3})

(def blueprints
  (mapv (fn [blueprint]
          (mapv (fn [robot]
                  (mapv (fn [resource]
                          (get (get blueprint robot) resource 0))
                        resource-order))
                resource-order)) 
        blueprints-verbose))


(defn is-possible? [resources recipe]
  (empty? (remove
           (fn [[type amount]]
             (<= amount (get resources type 0)))
           recipe)))

(defn possible-actions [blueprint state]
    (filter #(is-possible? (:resources state) (:cost %)) blueprint))

(defn produce-resources [resources robots]
  (merge-with + resources robots))

(defn spend-resources [resources action]
  (merge-with - resources (:cost action)))

(defn xy [blueprint state time]
  (if (< time 0)
    state
    (let [{resources :resources robots :robots} state
          resources' (produce-resources resources robots)
          time' (dec time)]
      (conj (mapv (fn [action]
                    (xy blueprint
                        (assoc state
                               :resources (spend-resources resources' action)
                               :robots (update robots (:robot action) inc))
                        time'))
                  (possible-actions blueprint resources))
            (xy blueprint (assoc state :resources resources') time')))))

(xy (first blueprints) {:robots {:ore 1 :clay 0 :obsidian 0 :geode 0} :resources {}} 16)

(is-possible? {:ore 3 :obsidian 20} {:obsidian 10 :ore 1})

(possible-actions (first blueprints) {:ore 4 :clay 14})

(postwalk (partial eval (first blueprints)) {:robots {:ore 1} :resources {}})

(postwalk #((if (< 0 (:iter %)) () (update % :iter dec))) {:iters 4})

(optimise-bp (first blueprints) 24 {:robots {:ore 1} :resources {} :building nil})

(defn demand [bp]
  (let [[ore clay obsidian geode] bp
        obsidian-demand (nth geode 2) 
        clay-demand (* obsidian-demand (nth obsidian 1))
        ore-demand (+ (* (nth clay 0) clay-demand) (nth ore 0) (nth obsidian 0) (nth geode 0))]
    [ore-demand clay-demand obsidian-demand -1]))

(def bp [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]])

(demand bp)

(map / (demand bp) [1 14 1 1])

ore -> clay -> obs.

(def policy [2 5 3])

(defn filter-indexes [fun col]
  (remove nil? 
          (map-indexed (fn [i el] 
                         (if (fun el) i nil)) 
                       col)))


(defn first-less-than [have want]
  (first (filter-indexes neg? (map - have want))))

(first-less-than [0 1 2] [0 1 0])

(first (filter < ))

(defn is-possible? [resources cost]
  (empty? (filter neg? (map - resources cost))))

(is-possible? [2 1 -1] [2 1 0])

(defn choose-action [policy robots resources bp]
  (let [want-to-build (or (first-less-than robots policy) 3)
        cost (get bp want-to-build)]
    (if (is-possible? resources cost) 
      {:robot want-to-build :cost cost}
      {:robot nil :cost [0 0 0 0]})))

(choose-action [1 0 0 0] [1 0 0 0] [50 50 6 50] bp)

(defn run-policy [policy blueprint]
  (loop [resources [0 0 0 0]
         robots [1 0 0 0]
         time 24] 
    (if (<= time 0)
      [resources robots]
      (let [{:keys [robot cost]} (choose-action policy robots resources blueprint)
            resources' (map + robots (map - resources cost))
            robots' (if (nil? robot) robots (update robots robot inc))]
        (println (- 25 time) robots robot resources)
        (recur resources' robots' (dec time))))))

(run-policy [1 3 3] bp)

(get bp (:geode resource-index))

(loop [supply-demand [0 0 0 0]
       resource :geode]
  (let [index (resource-index resource)
        cost (get bp index)]
    ()
    ))

(:geode {:geode 1} 5)

(defn run-policy' [policy blueprint initial-state initial-time]
  (loop [state initial-state
         time initial-time]
    (let [{resources :resources robots :robots} state
          resources' (produce-resources resources robots)
          action (choose-action policy blueprint robots)]
      (if (< time 0)
        resources'
        (recur (assoc state
                      :resources (spend-resources resources' action)
                      :robots (update robots (:robot action) inc))
               (dec time))))))

;; ---

resource-extraction-thresh = (-)steps-left *  policy * 

thresh = x + steps-left * y ^ z

time-to-robot
time-to-robot threshold at which you stop making 

;; ---
;; Okay, next approach - keep the tree smaller by jumping through time
;; Maybe there's a way to prune that, too?

(defn time-to-build [resources robots costs]
  (let [costs-robots (->> (map vector (map - costs resources) robots)
                          (filter (fn [[c _]] (pos? c))) ; does it cost something?
                          )]
    (if (some #(zero? (second %)) costs-robots) ; no robots?
      nil
      (+ 1 (int (math/ceil (apply max 0 (map #(apply / %) costs-robots))))))))

(time-until-buildable [8 0 8 0] [7 0 0 0] [2 0 0 0])

(defn robot-tree [blueprint resources robots time history]
  (let [ttb-partial (partial time-to-build resources robots)
        build-times (mapv ttb-partial blueprint)
        recipe-times (remove (fn [[_ t _]] (or (nil? t) (neg? (- time t)))) 
                             (map vector blueprint build-times (range)))] 
    (let [subgraph
          (pmap
           (fn [[costs duration robot]]
             (let [resources' (map (fn [re ro co]
                                     (- (+ re
                                           (* ro
                                              duration))
                                        co))
                                   resources robots costs)
                   robots' (update robots robot inc)
                   time' (- time duration)]
               (robot-tree blueprint resources' robots' time' (conj history robot))))
           recipe-times)]
      (if (empty? subgraph)
        history
        subgraph))
    ))

(robot-tree (first blueprints) [0 0 0 0] [1 0 0 0] 20 [])
