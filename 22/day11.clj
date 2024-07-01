(ns aoc22.day11
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [aoc22.day9 :as day9]))

(def input (next (str/split (slurp "resources/day11") #"Monkey ")))

(defn find-int [s] (Integer/parseInt (re-find #"\d+" s)))

(defn parse-monkey [input]
  (let [lines (str/split input #"\n")
        op-val-raw (re-find #"(?<=[+*]\s)\S+" (nth lines 2))
        op-val (biginteger (if (= "old" op-val-raw) 2 (Integer/parseInt op-val-raw)))
        op-raw (re-find #"\+|\*" (nth lines 2))
        op (cond
             (= "old" op-val-raw) (fn [x v] (.multiply x x))
             (= "+" op-raw) (fn [x v] (.add x v))
             (= "*" op-raw) (fn [x v] (.multiply x v)))]
    {:i (Integer/parseInt (re-find #"\d+(?=:)" (nth lines 0)))
     :items (mapv #(biginteger (Integer/parseInt (str %))) (re-seq #"\d+" (nth lines 1)))
     :op (vector op op-val)
     :test-div-by (biginteger (find-int (nth lines 3)))
     :true-to (find-int (nth lines 4))
     :false-to (find-int (nth lines 5))
     :thrown []}))

(defn exec-monkey [monkey]
  (let [op (first (get monkey :op))
        va (second (get monkey :op))
        items (get monkey :items)
        div-by (get monkey :test-div-by)]
    (mapv (fn [item-worry]
            (vector
             (get monkey (if (.equals (biginteger 0) (.mod item-worry div-by)) :true-to :false-to))
             item-worry))
          (map #(.divide % (biginteger 3)) (map #(op % va) items)))))

(defn exec-receive [monkey item]
  (assoc monkey :items (conj (:items monkey) item)))

(defn exec-throw [monkeys throw]
  (let [target (get monkeys (first throw))]
    (assoc monkeys (first throw) (exec-receive target (second throw)))))

(defn exec-next-monkey [monkeys i]
  (let [monkey (nth monkeys i)
        monkey-throws (exec-monkey monkey)
        n-monkeys (assoc monkeys i (assoc monkey :items '[] :thrown monkey-throws))]
    (comment (println i "has" (:items monkey) "throws" monkey-throws))
    (reduce exec-throw n-monkeys monkey-throws)))

(defn play-round [monkeys]
  (reduce exec-next-monkey monkeys (range (count monkeys))))

(def monkeys (mapv parse-monkey input))

(def monkey-history (take 21 (iterate play-round monkeys)))

(def monkey-throw-counts
  (reduce (fn [c monkeys]
            (day9/add-vec c (mapv #(count (:thrown %)) monkeys)))
          (vec (repeat (count monkeys) 0))
          monkey-history))

(apply * (take-last 2 (sort monkey-throw-counts)))

;; Part 2

(def divisors (map #(:test-div-by %) monkeys))
(def modulo-repeats-at (biginteger (reduce #(.multiply %1 %2) divisors)))

(defn exec-monkey-big [monkey]
  (let [op (first (get monkey :op))
        va (second (get monkey :op))
        items (get monkey :items)
        div-by (get monkey :test-div-by)]
    (mapv (fn [item-worry] 
            (vector
             (get monkey (if (.equals (biginteger 0) (.mod item-worry div-by)) :true-to :false-to))
             item-worry))
          (map #(.mod % modulo-repeats-at) 
               (map (fn [item] (op item va)) items)))))

(defn exec-next-monkey-big [monkeys i]
  (let [monkey (nth monkeys i)
        monkey-throws (exec-monkey-big monkey)
        n-monkeys (assoc monkeys i (assoc monkey :items '[] :thrown monkey-throws))]
    (reduce exec-throw n-monkeys monkey-throws)))

(defn play-round-big [monkeys]
  (reduce exec-next-monkey-big monkeys (range (count monkeys))))

(def monkey-history-big (take 10001 (iterate play-round-big monkeys)))

(def monkey-throw-counts-big
  (reduce (fn [c monkeys]
            (day9/add-vec c (mapv #(count (:thrown %)) monkeys)))
          (vec (repeat (count monkeys) 0))
          monkey-history-big))

(apply * (take-last 2 (sort monkey-throw-counts-big)))