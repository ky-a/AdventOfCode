;;; Advent of Code Day 1

;; --Imports--
(require '[clojure.string :as str])

;; code to safely parse string to int as found on https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number/9190531
(defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

;; split string on \n and map to integer
(defn parse [raw-data]
    (map parse-int 
        (str/split raw-data #"\n")))

;; fetch data from file and parse it
(def data (parse (slurp "data/day1")))

(print data)

;;; --Part 1---

(defn fuel-for-mass [mass]
    (+ -2
        (Math/floor 
            (/ mass 3))))

(def fuel-required 
    (reduce + 
        (map fuel-for-mass data)))

(print fuel-required)

;;; --Part 2---

(defn fuel-for-mass-recursive [mass]
    (def extra-mass (fuel-for-mass mass)) ; 
    (if (> extra-mass 0)
            (+ extra-mass (fuel-for-mass-recursive extra-mass)) ; recurse for remaining mass
            (0)))

(def fuel-required-p2 
    (reduce + 
        (map fuel-for-fuel data)))

(print fuel-required-p2)