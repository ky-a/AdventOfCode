;;; Advent of Code Day 1

;; --Imports--
(require '[clojure.string :as str])

;; code to safely parse string to int as found on https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number/9190531
(defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

;; split string on ',' and map to integer vector
(defn parse [raw-data]
    (vec 
        (map parse-int 
            (str/split raw-data #","))))

;; fetch data from file and parse it
(def data (parse (slurp "data/day2")))

(print data)

;;; --Part 1---

;; perform operation op with arguments relative to the eip and return new data vec
(defn operation [data eip op]
    (def val1 (nth data (nth data (+ eip 1))))
    (def val2 (nth data (nth data (+ eip 2))))
    (def index (nth data (+ eip 3)))
    (assoc data index (op val1 val2)))

;; switch opcode and seek through data vec
(defn run [data eip]
    (let [opcode (nth data eip)]
        (case opcode
            1 (run (operation data eip +) (+ eip 4))
            2 (run (operation data eip *) (+ eip 4))
            99 (vec data))
    ))

;; set first two values (noun, verb) of data vec
(defn restore-conditions [data noun verb]
    (assoc (assoc data 1 noun) 2 verb))

(def output (run (restore-conditions data 12 2) 0))
(def solution-p1 (first output))
(println " -> " solution-p1)

;;; --Part 2---

;; iterate through possible values of (noun, verb) and run intcode computer, add only if (nth output 1) = desired output
(defn find-arguments [data desired-output]
    (remove empty?
        (for [noun (range 0 100)]
            (for [verb (range 0 100)
                :let [out (run (restore-conditions data noun verb) 0)]
                :when (= (first out) desired-output)]
                    (eval [noun verb])
                ))))

(find-arguments data 19690720)