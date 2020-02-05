;;; Advent of Code Day 7

;; --Imports--
(require '[clojure.string :as str])

;; Permutations function from https://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
;; Also available in [clojure.math.combinatorics] but need a project setup to include dependencies with leiningen.
(defn permutations [colls]
    (if (= 1 (count colls))
      (list colls)
      (for [head colls
            tail (permutations (disj (set colls) head))]
        (cons head tail))))

;; split string on ',' and map to integer vector
(defn parse [raw-data]
    (vec 
        (map read-string
            (str/split raw-data #","))))

;; fetch data from file and parse it
(def data (parse (slurp "data/day7")))

(print data)

;;; --Part 1 & 2---


;; separate opcode from parameter modes
(defn read-opcode [opcode]
    (let [digits (reverse (map #(- (int %) 48) (str opcode)))]
        (case (count digits)
            1 [opcode [0 0 1]]
            2 [opcode [0 0 1]]
            3 [(+ (nth digits 0) (* (nth digits 1) 10)) [(nth digits 2) 0 1]]
            4 [(+ (nth digits 0) (* (nth digits 1) 10)) [(nth digits 2) (nth digits 3) 1]]
            5 [(+ (nth digits 0) (* (nth digits 1) 10)) [(nth digits 2) (nth digits 3) 1]]
            )))

;; get destination (p3) to save results or input to
(defn destination [data eip opcode]
    (case opcode
        (1 2 7 8) (nth data (+ eip 3))
        3 (nth data (+ eip 1))
        -1
    ))

;; get parameter with respect to parameter mode. default: 0 = 'position' 1 = 'immediate'
(defn parameter [data eip p-modes index]
    (if (= (nth p-modes index) 0)
        (nth data (nth data (+ eip (inc index))))
                  (nth data (+ eip (inc index)))
    ))

;; get parameter as string. if parameter mode is 'position' return address and value.
(defn parameter-string [data eip p-modes index]
    (def p-val (nth data (+ eip (inc index))))
    (if (= (nth p-modes index) 0)
        (format "@%d (%d)" p-val (nth data p-val))
        (format "%d" p-val)
    ))

;; print diagnostics and reasoning for every intruction
(defn log [eip inputs opcode dst p ps]
    (println (format "%03d |" eip) 
        (case opcode
            1 (format "@%03d = %d = %s + %s" dst (+ (p 0) (p 1)) (ps 0) (ps 1))
            2 (format "@%03d = %d = %s * %s" dst (* (p 0) (p 1)) (ps 0) (ps 1))
            3 (format "@%03d = @io (%d)" dst (first inputs))
            4 (p 0)
            5 (format "@eip = %s? %s != 0? %s" (ps 1) (ps 0) (if (not= 0 (p 0)) "yes" "no"))
            6 (format "@eip = %s? %s == 0? %s" (ps 1) (ps 0) (if (= 0 (p 0)) "yes" "no"))
            7 (format "@%03d = %d = %s  < %s" dst (if (< (p 0) (p 1)) 1 0) (ps 0) (ps 1))
            8 (format "@%03d = %d = %s == %s" dst (if (= (p 0) (p 1)) 1 0) (ps 0) (ps 1))
            99 "halt."
            (format "ERR op: %d" opcode)
        )))

;; analyse and switch opcode and seek through data vec
(defn run [data eip in out status]
    "returns state of machine after it has haltet or run out of inputs."
    (let [[opcode p-modes] (read-opcode (nth data eip))]
        (let [[par dst] [#(parameter data eip p-modes %) (destination data eip opcode)]]
            (log eip in opcode dst par #(parameter-string data eip p-modes %))
            (if (and (= opcode 3) (empty? in))
                [data eip in out "awaiting"]
                (case opcode
                    ;; RUN |                  DATA                        |                 EIP                    |        IO     | COMMENT
                    1 (run (assoc data dst (+ (par 0) (par 1)))             (+ eip 4)                               in out "running")           ; set dst = p1 + p2
                    2 (run (assoc data dst (* (par 0) (par 1)))             (+ eip 4)                               in out "running")           ; set dst = p1 * p2
                    3 (run (assoc data dst (first in))                      (+ eip 2)                               (vec (rest in)) out "reading")          ; set dst = next input
                    4 (run data                                             (+ eip 2)                               in (conj out (par 0)) "writing")  ; add to output
                    5 (run data                                             (if (not= 0 (par 0)) (par 1) (+ eip 3)) in out "running")                 ; jump to p2 if p1 != 0
                    6 (run data                                             (if (= 0 (par 0)) (par 1) (+ eip 3))    in out "running")                 ; jump to p2 if p1 = 0
                    7 (run (assoc data dst (if (< (par 0) (par 1)) 1 0))    (+ eip 4)                               in out "running")         ; set dst = 1 if p1 < p2 else 0
                    8 (run (assoc data dst (if (= (par 0) (par 1)) 1 0))    (+ eip 4)                               in out "running")         ; set dst = 1 if p1 = p2 else 0
                
                    99 [data eip in out "halted"]
                )))))

;; helper functions to retrive information from a 'machine' vector
(def m-in #(nth % 2))
(def m-out #(nth % 3))
(def m-eip #(nth % 1))
(def m-status #(last %))

(defn status [machine]
    "print the status of a machine [data eip input output status]."
    (println "Machine" (m-status machine) "at" (m-eip machine))
    (println "in:" (m-in machine) "out:" (m-out machine))
    (eval machine))

(defn spawn 
    "create new machine, optinally already provide first input data. "
    ([data] (spawn data [])) 
    ([data input] [data 0 input [] "reset"]))

(defn start 
    "run a machine thas is reset or awaiting input. extra input is appended to the machines internal input vector."
    ([machine] (start machine []))
    ([machine input]
        (let [[data eip in out state] machine]
            (status ; print status after run
                (apply run 
                    (status [data eip (vec (apply conj in input)) [] "starting"]) ; print status before run
)))))

(defn execute-machine-sequence [machines input output-states]
    (if (empty? machines)
        [input output-states] ; return final output and all new machine states
        (let [output-machine (start (first machines) input)] ; run a machine and save the output
            (execute-machine-sequence (rest machines) (m-out output-machine) (conj output-states output-machine))))) ; recur, using output as input and saving the new machine

(defn build-machine-sequence [data inputs]
    "initialize n machines with the same data but different first inputs."
    (map 
        #(spawn data %)
        inputs))

(def all-possible-machine-sequences
    (map 
        #(vec [
            (first ; save just the output of- and not the machines itself
                (execute-machine-sequence ; run after..
                    (build-machine-sequence data %)  ; creating a machine sequence from one of the input permutations
                    [0] []))
            (flatten %)]) ; save the permutation next to the output
        (permutations [[0] [1] [2] [3] [4]]) 
))

(last (sort-by first all-possible-machine-sequences)) ; solution part 1

(defn feedback-loop-machine-sequence [sequence first-input]
    "execute machine sequences recursively, using the last output as the first input"
    (loop [machines sequence
           input first-input]
        (if (= "halted" (m-status (last machines)))
            [input machines] ; return if last machine in sequence is 'halted' instead of 'awaiting input' or 'reset'
            (let [[output states] (execute-machine-sequence machines input [])] ; run machine sequence and dissect output
                (recur states output)
))))

(def all-possible-looped-machine-sequences
    (map 
        #(vec [
            (first ; save just the output of the machines
                (feedback-loop-machine-sequence ; run as feedback loop after..
                    (build-machine-sequence data %) ; creating a machine sequence from one of the input permutations
                    [0]))
            (flatten %)]) ; save the permutation next to the output
        (permutations [[5] [6] [7] [8] [9]])))

(last (sort-by first all-possible-looped-machine-sequences)) ; solution part 2 - this might take a while