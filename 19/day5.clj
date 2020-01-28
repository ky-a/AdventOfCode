;;; Advent of Code Day 5

;; --Imports--
(require '[clojure.string :as str])

;; split string on ',' and map to integer vector
(defn parse [raw-data]
    (vec 
        (map read-string
            (str/split raw-data #","))))

;; fetch data from file and parse it
(def data (parse (slurp "data/day5")))

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
(defn run [data eip in out]
    (let [[opcode p-modes] (read-opcode (nth data eip))]
        (let [[par dst] [#(parameter data eip p-modes %) (destination data eip opcode)]]
            (log eip in opcode dst par #(parameter-string data eip p-modes %))
            (case opcode
                ;; RUN |                  DATA                        |                 EIP                    |        IO     | COMMENT
                1 (run (assoc data dst (+ (par 0) (par 1)))             (+ eip 4)                               in out)           ; set dst = p1 + p2
                2 (run (assoc data dst (* (par 0) (par 1)))             (+ eip 4)                               in out)           ; set dst = p1 * p2
                3 (run (assoc data dst (first in))                      (+ eip 2)                               (rest in) out)          ; set dst = next input
                4 (run data                                             (+ eip 2)                               in (conj out (par 0)))  ; add to output
                5 (run data                                             (if (not= 0 (par 0)) (par 1) (+ eip 3)) in out)                 ; jump to p2 if p1 != 0
                6 (run data                                             (if (= 0 (par 0)) (par 1) (+ eip 3))    in out)                 ; jump to p2 if p1 = 0
                7 (run (assoc data dst (if (< (par 0) (par 1)) 1 0))    (+ eip 4)                               in out)         ; set dst = 1 if p1 < p2 else 0
                8 (run (assoc data dst (if (= (par 0) (par 1)) 1 0))    (+ eip 4)                               in out)         ; set dst = 1 if p1 = p2 else 0
            
                99 (eval out)
            ))))

(run data 0 [1] [])
(run data 0 [5] [])