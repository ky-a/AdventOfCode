;;; Advent of Code Day 1

;; --Puzzle Input--
(def lower-bound 152085)
(def upper-bound 670283)

(defn six-digits [x]
    (= (count (str x)) 6))

(defn two-adjacent-same [x]
    (true? ; 'some' returns 'nil' instead of 'false'
        (some true?
            (map = ; nth a = nth (inc a)?
                (str x) 
                (concat [-1] (str x)) ; add -1 to beginning of vector to 'shift' it right
                ))))

(defn digits-dont-decrease [x]
    (nil? ; ~ not some true
        (some true?
            (map < ; nth a < nth (inc a)?
                (map int (str x))
                (concat [-1] (map int (str x))) ; shift vec to right
                ))))


(def options 
    (for [x (range lower-bound upper-bound)
            :when (six-digits x)
            :when (two-adjacent-same x)
            :when (digits-dont-decrease x)]
            [x]))

(count options) ; solution part 1

;; ---Part 2---

(defn two-adjacent-same [x]
    (true? (some #(= 2 %) ; some combo of two in a row?
        (let [digits (str x)]
            (loop  [index 1 ; start at 1 and..
                    last (first digits) ; initialize 'last' at first
                    combos [1]] ; track lenghts of = sequences
                (if (= i (count digits))
                combos
                    (recur 
                        (inc index) 
                        (nth digits index) 
                        (if (= last (nth digits index)) ; if current = last..
                            (assoc combos (dec (count combos)) (inc (peek combos))) ; inc last element in 'combos' vector
                            (conj combos 1) ; else start a new count starting from 1
                        )
                    )
                ))))))

(def options-p2
    (for [option options ; use p1's options - no removed constrained
            :when (two-adjacent-same option)] 
        [option]))

(count options-p2) ; solution part 2