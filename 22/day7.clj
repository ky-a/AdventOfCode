(ns aoc22.day7
  (:require [clojure.string :as str]))

(def input (slurp "resources/day7"))

;; cd -> add to or pop last item from state.loc
(defn cd [state params _]
  (let [loc (get state :loc)
        target (first params)]
    (if (= target "..")
      (assoc state :loc (pop loc))
      (assoc state :loc (conj loc target)))))

(defn parse-file [path size name]
  (let [file {:path path :name name}]
    (if (= "dir" size)
      (assoc file :size 0 :isFile false)
      (assoc file :size (Integer/parseInt size) :isFile true))))

(defn read-content [path content]
  (map #(apply parse-file path %) (map #(str/split % #" ") content)))

;; ls -> read and parse list of files & dirs
(defn ls [state _ out]
  (let [contents (read-content (get state :loc) out)]
    (assoc state :fs (apply conj (get state :fs) contents))))

;; nf -> command not found
(defn nf [state _ _]
  (println "err: command"  "not found")
  state)

(def commands {"cd" cd "ls" ls})

(defn apply-instruction [state instruction]
  (let [cmd (get instruction :cmd)
        params (get instruction :params)
        out (get instruction :out)]
    ((get commands cmd nf) state params out)))

(def instructions
  (next
   (map (fn [lines]
          (let [cmd (str/split (first lines) #" ")]
            (hash-map :cmd (first cmd) :params (next cmd) :out (next lines))))
        (map #(str/split % #"\n")
             (str/split input #"\$ ")))))

(def result-state (reduce apply-instruction {:loc '[] :fs []} instructions))

(def fs (get result-state :fs))

(defn is-file? [a] (= true (get a :isFile)))
(defn is-dir? [a] (not (is-file? a)))

(defn get-full-content [fs dir]
  (filter #(= (take (inc (count (get dir :path))) (get % :path))
              (conj (get dir :path) (get dir :name)))
          fs))

(defn dir-size [fs dir]
  (reduce + (map #(get % :size) (filter is-file? (get-full-content fs dir)))))

(def sized-dirs
  (map (fn [entry] (assoc entry :size (dir-size fs entry)))
       (filter is-dir? fs)))

(reduce + (filter #(> 100000 %) (map #(get % :size) sized-dirs)))


;; Part 2

(def total-space 70000000)
(def required-space 30000000)

(def taken-space (reduce + (map #(get % :size) (filter is-file? fs))))

(def free-space (- total-space taken-space))
(def delete-at-least (- required-space free-space))

(def delete-candidates (filter #(> (get % :size) delete-at-least) sized-dirs))

(def smallest-delete (first (sort-by #(get % :size) delete-candidates)))

(get smallest-delete :size)