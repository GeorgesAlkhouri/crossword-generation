(ns crossword.core
  (:gen-class)
  (:use clojure.test))

(defrecord Pattern [x y direction length freedom])
(def across true)
(def down false)

(def patterns '())
(def words '())

;;;;;;;Helpers;;;;;;;

(defn get-columns
  "Get a list of columns from the grid. Pares the chars back to strings"
  [grid]
  (let [count (count grid)]
   (map #(apply str %) (partition count (apply interleave grid)))))

(defn format-grid
  "Removes space chars from the grid strings."
  [grid]
  (map #(.replace % " " "") grid))

;source http://stackoverflow.com/questions/3262195/compact-clojure-code-for-regular-expression-matches-and-their-position-in-string
(defn re-pos
  "Finds matching regex and returns them with their respective position"
  [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn map-patterns
  "Maps positions, directions and lengths to patterns."
  [patterns direction]
  (apply concat (map-indexed (fn ;; concat because of nested map, otherwise it would create nested list.
                  [idx itm]
                  (map (fn [p]
                         (let [pos (first p)
                               len (count (second p))]
                           (if (= direction across)
                             (->Pattern idx pos direction len 0)
                             (->Pattern pos idx direction len 0)))) itm))
                patterns)))

;;;;;;;Program;;;;;;;

(defn pick-pattern
  "Pick a pattern for a given filling strategy f."
  [function patterns]
  nil)

(defn create-patterns
  "Determines all patterns from a fresh grid. Only execudes one time after start."
  [grid]
  (let [horizontal grid
        vertical (get-columns grid)
        patterns-h (map #(re-pos #"_+" %) horizontal)
        patterns-v (map #(re-pos #"_+" %) vertical)
        mapped-p-h (map-patterns patterns-h across)
        mapped-p-v (map-patterns patterns-v down)]
    (set (concat mapped-p-h mapped-p-v))))

;; #"_+"

;;;;;;;Test Cases;;;;;;;

(def test-grid-simple ["# # _ _ _"
                       "# _ _ _ _"
                       "_ _ _ _ _"
                       "_ _ _ _ #"
                       "_ _ _ # #"])

(deftest init-patterns
  (is (= (create-patterns (format-grid test-grid-simple)) (set (list (->Pattern 0 2 across 3 0)
                                                                     (->Pattern 1 1 across 4 0)
                                                                     (->Pattern 2 0 across 5 0)
                                                                     (->Pattern 3 0 across 4 0)
                                                                     (->Pattern 4 0 across 3 0)
                                                                     (->Pattern 2 0 down 3 0)
                                                                     (->Pattern 1 1 down 4 0)
                                                                     (->Pattern 0 2 down 5 0)
                                                                     (->Pattern 0 3 down 4 0)
                                                                     (->Pattern 0 4 down 3 0))))))





