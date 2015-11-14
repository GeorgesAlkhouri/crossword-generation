(ns crossword.core
  (:gen-class)
  (:use clojure.test))

(require '[clojure.java.io :as io])

(defrecord Pattern [x y direction length freedom regex])
(def across true)
(def down false)

(def patterns '())
(def words '())

;;;;;;;Util;;;;;;;

(defn read-wordlist
  "Reads the provided word list into a seq."
  []
  (let [path (-> "knuth_words_all_lower_sorted" io/resource io/file)]
    (with-open [rdr (io/reader path)]
      (doall (line-seq rdr))))) ;; doall needed to realize (not lazy) all lines in buffer ))))

(defn hash-wordlist
  ""
  [wordlist]
  (loop [remain wordlist
         result {}]
    (let [key (-> (count (first remain)) str keyword)]
      (if (empty? remain)
        result
        (if (nil? (key result)) ;; length not added yet to map
          (recur (next remain) (assoc result key [(first remain)]));; add new key and new word
          (let [old-list (key result)
                new-list (conj old-list (first remain))]
            (recur (next remain) (assoc result key new-list))) ;; add new word to key
          )))))

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
                             (->Pattern idx pos direction len 0 "")
                             (->Pattern pos idx direction len 0 "")))) itm))
                patterns)))

(defn pattern->regex
  "Creates a regex from a patter and the corresponding grid."
  [pattern grid]
  (letfn [(extract [start end line]
            (let [letters (subs line start (+ start end))]
              letters))
          (parse [letters]
            (-> (apply str (map (fn [c]
                                  (if (= \_ c)
                                    "[a-z]"
                                    (str c))) letters)) re-pattern))]
    (if (= (:direction pattern) across)
      (-> (extract (:y pattern) (:length pattern) (nth grid (:x pattern))) parse)
      (-> (extract (:x pattern) (:length pattern) (nth (get-columns grid) (:y pattern))) parse))))


;;;;;;;Fill Strategies;;;;;;;



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
        mapped-p-v (map-patterns patterns-v down)
        patterns ( map (fn [p]
                         (let [regex (pattern->regex p grid)]
                           (assoc p :regex (str regex)))) (concat mapped-p-h mapped-p-v))]
    (set patterns)))

(defn -main
  [& args]
  (-> (read-wordlist) hash-wordlist))

;;;;;;;Test Cases;;;;;;;

(def test-grid-simple ["# # _ _ _"
                       "# _ _ _ _"
                       "_ _ _ _ _"
                       "_ _ _ _ #"
                       "_ _ _ # #"])

(def test-grid-medium ["# # # _ _ _ # # #"
                       "# # _ _ _ _ _ # #"
                       "# _ _ _ _ _ _ _ #"
                       "_ _ _ _ # _ _ _ _"
                       "_ _ _ # # # _ _ _"
                       "_ _ _ _ # _ _ _ _"
                       "# _ _ _ _ _ _ _ #"
                       "# # _ _ _ _ _ # #"
                       "# # # _ _ _ # # #"])

(def test-grid-all-patterns ["_ _"
                             "_ _"])

(def test-grid-no-patterns ["# # # # #"
                            "# # # # #"])

(def test-grid-no-elements [])

(def test-grid-letters ["# a p _ "])

(deftest test-hash-wordlist
  (is (= (hash-wordlist '()) {}))
  (is (= (hash-wordlist '("a" "ab" "abc" "aa" "bb" "c")) {:1 '("a" "c"), :2 '("ab" "aa" "bb"), :3 '("abc")}))) ;; order will be respected

(deftest create-init-patterns
  (is (= (create-patterns (format-grid test-grid-simple)) (set (list (->Pattern 0 2 across 3 0 "[a-z][a-z][a-z]")
                                                                     (->Pattern 1 1 across 4 0 "[a-z][a-z][a-z][a-z]")
                                                                     (->Pattern 2 0 across 5 0 "[a-z][a-z][a-z][a-z][a-z]")
                                                                     (->Pattern 3 0 across 4 0 "[a-z][a-z][a-z][a-z]")
                                                                     (->Pattern 4 0 across 3 0 "[a-z][a-z][a-z]")
                                                                     (->Pattern 2 0 down 3 0 "[a-z][a-z][a-z]")
                                                                     (->Pattern 1 1 down 4 0 "[a-z][a-z][a-z][a-z]")
                                                                     (->Pattern 0 2 down 5 0 "[a-z][a-z][a-z][a-z][a-z]")
                                                                     (->Pattern 0 3 down 4 0 "[a-z][a-z][a-z][a-z]")
                                                                     (->Pattern 0 4 down 3 0 "[a-z][a-z][a-z]")))))
  (is (= (create-patterns (format-grid test-grid-no-patterns)) (set nil)))
  (is (= (create-patterns (format-grid test-grid-no-elements)) (set nil)))
  (is (= (create-patterns (format-grid test-grid-all-patterns)) (set (list (->Pattern 0 0 across 2 0 "[a-z][a-z]")
                                                                           (->Pattern 1 0 across 2 0 "[a-z][a-z]")
                                                                           (->Pattern 0 0 down 2 0 "[a-z][a-z]")
                                                                           (->Pattern 0 1 down 2 0 "[a-z][a-z]"))))))

(deftest test-pattern->regex
  (is (= (str (pattern->regex (->Pattern 0 2 across 3 0 #"") (format-grid test-grid-simple))) (str  #"[a-z][a-z][a-z]")))
  (is (= (str (pattern->regex (->Pattern 0 0 down 2 0 #"") (format-grid test-grid-all-patterns))) (str #"[a-z][a-z]")))
  (is (= (str (pattern->regex (->Pattern 0 1 across 3 0 #"") (format-grid test-grid-letters))) (str #"ap[a-z]"))))





