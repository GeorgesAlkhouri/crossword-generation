(ns crossword.core
  (:gen-class)
  (:use clojure.test))
(require 'clojure.tools.trace)


(require '[clojure.java.io :as io])

(defrecord Pattern [x y direction length freedom regex word])

(def across true)
(def down false)

(def test-grid-simple ["# # _ _ _"
                       "# _ _ _ _"
                       "_ _ _ _ _"
                       "_ _ _ _ #"
                       "_ _ _ # #"])

(def test-grid-easy ["_ _ #"
                     "_ _ _"
                     "_ _ _"])

(def test-grid-medium ["# # # _ _ _ # # #"
                       "# # _ _ _ _ _ # #"
                       "# _ _ _ _ _ _ _ #"
                       "_ _ _ _ # _ _ _ _"
                       "_ _ _ # # # _ _ _"
                       "_ _ _ _ # _ _ _ _"
                       "# _ _ _ _ _ _ _ #"
                       "# # _ _ _ _ _ # #"
                       "# # # _ _ _ # # #"])

(def test-grid-hard ["_ _ _ _ # _ _ _ # _ _ _ _"
                     "_ _ _ _ # _ _ _ # _ _ _ _"
                     "_ _ _ _ # _ _ _ # _ _ _ _"
                     "_ _ _ _ _ _ # _ _ _ _ _ _"
                     "# # # _ _ _ # _ _ _ # # #"
                     "_ _ _ _ _ # # # _ _ _ _ _"
                     "_ _ _ # # # # # # # _ _ _"
                     "_ _ _ _ _ # # # _ _ _ _ _"
                     "# # # _ _ _ # _ _ _ # # #"
                     "_ _ _ _ _ _ # _ _ _ _ _ _"
                     "_ _ _ _ # _ _ _ # _ _ _ _"
                     "_ _ _ _ # _ _ _ # _ _ _ _"
                     "_ _ _ _ # _ _ _ # _ _ _ _"])


;;;;;;;Util

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
  "Get a list of columns from the grid."
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
                             (->Pattern idx pos direction len 0 "" "")
                             (->Pattern pos idx direction len 0 "" "")))) itm))
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

(defn pattern->rect [pattern]
  (let [left (:y pattern)
        top (:x pattern)
        right (if (= (:direction pattern) across)
                (- (+ left (:length pattern)) 1)
                left)
        bottom (if (= (:direction pattern) down)
                 (- (+ top (:length pattern)) 1)
                 top)]
    {:left left :top top :right right :bottom bottom}))

(defn get-affected-patterns
  [patterns pattern]
  (let [a (pattern->rect pattern)]
    (filter #(let [b (pattern->rect %)]
               (and
                (not (or (> (:left b) (:right a))
                         (< (:right b) (:left a))
                         (> (:top b) (:bottom a))
                         (< (:bottom b) (:top a))))
                (not= a b))) patterns) ))

(defn match-words
  [regex words]
  (filter #(re-matches regex %) words))

(defn words-with-length
  [length wordlist]
  (-> (str length) keyword wordlist))

(defn update-freedom
  [fill-strategy patterns wordlist]
  (map (fn [p]
         (let [regex (re-pattern (:regex p))
               words (-> (str (:length p)) keyword wordlist)]
           (assoc p :freedom (fill-strategy regex words)))) patterns))

(defn replace-string
  "Insert c in string s at index i."
  [s c i]
  ;;(println s c i)
  (str (subs s 0 i) c (subs s (+ i 1))))

;; (defn replace-string
;;   ([target new offset] (replace-string
;;                         target new offset (count new)))
;;   ([target new offset length]
;;    (println target new offset length)
;;    (str (subs target 0 offset) new (subs target (+ offset length)))) )

(defn update-regex
  [pattern affected-patterns]
  (let [temp-regex-pattern (clojure.string/replace (:regex pattern)  #"\[a\-z\]" "*") ]
    (map #(let [temp-regex (clojure.string/replace (:regex %)  #"\[a\-z\]" "*")
                updated-regex (let []
                               ;;(println "[ " pattern " ]")
                               ;;(println " [[" % "]]")
                               ;;(println "p1" temp-regex-pattern "x:"  (- (:x %) (:x pattern))  "y:"  (- (:y %) (:y pattern)))
                               ;;(println "p2" temp-regex "x:"  (- (:y pattern) (:y %)) "y: "  (- (:x pattern) (:x %)))
                                
                                (if (= (:direction pattern) across)
                                  (replace-string temp-regex (str (nth temp-regex-pattern (- (:y %) (:y pattern)))) (- (:x pattern) (:x %)))
                                  (replace-string temp-regex (str (nth temp-regex-pattern (- (:x %) (:x pattern)))) (- (:y pattern) (:y %)) )))]
            ;;(println updated-regex)
            (assoc % :regex (clojure.string/replace updated-regex #"\*" "[a-z]"))) affected-patterns)))

(defn pattern-equal?
  "Compares patterns without considering regex and freedom."
  [a b]
  (and (= (:x a) (:x b))
       (= (:y a) (:y b))
       (= (:direction a) (:direction b))
       (= (:length a) (:length b))))

(defn pattern-replace
  [patterns replace]
   (map #(if (pattern-equal? replace %)
           replace
           %) patterns))

(defn update-patterns
  [patterns replacements]
  (loop [p patterns
         r replacements]
    (if (empty? r)
      p
      (recur (pattern-replace p (first r))
             (next r)))))

(defn patterns-into-grid
  [patterns grid]
  (let [cols (get-columns grid)
        rows grid]
    (map #(let [coll (if (= (:direction %) across) rows cols)
                el (if (= (:direction %) across) (:x %) (:y %))
                pos (if (= (:direction %) across)(:y %) (:x %))
                regex (clojure.string/replace (:regex %)  #"\[a\-z\]" "*")]
            (map-indexed (fn [i c]
                           (when (>= i (- (count coll) (:length %)))
                             (let [c' (nth regex i)]
                               (if (re-matches #"[a-z]" c')
                                 (replace-string (nth coll el) c' )
                                 c)))) (nth coll el))) patterns)))

;;;;;;;Fill Strategies;;;;;;;

(defn most-constrained
  [regex words]
  (count (match-words regex words)))


;;;;;;;Pick Strategies;;;;;;;



;;;;;;;Program;;;;;;;

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

(defn fill-pattern
  "Fill a pattern for a given fill/delete strategy f. Chooses the most constrained pattern. "
  [fill-strategy patterns wordlist]
  (let [rated (update-freedom fill-strategy patterns wordlist)
        most-constrained (apply min-key #(:freedom %) rated)] ;;dirty! change it, so that you can abstract from most-constraint to any pick strat.
    most-constrained))

(defn pick-words ;;dirty! use pick strategy abstraction, now only first n strategy is used
  "Picks possible words with a picking strategy." 
  [pick-strategy pattern wordlist]
  (let [words (words-with-length (:length pattern) wordlist)
        matching-words (take 10 (match-words (re-pattern (:regex pattern)) words))]
    matching-words))

(defn determine-best-word
  "Instantiates each word into patterns
  to calculate and build product of each new freedom values.
  Uses highest freedom value."
  [affected-patterns pattern best-words wordlist]
  (if (empty? best-words)
    '()
    (let [freedom-product  (map #(let [p' (assoc pattern :regex %)
                                            updated-patterns (update-regex p' affected-patterns)
                                            p'' (update-freedom most-constrained updated-patterns wordlist) ;; dirty! 
                                            freedom (reduce * (map :freedom p''))]
                                        {:p p'' :w % :f freedom}) best-words)]
      (reverse (sort-by :f  freedom-product))  ;; (apply max-key (fn [m] (:f m)) freedom-prdouct)
      )))

;; 1. Insert word w
;; 2. Recalculate possibilites of inserting a word for each affected crossworing for w and build PRODUCT
;; 3. Use the word that maximizes this PRODUCT

(defn solve
  [patterns wordlist solved]
  (if (empty? patterns)
    (let []
      (println solved)
      true)
    (let [next-pattern (fill-pattern most-constrained patterns wordlist) ;; 1. Delete tuple from list with lowest freedo,
          affected-patterns (get-affected-patterns patterns next-pattern)
          possible-words (pick-words nil next-pattern wordlist) ;; 2. Instantiate pattern to grid
          best-w-p (determine-best-word affected-patterns next-pattern possible-words wordlist)] ;; 3.-4. Propagate instantiation to affected patterns and check for arc-consistent
      (if (empty? best-w-p)
        false
        (loop [rest best-w-p]          
          (let [instance (-> rest first)]
            (when (not= 0 (:f instance))
              (let [w (:w instance)
                    ps (:p instance)
                    u (update-patterns patterns ps)]
                ;;(println instance)
                ;;(println (type instance))
                (if (solve
                     (remove #(pattern-equal? next-pattern %) u)
                     (assoc wordlist
                       (-> (count w) str keyword)
                       (remove #(= w %) (words-with-length (count w) wordlist)))
                     (cons (assoc next-pattern :word w) solved))
                  true
                  (when (-> (next rest) empty? not)
                    (recur (next rest))))))))))))

(defn -main
  [& args]
  (let [wordlist (-> (read-wordlist) hash-wordlist) ;;{:2 '("an") :3 '("dad" "and" "dup" "too" "fog" "dog")} ;;(-> (read-wordlist) hash-wordlist)
        grid (format-grid test-grid-hard)
        patterns (create-patterns grid)]
    (solve patterns wordlist #{})))

;;;;;;;Test Cases;;;;;;;

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
  (is (= (create-patterns (format-grid test-grid-simple)) (set (list (->Pattern 0 2 across 3 0 "[a-z][a-z][a-z]" "")
                                                                     (->Pattern 1 1 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 2 0 across 5 0 "[a-z][a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 3 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 4 0 across 3 0 "[a-z][a-z][a-z]" "")
                                                                     (->Pattern 2 0 down 3 0 "[a-z][a-z][a-z]" "")
                                                                     (->Pattern 1 1 down 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 0 2 down 5 0 "[a-z][a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 0 3 down 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 0 4 down 3 0 "[a-z][a-z][a-z]" "")))))
  (is (= (create-patterns (format-grid test-grid-no-patterns)) (set nil)))
  (is (= (create-patterns (format-grid test-grid-no-elements)) (set nil)))
  (is (= (create-patterns (format-grid test-grid-all-patterns)) (set (list (->Pattern 0 0 across 2 0 "[a-z][a-z]" "")
                                                                           (->Pattern 1 0 across 2 0 "[a-z][a-z]" "")
                                                                           (->Pattern 0 0 down 2 0 "[a-z][a-z]" "")
                                                                           (->Pattern 0 1 down 2 0 "[a-z][a-z]" ""))))))

(deftest test-pattern->regex
  (is (= (str (pattern->regex (->Pattern 0 2 across 3 0 "" "") (format-grid test-grid-simple))) (str  #"[a-z][a-z][a-z]")))
  (is (= (str (pattern->regex (->Pattern 0 0 down 2 0 "" "") (format-grid test-grid-all-patterns))) (str #"[a-z][a-z]")))
  (is (= (str (pattern->regex (->Pattern 0 1 across 3 0 "" "") (format-grid test-grid-letters))) (str #"ap[a-z]"))))

(deftest test-most-constrained ;; {:2 '("at" "on") :3 ("the" "cat" "dog")}
  (is (= (most-constrained #"[a-z][a-z][a-z]" '("the" "cat" "dog")) 3))
  (is (= (most-constrained #"[a-z]o[a-z]" '("the" "cow" "dog")) 2))
  (is (= (most-constrained #"fo[a-z]" '("the" "cow" "dog")) 0)))

(deftest test-fill-pattern
  (is (= (fill-pattern most-constrained (list (->Pattern 0 3 across 3 0 "[a-z][a-z][a-z]" "")
                                              (->Pattern 0 2 across 3 0 "[a-z]o[a-z]" "")) {:3 '("cat" "dog" "cow")}) (->Pattern 0 2 across 3 2 "[a-z]o[a-z]" "")))
  (is (= (fill-pattern most-constrained (list (->Pattern 0 3 across 3 0 "[a-z][a-z][a-z]" "")
                                              (->Pattern 0 2 across 3 0 "[a-z]ot" "")) {:3 '("cat" "dog" "cow")}) (->Pattern 0 2 across 3 0 "[a-z]ot" "")))
  (is (= (fill-pattern most-constrained (list (->Pattern 0 3 across 3 0 "[a-z][a-z][a-z]" "")
                                              (->Pattern 0 2 across 3 0 "[a-z]o[a-z]" "")) {:3 '("cot" "dog" "cow")}) (->Pattern 0 2 across 3 3 "[a-z]o[a-z]" ""))))

(deftest test-pick-word
  (is (= (pick-words nil (->Pattern 0 0 across 5 0 "[a-z][a-z]a[a-z]t" "") {:1 '("a" "b") :5 '("quart" "cluby" "testi" "start")})
         '("quart" "start")))
  (is (= (pick-words nil (->Pattern 0 0 across 3 0 "[a-z]at" "") {:3 '("dog" "ape" "cot")})
         '()))) ;; first-n

(def test-affected-pattern-grid-across ["_ _ # _"
                                        "_ _ _ _"])

(def test-affected-pattern-grid-down ["_ _ _ _"
                                      "_ _ _ _"
                                      "# # _ _"
                                      "_ _ _ _"])

(deftest test-get-affected-patterns
  (is (= (get-affected-patterns (create-patterns (format-grid test-affected-pattern-grid-across)) (->Pattern 0 0 across 2 0 "" ""))
         (list (->Pattern 0 0 down 2 0 "[a-z][a-z]" "")
               (->Pattern 0 1 down 2 0 "[a-z][a-z]" "") ))
      (= (get-affected-patterns (create-patterns (format-grid test-affected-pattern-grid-down)) (->Pattern 0 1 down 2 0 "" ""))
         (list (->Pattern 0 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")
               (->Pattern 1 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")))))

(deftest test-update-regex
  (is (= (update-regex (->Pattern 0 0 across 3 0 "cat" "") (list (->Pattern 0 0 down 3 0 "[a-z][a-z][a-z]" "")
                                                              (->Pattern 0 1 down 3 0 "[a-z][a-z][a-z]" "")
                                                              (->Pattern 0 2 down 3 0 "[a-z][a-z][a-z]" "")))
         (list (->Pattern 0 0 down 3 0 "c[a-z][a-z]" "")
               (->Pattern 0 1 down 3 0 "a[a-z][a-z]" "")
               (->Pattern 0 2 down 3 0 "t[a-z][a-z]" ""))))
  (is (= (update-regex (->Pattern 0 1 down 3 0 "dog" "") (list (->Pattern 0 0 across 3 0 "[a-z][a-z][a-z]" "")
                                                            (->Pattern 1 0 across 3 0 "[a-z][a-z][a-z]" "")
                                                            (->Pattern 2 0 across 3 0 "[a-z][a-z][a-z]" "")))
         (list (->Pattern 0 0 across 3 0 "[a-z]d[a-z]" "")
               (->Pattern 1 0 across 3 0 "[a-z]o[a-z]" "")
               (->Pattern 2 0 across 3 0 "[a-z]g[a-z]" ""))))
  (is (= (update-regex (->Pattern 0 2 down 3 0 "dog" "") (list (->Pattern 0 1 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                            (->Pattern 1 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                            (->Pattern 2 0 across 3 0 "[a-z][a-z][a-z]" "")))
         (list (->Pattern 0 1 across 4 0 "[a-z]d[a-z][a-z]" "")
               (->Pattern 1 0 across 4 0 "[a-z][a-z]o[a-z]" "")
               (->Pattern 2 0 across 3 0 "[a-z][a-z]g" "")))))

;; (deftest test-patterns-into-grid
;;   (is (= (patterns-into-grid (list (->Pattern 0 0 across 2 1 "a[a-z]" "")
;;                                    (->Pattern 1 0 across 2 0 "n[a-z]" "")
;;                                    (->Pattern 0 0 down 2 0 "[a-z][a-z]" "")
;;                                    (->Pattern 0 1 down 2 0 "dn" "")) ["##"
;;                                                                       "##"]) ["a d"
;;                                                                               "n n"])))

;; (deftest test-determine-best-word
;;   (is (= (determine-best-word
;;           (list (->Pattern 0 0 down 3 6 "[a-z][a-z][a-z]")
;;                 (->Pattern 0 1 down 3 6 "[a-z][a-z][a-z]")
;;                 (->Pattern 0 2 down 3 6 "[a-z][a-z][a-z]")
;;                 (->Pattern 0 0 across 3 6 "[a-z][a-z][a-z]")
;;                 (->Pattern 1 0 across 3 6 "[a-z][a-z][a-z]")
;;                 (->Pattern 2 0 across 3 6 "[a-z][a-z][a-z]"))
;;           (->Pattern 2 0 across 3 0 "dog")
;;           '("dad" "too" "fog")
;;           {:3 '("dad" "and" "dup" "too" "fog" "dog")}) "dad")))
