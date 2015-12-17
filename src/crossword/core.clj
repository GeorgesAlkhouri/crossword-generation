(ns crossword.core
  (:gen-class))
(require '[clojure.test])
(require '[criterium.core :as criterium])
(require '[clojure.java.io :as io])
(require '[clojure.string :as string])
(require '[clojail.core :as clojail])

(defrecord Pattern [x y direction length freedom regex word])

(def ^:const across true)
(def ^:const down false)

(def ^:const most-constrained "most-constrained")
(def ^:const ratio "ratio")
(def ^:const first-n "first-n")
(def ^:const random "random")
(def ^:const dynamic "dynamic")

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

(defn contains-word
  [regex words]
  (if (some #(not (nil? %)) (map #(re-matches regex %) words))
    true
    false))

(defn words-with-length
  [length wordlist]
  (-> (str length) keyword wordlist))

(defn replace-string
  "Insert c in string s at index i."
  [s c i]
  (str (subs s 0 i) c (subs s (+ i 1))))

(defn update-regex
  [pattern affected-patterns]
  (let [temp-regex-pattern (string/replace (:regex pattern)  #"\[a\-z\]" "*") ]
    (map #(let [temp-regex (string/replace (:regex %)  #"\[a\-z\]" "*")
                updated-regex (if (= (:direction pattern) across)
                                (replace-string temp-regex (str (nth temp-regex-pattern (- (:y %) (:y pattern)))) (- (:x pattern) (:x %)))
                                (replace-string temp-regex (str (nth temp-regex-pattern (- (:x %) (:x pattern)))) (- (:y pattern) (:y %))))]
            (assoc % :regex (string/replace updated-regex #"\*" "[a-z]"))) affected-patterns)))

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
  (let [removed (remove #(= down (:direction %)) patterns)
        sorted (sort-by :x removed)
        parted (partition-by :x sorted)]
    (map #(let [res (reduce (fn [a b]
                              (let [pos (:y b)
                                    s (str (subs a 0 pos) (:word b) (subs a (+ pos (:length b))))]
                                s)) (nth grid (:x (first %)))  %)]
            (apply str (interpose " " res))) parted)))

(defmulti fill-strategy
  ""
  (fn [data] (:fill-strategy data)))

(defn update-freedom
  [fill patterns wordlist]
  (map (fn [p]
         (let [regex (re-pattern (:regex p))
               words (-> (str (:length p)) keyword wordlist)]
           (assoc p
             :freedom (fill-strategy {:fill-strategy (keyword fill) :regex regex :words words})))) patterns))

(defn propagate-pattern
  [fill pattern patterns word wordlist]
  (if (nil? word)
    nil
    (let [affected-patterns (get-affected-patterns patterns pattern)
          p (assoc pattern :regex word)
          updated-patterns (update-regex p affected-patterns)
          p' (update-freedom fill updated-patterns wordlist)]
      {:p p' :w word})))

;;;;;;;Fill Strategies;;;;;;;

(defmethod fill-strategy :most-constrained
  [data]
  (count (match-words (:regex data) (:words data))))

(defmethod fill-strategy :ratio
  [data]
  (let [temp (string/replace (str (:regex data)) #"\[a\-z\]" "*")]
    (/ (count (re-seq #"\*" temp))
       (count temp))))

(defmethod fill-strategy :est-constrained
  [data])

(defn fill-pattern
  "Fill a pattern for a given fill/delete strategy f. Chooses the most constrained pattern. "
  [fill patterns wordlist]
  (let [rated (update-freedom fill patterns wordlist)
        most-constr (apply min-key #(:freedom %) rated)] ;;dirty! change it, so that you can abstract from most-constraint to any pick strat.
    most-constr))

;;;;;;;Pick Strategies;;;;;;;

(defmulti pick-strategy
  ""
  (fn [data] (:pick-strategy data)))

(defmethod pick-strategy :first-n
  [data]
  (take 10 (match-words (:regex data) (:words data))))

(defmethod pick-strategy :random
  [data]
  (let [matches (match-words (:regex data) (:words data))]
    (when (not (empty? matches))
      (-> (repeatedly 10 #(rand-nth matches)) distinct))));; TODO, delete word after choosing it.

(defmethod pick-strategy :dynamic
  [data]
  (let [random-pick (pick-strategy {:pick-strategy :random
                                    :regex (:regex data)
                                    :words (:words data)})
        pattern (:pattern data)
        patterns (:patterns data)
        wordlist (:wordlist data)
        words (map #(let [p' (propagate-pattern most-constrained pattern patterns % wordlist)
                          freedom (reduce *' (map :freedom (:p p')))]
                      {:w % :f freedom}) random-pick)]
    (map :w (reverse (sort-by :f words)))))

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
    (set (filter #(not= (:length %) 1) patterns))))

(defn pick-words
  "Picks possible words with a picking strategy." 
  [strategy pattern patterns wordlist]
  (let [words (cond
               (= strategy dynamic) (pick-strategy {:pick-strategy :dynamic
                                                    :regex (-> (:regex pattern) re-pattern)
                                                    :words (words-with-length (:length pattern) wordlist)
                                                    :wordlist wordlist
                                                    :pattern pattern
                                                    :patterns patterns})
               :else (pick-strategy {:pick-strategy (keyword strategy)
                                     :regex (-> (:regex pattern) re-pattern)
                                     :words (words-with-length (:length pattern) wordlist)}))]
    words))

(defn arc-consistency?
  [fill pick patterns wordlist]
  (if (or (empty? patterns)
          (nil? patterns))
    true ;; last case every pattern has been solved
    (if (or (= fill most-constrained)
            (= pick dynamic)
            ;;(= pick probe)
            )
      ;;      (every? #(> (:freedom %) 0) patterns)
      true
      (every? true? (map #(contains-word (re-pattern (:regex %)) (words-with-length (:length %) wordlist)) patterns)))))

;;(def bench-bt (atom {:bt []}))

(defn solve [patterns wordlist fill pick]
  (letfn [(solve-rec [patterns wordlist back-tracks solved]
            (if (empty? patterns)
              [true back-tracks solved]
              (let [next-pattern (fill-pattern fill patterns wordlist) ;; 1. Delete tuple from list with lowest freedo,
                    possible-words (pick-words pick next-pattern patterns wordlist) ;; 2. Instantiate pattern to grid
                    ]
                (let [[s? b s-p] (reduce (fn [interim-result word]                                           
                                           (if-not (first interim-result)
                                             (let [updated (propagate-pattern fill next-pattern patterns word wordlist) ;; 3.-4. Propagate instantiation to affected patterns and check for arc-consistent
                                                   arc-consistent (arc-consistency? fill pick (:p updated) wordlist)]
                                               (if arc-consistent
                                                 (let [w (:w updated)
                                                       ps (:p updated)
                                                       u (update-patterns patterns ps)]
                                                   (solve-rec
                                                    (remove #(pattern-equal? next-pattern %) u)
                                                    (assoc wordlist
                                                      (-> (count w) str keyword)
                                                      (remove #(= w %) (words-with-length (count w) wordlist)))
                                                    (second interim-result)
                                                    (cons (assoc next-pattern :word w) solved)))
                                                 [false back-tracks solved]))
                                             interim-result)) [false back-tracks solved] possible-words)
                      b' (if-not s? (inc b) b)]
                  [s? b' s-p]))))]
    (solve-rec patterns wordlist 0 #{})))

;; ["_ _"
;;  "_ _"]

(def grid-5x5  ["# # _ _ _"
                "# _ _ _ _"
                "_ _ _ _ _"
                "_ _ _ _ #"
                "_ _ _ # #"])

;; (def test-grid-easy ["_ _ #"
;;                      "_ _ _"
;;                      "_ _ _"])

(def grid-9x9 ["# # # _ _ _ # # #"
               "# # _ _ _ _ _ # #"
               "# _ _ _ _ _ _ _ #"
               "_ _ _ _ # _ _ _ _"
               "_ _ _ # # # _ _ _"
               "_ _ _ _ # _ _ _ _"
               "# _ _ _ _ _ _ _ #"
               "# # _ _ _ _ _ # #"
               "# # # _ _ _ # # #"])

(def grid-13x13 ["_ _ _ _ # _ _ _ # _ _ _ _"
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

(def grid-15x15 ["_ _ _ _ _ _ # _ _ _ _ _ _ _ _"
                 "# _ # _ # _ # _ # _ # _ # _ #"
                 "_ _ _ _ _ _ _ _ _ _ # _ _ _ _"
                 "# _ # _ # _ # _ # _ # _ # _ #"
                 "_ _ _ _ _ _ _ _ # _ _ _ _ _ _"
                 "# # # _ # _ # _ # _ # # # _ #"
                 "_ _ _ _ # # # _ _ _ _ _ _ _ _"
                 "# _ # _ # _ # _ # _ # _ # _ #"
                 "_ _ _ _ _ _ _ _ # # # _ _ _ _"
                 "# _ # # # _ # _ # _ # _ # # #"
                 "_ _ _ _ _ _ # _ _ _ _ _ _ _ _"
                 "# _ # _ # _ # _ # _ # _ # _ #"
                 "_ _ _ _ # _ _ _ _ _ _ _ _ _ _"
                 "# _ # _ # _ # _ # _ # _ # _ #"
                 "_ _ _ _ _ _ _ _ # _ _ _ _ _ _"])

(defn seed
  ""
  [patterns fill pick wordlist]
  (let [max-length (-> (apply max-key #(:length %) patterns) :length)
        filtered-p (filter #(= max-length (:length %)) patterns)
        random-max-p (-> filtered-p shuffle first)
        random-w (-> (words-with-length (:length random-max-p) wordlist) shuffle first);; FIXME: Check for nil return from wordlist
        updated-p (propagate-pattern fill random-max-p patterns random-w wordlist)
        arc-consistent? (arc-consistency? fill pick (:p updated-p) wordlist)]
    (if-not arc-consistent?
      (throw (Exception. "Not arc-consistent seeding."))
      (let [final-p (assoc random-max-p :regex random-w :word random-w :freedom 0)]
        (update-patterns patterns (cons final-p (:p updated-p)))))))

(defn -main
  [fill pick grid]
  (let []
    (let [wordlist (-> (read-wordlist) hash-wordlist)
          g' (format-grid grid)
          patterns (create-patterns g')
          seeded-p (seed patterns fill pick wordlist)
          res (solve seeded-p wordlist fill pick)]
      (println "Backtacks: " (second res))
      (if-not (nil? res)
              (doseq [line (patterns-into-grid (last res) g')]
          (println line))
        (println "Not solvable.")))))

(defn solve-with-timeout
  [ms patterns wordlist fill pick]
  (try
    (clojail/thunk-timeout (fn [] (solve patterns wordlist fill pick)) ms)
    (catch Exception e [false 0 "Timeout: No result."])))

(defn -bench
  [fill pick grid]
  (let [wordlist (-> (read-wordlist) hash-wordlist)
        g (format-grid grid)
        p (create-patterns g)
        seeded-p (seed p fill pick wordlist)
        [s? b p] (time (solve-with-timeout 10000 seeded-p wordlist fill pick))]
    (if s?
      (println "Bt:" b)
      (println p))))

;; (defn -bench
;;   [& args]
;;   (let [wordlist (-> (read-wordlist) hash-wordlist)]
;;     (for [fill '(most-constrained)
;;           pick '(first-n)
;;           grid (list grid-5x5)
;;           :let [fresh (swap! bench-bt assoc :bt [])
;;                 g (format-grid grid)
;;                 p (create-patterns g)
;;                 report (with-out-str (criterium/bench (solve-with-timeout 10000 p wordlist fill pick)))
;;                 avg (if (= 0 (count (:bt @bench-bt))) 0 (/ (reduce + (:bt @bench-bt)) (count (:bt @bench-bt))))]]
;;       {:g (count grid) :f fill :p pick :r report :bt avg})))
