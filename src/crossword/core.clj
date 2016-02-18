(ns crossword.core
  (:gen-class))
(require '[clojure.test]
         '[clojure.string :as string]
         '[clojail.core :as clojail]
         '[crossword.wordlist :as wordlist]
         '[crossword.pattern :as pattern]
         '[crossword.grid :as grid])

(def ^:const wordlist-name "knuth_words_all_lower_sorted")

(def ^:const most-constrained "most-constrained")
(def ^:const ratio "ratio")
(def ^:const first-n "first-n")
(def ^:const random "random")
(def ^:const dynamic "dynamic")

(defn pattern->rect
  "Maps a pattern to its rectangle representation."
  [pattern]
  (let [left (:y pattern)
        top (:x pattern)
        right (if (= (:direction pattern) pattern/across)
                (- (+ left (:length pattern)) 1)
                left)
        bottom (if (= (:direction pattern) pattern/down)
                 (- (+ top (:length pattern)) 1)
                 top)]
    {:left left :top top :right right :bottom bottom}))

(defn intersecting-patterns
  "Returns all patterns, which intersecting with the given pattern."
  [pattern patterns]
  (let [a (pattern->rect pattern)]
    (filter #(let [b (pattern->rect %)]
              (and
                (not (or (> (:left b) (:right a))
                         (< (:right b) (:left a))
                         (> (:top b) (:bottom a))
                         (< (:bottom b) (:top a))))
                (not= a b))) patterns)))

(defn match-words
  "Returns a collation with all words matching giving regex."
  [regex words]
  (filter #(re-matches regex %) words))

(defn contains-word?
  "Returns true if word collection contains word, othwise false."
  [regex words]
  (> (count (match-words regex words)) 0))

(defn replace-string
  "Inserts c in string s at index i."
  [s c i]
  (str (subs s 0 i) c (subs s (+ i 1))))

(defn update-regex
  "Updates the changed regex from a pattern to all affected patterns."
  [pattern affected-patterns]
  (let [temp-regex-pattern (string/replace (:regex pattern) #"\[a\-z\]" "*")]
    (map #(let [temp-regex (string/replace (:regex %) #"\[a\-z\]" "*")
                updated-regex (if (= (:direction pattern) pattern/across)
                                (replace-string temp-regex (str (nth temp-regex-pattern (- (:y %) (:y pattern)))) (- (:x pattern) (:x %)))
                                (replace-string temp-regex (str (nth temp-regex-pattern (- (:x %) (:x pattern)))) (- (:y pattern) (:y %))))]
           (assoc % :regex (string/replace updated-regex #"\*" "[a-z]"))) affected-patterns)))

(defn replace-pattern
  "Replace a pattern in a collection of patterns."
  [replace patterns]
  (map #(if (pattern/pattern-equal? replace %)
         replace
         %) patterns))

(defn replace-patterns
  "Replace a collection of patterns in another collection."
  [replacements patterns]
  (reduce (fn [p r]
            (replace-pattern r p)) patterns replacements))

;; TODO: Doc pick and fill strategies

(defmulti fill-strategy
          "Multi method for the different fill strategies."
          (fn [data] (:fill-strategy data)))

(defmethod fill-strategy :most-constrained
  [data]
  (count (match-words (:regex data) (:words data))))

(defmethod fill-strategy :ratio
  [data]
  (let [temp (string/replace (str (:regex data)) #"\[a\-z\]" "*")]
    (/ (count (re-seq #"\*" temp))
       (count temp))))

(defmethod fill-strategy :est-constrained
  [data]
  (throw (Exception. "Not implemented, yet.")))

(defmulti pick-strategy
          "Multi method for the different pick strategies."
          (fn [data] (:pick-strategy data)))

(defmethod pick-strategy :first-n
  [data]
  (take 10 (match-words (:regex data) (:words data))))

(defmethod pick-strategy :random
  [data]
  (let [matches (match-words (:regex data) (:words data))]
    (-> (repeatedly 10 #(rand-nth matches)) distinct)))     ;; TODO: delete word after choosing it.

(declare propagate-pattern)

(defmethod pick-strategy :dynamic
  [data]
  (let [random-pick (pick-strategy {:pick-strategy :random
                                    :regex         (:regex data)
                                    :words         (:words data)})
        words (map #(let [p' (propagate-pattern most-constrained (:pattern data) (:patterns data) % (:wordlist data))
                          freedom (reduce *' (map :freedom (:p p')))]
                     {:w % :f freedom}) random-pick)
        words' (filter #(> (:f %) 0) words)]
    (map :w (reverse (sort-by :f words')))))

(defmethod pick-strategy :prob
  [data]
  (throw (Exception. "Not implemented, yet.")))

(defn update-freedom
  "Updates freedom value of patterns with a given fill strategy."
  [fill patterns wordlist]
  (map (fn [p]
         (let [regex (re-pattern (:regex p))
               words (wordlist/words-with-length (:length p) wordlist)]
           (assoc p :freedom
                    (fill-strategy {:fill-strategy (keyword fill) :regex regex :words words})))) patterns))

(defn propagate-pattern
  "Propagates a pattern to the grid. Updates freedom of intersecting patterns with a fill strategy."
  [fill pattern patterns word wordlist]
  (if (nil? word)
    nil
    (let [intersect-patterns (intersecting-patterns pattern patterns)
          p (assoc pattern :regex word)
          updated-patterns (update-regex p intersect-patterns)
          updated-patterns' (update-freedom fill updated-patterns wordlist)]
      {:p updated-patterns' :w word})))

(defn fill-pattern
  "Parameter: patterns - All patterns to be solved.
  Fill a pattern for a given fill strategy. Chooses the most constrained pattern (Smallest freedom value).
  If fill strategy is 'most-constrained', checks for arc-consistency early to avoid unnecessary calculations."
  [fill patterns wordlist]
  (let [rated (update-freedom fill patterns wordlist)
        most-constr (apply min-key #(:freedom %) rated)]
    (if (= fill most-constrained)
      (if (= (:freedom most-constr) 0)
        nil
        most-constr)
      most-constr)))

(defn pick-words
  "Picks possible words with a picking strategy."
  [strategy pattern patterns wordlist]
  (let [words (cond
                (= strategy dynamic) (pick-strategy {:pick-strategy :dynamic
                                                     :regex         (-> (:regex pattern) re-pattern)
                                                     :words         (wordlist/words-with-length (:length pattern) wordlist)
                                                     :wordlist      wordlist
                                                     :pattern       pattern
                                                     :patterns      patterns})
                :else (pick-strategy {:pick-strategy (keyword strategy)
                                      :regex         (-> (:regex pattern) re-pattern)
                                      :words         (wordlist/words-with-length (:length pattern) wordlist)}))]
    words))

(defn arc-consistency?
  "Checks all patterns for solvability."
  [fill pick patterns wordlist]
  (if (empty? patterns)
    true                                                    ;; last case every pattern has been solved
    (if (or (= fill most-constrained)
            (= pick dynamic))
      true
      (every? true? (map #(contains-word? (re-pattern (:regex %)) (wordlist/words-with-length (:length %) wordlist)) patterns)))))

(defn seed-patterns
  "Determines a pattern with max length and fills it with a word of that length."
  [patterns fill pick wordlist]
  (let [max-length (-> (apply max-key #(:length %) patterns) :length)
        filtered-p (filter #(= max-length (:length %)) patterns)
        random-max-p (-> filtered-p shuffle first)
        random-w (-> (wordlist/words-with-length (:length random-max-p) wordlist) shuffle first) ;; TODO: Check for nil return from wordlist
        updated-p (propagate-pattern fill random-max-p patterns random-w wordlist)
        arc-consistent? (arc-consistency? fill pick (:p updated-p) wordlist)]
    (if-not arc-consistent?
      ;;(throw (Exception. "Not arc-consistent seeding."))
      (seed-patterns patterns fill pick wordlist)
      (let [final-p (assoc random-max-p :regex random-w :word random-w :freedom 0)]
        (replace-patterns (cons final-p (:p updated-p)) patterns)))))

(defn generate [patterns wordlist fill pick]
  "Generates a crossword grid. If all given patterns are solved the grid is generated.
  If not, the next pattern will be chosen and an appropriate word will be filled in the pattern.
  If no word could match the next pattern a backtrack step will be applied
  and another word will be inserted in the previous pattern."
  (letfn [(generate-rec [patterns wordlist back-tracks solved]
            (if (empty? patterns)
              [true back-tracks solved]
              (if-let [next-pattern (fill-pattern fill patterns wordlist)] ;; 1. Delete tuple from list with lowest freedom
                (if-let [possible-words (pick-words pick next-pattern patterns wordlist)] ;; 2. Instantiate pattern to grid]
                  (let [[s? b s-p] (reduce (fn [interim-result word]
                                             (if-not (first interim-result)
                                               (let [updated (propagate-pattern fill next-pattern patterns word wordlist) ;; 3. Propagate instantiation to affected patterns
                                                     wordlist' (assoc wordlist
                                                                 (-> (count (:w updated)) str keyword)
                                                                 (remove #(= (:w updated) %) (wordlist/words-with-length (count (:w updated)) wordlist)))
                                                     arc-consistent (arc-consistency? fill pick (:p updated) wordlist') ;; 4. Check for arc-consistency
                                                     ]
                                                 (if arc-consistent
                                                   (let [w (:w updated)
                                                         ps (:p updated)
                                                         u (replace-patterns ps patterns)]
                                                     (generate-rec
                                                       (remove #(pattern/pattern-equal? next-pattern %) u)
                                                       wordlist'
                                                       (second interim-result)
                                                       (cons (assoc next-pattern :word w) solved)))
                                                   [false back-tracks solved]))
                                               interim-result)) [false back-tracks solved] possible-words)
                        b' (if-not s? (inc b) b)]
                    [s? b' s-p])
                  [false back-tracks solved])
                [false back-tracks solved])))]
    (generate-rec patterns wordlist 0 #{})))

(defn generate-with-timeout
  "Cancels crossword generation after a period of time and throws an exception."
  [ms patterns wordlist fill pick]
  (try
    (clojail/thunk-timeout (fn [] (generate patterns wordlist fill pick)) ms)
    (catch Exception _
      [false 0 patterns])))

;; Convenient functions

(defn generate-crossword
  "Seeds initial grid and generates a filled crossword grid."
  [fill pick grid]
  (let [wordlist (-> (wordlist/read-wordlist wordlist-name) wordlist/map-wordlist)
        g (grid/format-grid grid)
        patterns (pattern/create-patterns g)
        seeded-p (seed-patterns patterns fill pick wordlist)
        [s? b p] (generate seeded-p wordlist fill pick)]
    (println "Backtacks: " b)
    (if s?
      (doseq [line (grid/patterns-into-grid p g)]
        (println line))
      (println "Could not be generated."))))

(defn benchmark-generation
  [fill pick grid]
  (let [wordlist (-> (wordlist/read-wordlist wordlist-name) wordlist/map-wordlist)
        g (grid/format-grid grid)
        patterns (pattern/create-patterns g)]
    (for [_ (range 0 40)]
      (let [seeded-p (seed-patterns patterns fill pick wordlist)
            [s? b _] (time (generate-with-timeout 60000 seeded-p wordlist fill pick))]
        (if s?
          b
          -1)))))

(defn benchmark-all
  []
  (for [fill (list most-constrained ratio)
        pick (list first-n random dynamic)
        grid (list grid/grid-5x5 grid/grid-9x9 grid/grid-13x13 grid/grid-15x15)]
    (str "F: " fill " P: " pick " G: " (count grid) " B: " (clojure.string/join ", " (benchmark-generation fill pick grid)))))

(defn -main
  [& args]
  (println (benchmark-all)))