(ns crossword.core
  (:gen-class))
(require '[clojure.test]
         '[clojure.string :as string]
         '[clojail.core :as clojail]
         '[crossword.wordlist :as wordlist]
         '[crossword.pattern :as pattern]
         '[crossword.grid :as grid])

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

(defn get-affected-patterns
  "Gets intersecting patterns of given pattern."
  [patterns pattern]
  (let [a (pattern->rect pattern)]
    (filter #(let [b (pattern->rect %)]
              (and
                (not (or (> (:left b) (:right a))
                         (< (:right b) (:left a))
                         (> (:top b) (:bottom a))
                         (< (:bottom b) (:top a))))
                (not= a b))) patterns)))

(defn match-words
  ""
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
  (let [temp-regex-pattern (string/replace (:regex pattern) #"\[a\-z\]" "*")]
    (map #(let [temp-regex (string/replace (:regex %) #"\[a\-z\]" "*")
                updated-regex (if (= (:direction pattern) pattern/across)
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
  (reduce (fn [p r]
            (pattern-replace p r)) patterns replacements))

(defmulti fill-strategy
          ""
          (fn [data] (:fill-strategy data)))

(defn update-freedom
  "Updates freedom of patterns with a given fill strategy."
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
        most-constr (apply min-key #(:freedom %) rated)]    ;;dirty! change it, so that you can abstract from most-constraint to any pick strat.
    most-constr))

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
      (-> (repeatedly 10 #(rand-nth matches)) distinct))))  ;; TODO, delete word after choosing it.

(defmethod pick-strategy :dynamic
  [data]
  (let [random-pick (pick-strategy {:pick-strategy :random
                                    :regex         (:regex data)
                                    :words         (:words data)})
        pattern (:pattern data)
        patterns (:patterns data)
        wordlist (:wordlist data)
        words (map #(let [p' (propagate-pattern most-constrained pattern patterns % wordlist)
                          freedom (reduce *' (map :freedom (:p p')))]
                     {:w % :f freedom}) random-pick)]
    (map :w (reverse (sort-by :f words)))))

(defn pick-words
  "Picks possible words with a picking strategy."
  [strategy pattern patterns wordlist]
  (let [words (cond
                (= strategy dynamic) (pick-strategy {:pick-strategy :dynamic
                                                     :regex         (-> (:regex pattern) re-pattern)
                                                     :words         (words-with-length (:length pattern) wordlist)
                                                     :wordlist      wordlist
                                                     :pattern       pattern
                                                     :patterns      patterns})
                :else (pick-strategy {:pick-strategy (keyword strategy)
                                      :regex         (-> (:regex pattern) re-pattern)
                                      :words         (words-with-length (:length pattern) wordlist)}))]
    words))

(defn arc-consistency?
  [fill pick patterns wordlist]
  (if (or (empty? patterns)
          (nil? patterns))
    true                                                    ;; last case every pattern has been solved
    (if (or (= fill most-constrained)
            (= pick dynamic))
      ;;      (every? #(> (:freedom %) 0) patterns)
      true
      (every? true? (map #(contains-word (re-pattern (:regex %)) (words-with-length (:length %) wordlist)) patterns)))))

(defn solve [patterns wordlist fill pick]
  (letfn [(solve-rec [patterns wordlist back-tracks solved]
            (if (empty? patterns)
              [true back-tracks solved]
              (let [next-pattern (fill-pattern fill patterns wordlist) ;; 1. Delete tuple from list with lowest freedo
                    possible-words (pick-words pick next-pattern patterns wordlist) ;; 2. Instantiate pattern to grid
                    ]
                (let [[s? b s-p] (reduce (fn [interim-result word]
                                           (if-not (first interim-result)
                                             (let [updated (propagate-pattern fill next-pattern patterns word wordlist) ;; 3. Propagate instantiation to affected patterns
                                                   arc-consistent (arc-consistency? fill pick (:p updated) wordlist) ;; 4. Check for arc-consistency
                                                   ]
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

(defn seed
  ""
  [patterns fill pick wordlist]
  (let [max-length (-> (apply max-key #(:length %) patterns) :length)
        filtered-p (filter #(= max-length (:length %)) patterns)
        random-max-p (-> filtered-p shuffle first)
        random-w (-> (words-with-length (:length random-max-p) wordlist) shuffle first) ;; FIXME: Check for nil return from wordlist
        updated-p (propagate-pattern fill random-max-p patterns random-w wordlist)
        arc-consistent? (arc-consistency? fill pick (:p updated-p) wordlist)]
    (if-not arc-consistent?
      (throw (Exception. "Not arc-consistent seeding."))
      (let [final-p (assoc random-max-p :regex random-w :word random-w :freedom 0)]
        (println final-p)
        (update-patterns patterns (cons final-p (:p updated-p)))))))

(defn solve-with-timeout
  [ms patterns wordlist fill pick]
  (try
    (clojail/thunk-timeout (fn [] (solve patterns wordlist fill pick)) ms)
    (catch Exception e [false 0 patterns])))

(defn -main
  [fill pick grid]
  (let []
    (let [wordlist (-> (wordlist/read-wordlist) wordlist/hash-wordlist)
          g' (grid/format-grid grid)
          patterns (pattern/create-patterns g')
          seeded-p (seed patterns fill pick wordlist)
          res (solve seeded-p wordlist fill pick)]
      (println "Backtacks: " (second res))
      (if-not (nil? res)
        (doseq [line (grid/patterns-into-grid (last res) g')]
          (println line))
        (println "Not solvable.")))))

(defn -bench
  [fill pick grid]
  (let [wordlist (-> (wordlist/read-wordlist) wordlist/hash-wordlist)
        g (grid/format-grid grid)
        p (pattern/create-patterns g)]
    (for [i (range 0 40)]
      (let [seeded-p (seed p fill pick wordlist)
            [s? b p] (time (solve-with-timeout 60000 seeded-p wordlist fill pick))]
        (if s?
          (println "Bt:" b)
          (println p))))))

(defn print-grid
  [patterns grid]
  (doseq [line (grid/patterns-into-grid patterns grid)]
    (println line)))

(defn -hypo
  []
  (let [wordlist (-> (wordlist/read-wordlist) wordlist/hash-wordlist)
        grid (grid/format-grid grid/grid-5x5)
        patterns (pattern/create-patterns grid)
        ;;seeded-p (seed patterns most-constrained dynamic wordlist)
        ]
    ;;    (println seeded-p)
    (for [i (range 0 50)
          pick [first-n random]]
      (let [[s? b p] (time (solve-with-timeout 60000 patterns wordlist most-constrained pick))]
        [pick b]))))
