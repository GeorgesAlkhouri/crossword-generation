(ns crossword.pattern)

(defrecord Pattern [x y direction length freedom regex word])

(def ^:const across true)
(def ^:const down false)

(defn get-columns
  "Get a list of columns from the grid."
  [grid]
  (let [count (count grid)]
    (map #(apply str %) (partition count (apply interleave grid)))))

;;source http://stackoverflow.com/questions/3262195/compact-clojure-code-for-regular-expression-matches-and-their-position-in-string
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
  (apply concat (map-indexed (fn                            ;; concat because of nested map, otherwise it would create nested list.
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

(defn create-patterns
  "Determines all patterns from a fresh grid. Only execudes one time after start."
  [grid]
  (let [horizontal grid
        vertical (get-columns grid)
        patterns-h (map #(re-pos #"_+" %) horizontal)
        patterns-v (map #(re-pos #"_+" %) vertical)
        mapped-p-h (map-patterns patterns-h across)
        mapped-p-v (map-patterns patterns-v down)
        patterns (map (fn [p]
                        (let [regex (pattern->regex p grid)]
                          (assoc p :regex (str regex)))) (concat mapped-p-h mapped-p-v))]
    (set (filter #(not= (:length %) 1) patterns))))