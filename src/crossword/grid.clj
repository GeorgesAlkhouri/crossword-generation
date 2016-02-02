(ns crossword.grid)
(require '[crossword.pattern :as pattern])

(def ^:const grid-5x5 ["# # _ _ _"
                       "# _ _ _ _"
                       "_ _ _ _ _"
                       "_ _ _ _ #"
                       "_ _ _ # #"])

(def ^:const grid-9x9 ["# # # _ _ _ # # #"
                       "# # _ _ _ _ _ # #"
                       "# _ _ _ _ _ _ _ #"
                       "_ _ _ _ # _ _ _ _"
                       "_ _ _ # # # _ _ _"
                       "_ _ _ _ # _ _ _ _"
                       "# _ _ _ _ _ _ _ #"
                       "# # _ _ _ _ _ # #"
                       "# # # _ _ _ # # #"])

(def ^:const grid-13x13 ["_ _ _ _ # _ _ _ # _ _ _ _"
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

(def ^:const grid-15x15 ["_ _ _ _ _ _ # _ _ _ _ _ _ _ _"
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

(defn format-grid
  "Removes space chars from the grid strings."
  [grid]
  (map #(.replace % " " "") grid))

(defn patterns-into-grid
  [patterns grid]
  (let [removed (remove #(= pattern/down (:direction %)) patterns)
        sorted (sort-by :x removed)
        parted (partition-by :x sorted)]
    (map #(let [res (reduce (fn [a b]
                              (let [pos (:y b)
                                    s (if (not= (count (:word b)) 0)
                                        (str (subs a 0 pos) (:word b) (subs a (+ pos (:length b))))
                                        (apply str (seq (char-array (:length b) \#))))]
                                s)) (nth grid (:x (first %))) %)]
           (apply str (interpose " " res))) parted)))