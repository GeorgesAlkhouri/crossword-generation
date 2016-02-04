(ns crossword.wordlist)
(require  '[clojure.java.io :as io])

(defn read-wordlist
  "Reads the provided words list into a sequence."
  []
  (let [path (-> "knuth_words_all_lower_sorted" io/resource io/file)]
    (with-open [rdr (io/reader path)]
      (doall (line-seq rdr)))))                             ;; doall needed to realize (not lazy) all lines in buffer

(defn hash-wordlist
  "Puts every word in a map with the proper word length as key."
  [wordlist]
  (reduce (fn [r w]
            (let [key (-> (count w) str keyword)]
              (if (nil? (key r))
                (assoc r key [w])
                (let [old-r (key r)
                      new-r (conj old-r w)]
                  (assoc r key new-r))))) {} wordlist))

(defn words-with-length
  ""
  [length wordlist]
  (-> (str length) keyword wordlist))