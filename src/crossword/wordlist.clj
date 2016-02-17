(ns crossword.wordlist)
(require  '[clojure.java.io :as io])

(defn read-wordlist
  "Reads the provided word list into a sequence."
  [name]
  (let [path (-> name io/resource io/file)]
    (with-open [rdr (io/reader path)]
      (doall (line-seq rdr)))))                             ;; doall needed to realize (not lazy) all lines in buffer

(defn map-wordlist
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
  "Returns all words with the specified length from a word list."
  [length wordlist]
  (-> (str length) keyword wordlist))